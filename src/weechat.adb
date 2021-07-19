--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with Ada.Calendar.Time_Zones;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Maps;
with Ada.Unchecked_Conversion;

package body WeeChat is

   use Interfaces.C.Strings;

   package SM renames Ada.Strings.Maps;

   function Trim (Value : String) return String is
     (SF.Trim (Value, SM.Null_Set, SM.To_Set (L1.LF)));

   function Split
     (Value     : String;
      Separator : String := " ";
      Maximum   : Natural := 0) return String_List
   is
      Lines : constant String := Trim (Value);

      Index : Positive := Lines'First;

      Auto_Count : constant Positive := SF.Count (Lines, Separator) + 1;
      Count : constant Positive :=
        (if Maximum > 0 then Positive'Min (Maximum, Auto_Count) else Auto_Count);
   begin
      return Result : String_List (1 .. Count) do
         for I in Result'First .. Result'Last - 1 loop
            declare
               Next_Index : constant Positive := SF.Index (Lines, Separator, Index);
            begin
               Result (I) := SU.To_Unbounded_String (Lines (Index .. Next_Index - 1));
               Index := Next_Index + Separator'Length;
            end;
         end loop;
         Result (Result'Last) := SU.To_Unbounded_String (Lines (Index .. Lines'Last));
      end return;
   end Split;

   function Join (List : String_List; Separator : String) return String is
      Result : SU.Unbounded_String;
   begin
      for Index in List'First .. List'Last - 1 loop
         SU.Append (Result, List (Index));
         SU.Append (Result, Separator);
      end loop;
      SU.Append (Result, List (List'Last));
      return +Result;
   end Join;

   procedure Send_Message
     (Plugin : Plugin_Ptr;
      Server, Recipient, Message : String)
   is
      Signal_Message : chars_ptr := New_String
        (Server & ";" & Recipient & ";priority_high,user_message;;" & Message);

      function Convert is new Ada.Unchecked_Conversion (chars_ptr, Void_Ptr);
   begin
      Send_Signal (Plugin, "irc_input_send", String_Type, Convert (Signal_Message));
      Free (Signal_Message);
   exception
      when others =>
         Free (Signal_Message);
         raise;
   end Send_Message;

   function Get_Nick (Host : String) return String is
      Sender : constant String := SU.To_String (Split (Host, Separator => "!", Maximum => 2) (1));
   begin
      return Sender (Sender'First + 1 .. Sender'Last);
   end Get_Nick;

   function Name (Plugin : Plugin_Ptr) return String is
     (Value (Plugin.Plugin_Get_Name (Plugin)));

   procedure Print (Plugin : Plugin_Ptr; Prefix : Prefix_Kind; Message : String) is
      package CH renames Ada.Characters.Handling;

      Prefix_String  : constant chars_ptr := Plugin.Prefix (CH.To_Lower (Prefix'Image) & L1.NUL);
      Message_String : constant C_String  := Value (Prefix_String) & Message & L1.NUL;
   begin
      Plugin.Printf_Date_Tags
        (System.Null_Address, 0, Null_Ptr, Message_String);
   end Print;

   procedure Print (Plugin : Plugin_Ptr; Prefix : String; Message : String) is
   begin
      Plugin.Printf_Date_Tags
        (System.Null_Address, 0, Null_Ptr, Prefix & L1.HT & Message & L1.NUL);
   end Print;

   procedure Print (Plugin : Plugin_Ptr; Message : String) is
   begin
      Print (Plugin, " ", Message);
   end Print;

   procedure Log (Plugin : Plugin_Ptr; Message : String) is
   begin
      Plugin.Log_Printf (Message & L1.NUL);
   end Log;

   procedure Print_Error (Plugin : Plugin_Ptr; Value : Ada.Exceptions.Exception_Occurrence) is
   begin
      Print (Plugin, Error, Ada.Exceptions.Exception_Information (Value));
   end Print_Error;

   function Command_Callback
     (Callback : On_Command_Callback;
      Data     : Data_Ptr;
      Buffer   : Buffer_Ptr;
      Argc     : int;
      Argv     : access chars_ptr;
      Argv_EOL : access chars_ptr) return Callback_Result
   is
      Raw_Arguments : chars_ptr_array (1 .. size_t (Argc))
        with Address => (if Argv /= null then Argv.all'Address else System.Null_Address),
             Import  => True;

      function Get_Argument (Index : Positive) return String
        with Pre => Index <= Integer (Argc) or else
          raise Constraint_Error with "Index" & Index'Image & " not in 1 .." & Argc'Image;

      function Get_Argument (Index : Positive) return String is
        (Value (Raw_Arguments (size_t (Index))));

      Arguments : String_List (1 .. Raw_Arguments'Length);
   begin
      for Index in Arguments'Range loop
         Arguments (Index) := SU.To_Unbounded_String (Get_Argument (Index));
      end loop;

      return Callback (Data.Plugin, Buffer, Arguments);
   exception
      when E : others =>
         Print_Error (Data.Plugin, E);
         return Error;
   end Command_Callback;

   function Command_Run_Callback
     (Callback : On_Command_Run_Callback;
      Data     : Data_Ptr;
      Buffer   : Buffer_Ptr;
      Command  : chars_ptr) return Callback_Result is
   begin
      return Callback (Data.Plugin, Buffer, Value (Command));
   exception
      when E : others =>
         Print_Error (Data.Plugin, E);
         return Error;
   end Command_Run_Callback;

   function Completion_Callback
     (Callback   : On_Completion_Callback;
      Data       : Data_Ptr;
      Item       : Interfaces.C.Strings.chars_ptr;
      Buffer     : Buffer_Ptr;
      Completion : Completion_Ptr) return Callback_Result is
   begin
      return Callback (Data.Plugin, Value (Item), Buffer, Completion);
   exception
      when E : others =>
         Print_Error (Data.Plugin, E);
         return Error;
   end Completion_Callback;

   function Modifier_Callback
     (Callback      : On_Modifier_Callback;
      Data          : Data_Ptr;
      Modifier      : chars_ptr;
      Modifier_Data : chars_ptr;
      Text          : chars_ptr) return chars_ptr is
   begin
      return New_String
        (Callback (Data.Plugin, Value (Modifier), Value (Modifier_Data), Value (Text)));
   exception
      when E : others =>
         Print_Error (Data.Plugin, E);
         return Text;
   end Modifier_Callback;

   function Print_Callback
     (Callback   : On_Print_Callback;
      Data       : Data_Ptr;
      Buffer     : Buffer_Ptr;
      Date       : Time_T;
      Tagc       : int;
      Tagv       : access chars_ptr;
      Displayed  : int;
      Highlight  : int;
      Prefix     : chars_ptr;
      Message    : chars_ptr) return Callback_Result
   is
      Raw_Tags : chars_ptr_array (1 .. size_t (Tagc))
        with Address => (if Tagv /= null then Tagv.all'Address else System.Null_Address),
             Import  => True;

      function Get_Tag (Index : Positive) return String
        with Pre => Index <= Integer (Tagc) or else
          raise Constraint_Error with "Index" & Index'Image & " not in 1 .." & Tagc'Image;

      function Get_Tag (Index : Positive) return String is
        (Value (Raw_Tags (size_t (Index))));

      use Ada.Calendar;

      Time_Epoch  : constant Time := Time_Of (Year => 1970, Month => 1, Day => 1);
      Time_Offset : constant Duration := Duration (Time_Zones.UTC_Time_Offset (Time_Epoch)) * 60;

      Tags : String_List (1 .. Raw_Tags'Length);
   begin
      for Index in Tags'Range loop
         Tags (Index) := SU.To_Unbounded_String (Get_Tag (Index));
      end loop;

      return Callback (Data.Plugin, Buffer, Time_Epoch + Time_Offset + Duration (Date),
        Tags, Displayed = 1, Highlight = 1, Value (Prefix), Value (Message));
   exception
      when E : others =>
         Print_Error (Data.Plugin, E);
         return Error;
   end Print_Callback;

   function Signal_Callback
     (Callback    : On_Signal_Callback;
      Data        : Data_Ptr;
      Signal      : chars_ptr;
      Type_Data   : chars_ptr;
      Signal_Data : Void_Ptr) return Callback_Result
   is
      Type_String : constant String := Value (Type_Data);

      Kind : Data_Kind;
   begin
      if Type_String = "string" then
         Kind := String_Type;
      elsif Type_String = "int" then
         Kind := Int_Type;
      elsif Type_String = "pointer" then
         Kind := Pointer_Type;
      else
         raise Constraint_Error with "Invalid signal type";
      end if;

      return Callback (Data.Plugin, Value (Signal), Kind, Signal_Data);
   exception
      when E : others =>
         Print_Error (Data.Plugin, E);
         return Error;
   end Signal_Callback;

   function Timer_Callback
     (Callback        : On_Timer_Callback;
      Data            : Data_Ptr;
      Remaining_Calls : int) return Callback_Result is
   begin
      return Callback (Data.Plugin, Integer (Remaining_Calls));
   exception
      when E : others =>
         Print_Error (Data.Plugin, E);
         return Error;
   end Timer_Callback;

   -----------------------------------------------------------------------------

   procedure Add_Command
     (Plugin                : Plugin_Ptr;
      Command               : String;
      Description           : String;
      Arguments             : String;
      Arguments_Description : String;
      Completion            : String;
      Callback              : On_Command_Callback)
   is
      Result : Hook_Ptr;
   begin
      Result := Plugin.Hook_Command
        (Plugin,
         Command & L1.NUL,
         Description & L1.NUL,
         Arguments & L1.NUL,
         Arguments_Description & L1.NUL,
         Completion & L1.NUL,
         Command_Callback'Access,
         Callback,
         new Data'(Plugin => Plugin));
      pragma Assert (Result /= null);
   end Add_Command;

   procedure On_Command_Run
     (Plugin   : Plugin_Ptr;
      Command  : String;
      Callback : On_Command_Run_Callback)
   is
      Result : Hook_Ptr;
   begin
      Result := Plugin.Hook_Command_Run
        (Plugin,
         Command & L1.NUL,
         Command_Run_Callback'Access,
         Callback,
         new Data'(Plugin => Plugin));
      pragma Assert (Result /= null);
   end On_Command_Run;

   procedure On_Completion
     (Plugin      : Plugin_Ptr;
      Item        : String;
      Description : String;
      Callback    : On_Completion_Callback)
   is
      Result : Hook_Ptr;
   begin
      Result := Plugin.Hook_Completion
        (Plugin,
         Item & L1.NUL,
         Description & L1.NUL,
         Completion_Callback'Access,
         Callback,
         new Data'(Plugin => Plugin));
      pragma Assert (Result /= null);
   end On_Completion;

   procedure Add_Completion_Word
     (Plugin     : Plugin_Ptr;
      Completion : Completion_Ptr;
      Word       : String;
      Is_Nick    : Boolean             := False;
      Where      : Completion_Position := Any_Position) is
   begin
      Plugin.Hook_Completion_List_Add
        (Completion, Word & L1.NUL, (if Is_Nick then 1 else 0),
         (case Where is
            when Any_Position      => "sort",
            when Beginning_Of_List => "beginning",
            when End_Of_List       => "end") & L1.NUL);
   end Add_Completion_Word;

   function Run_Command
     (Plugin  : Plugin_Ptr;
      Buffer  : Buffer_Ptr;
      Message : String) return Boolean
   is
      Result : Callback_Result;
   begin
      Result := Plugin.Command (Plugin, Buffer, Message & L1.NUL);
      pragma Assert (Result /= Eat);
      return Result /= Error;
   end Run_Command;

   procedure Run_Command
     (Plugin  : Plugin_Ptr;
      Buffer  : Buffer_Ptr;
      Message : String) is
   begin
      if not Run_Command (Plugin, Buffer, Message) then
         raise Program_Error;
      end if;
   end Run_Command;

   procedure On_Modifier
     (Plugin   : Plugin_Ptr;
      Modifier : String;
      Callback : On_Modifier_Callback)
   is
      Result : Hook_Ptr;
   begin
      Result := Plugin.Hook_Modifier
        (Plugin,
         Modifier & L1.NUL,
         Modifier_Callback'Access,
         Callback,
         new Data'(Plugin => Plugin));
      pragma Assert (Result /= null);
   end On_Modifier;

   procedure On_Print
     (Plugin       : Plugin_Ptr;
      Buffer       : Buffer_Ptr;
      Tags         : String;
      Message      : String;
      Strip_Colors : Boolean;
      Callback     : On_Print_Callback)
   is
      Result : Hook_Ptr;
   begin
      Result := Plugin.Hook_Print
        (Plugin, Buffer, Tags & L1.NUL, Message & L1.NUL,
         (if Strip_Colors then 1 else 0), Print_Callback'Access,
         Callback,
         new Data'(Plugin => Plugin));
      pragma Assert (Result /= null);
   end On_Print;

   procedure On_Signal
     (Plugin   : Plugin_Ptr;
      Signal   : String;
      Callback : On_Signal_Callback)
   is
      Result : Hook_Ptr;
   begin
      Result := Plugin.Hook_Signal
        (Plugin, (if Signal'Length > 0 then Signal else "*") & L1.NUL,
         Signal_Callback'Access,
         Callback,
         new Data'(Plugin => Plugin));
      pragma Assert (Result /= null);
   end On_Signal;

   procedure Send_Signal
     (Plugin      : Plugin_Ptr;
      Signal      : String;
      Kind        : Data_Kind;
      Signal_Data : Void_Ptr)
   is
      package CH renames Ada.Characters.Handling;

      Data_Kind : constant String := CH.To_Lower (Kind'Image);

      Unused_Result : Callback_Result;
   begin
      Unused_Result := Plugin.Hook_Signal_Send
        (Signal & L1.NUL, Data_Kind (Data_Kind'First .. Data_Kind'Last - 5) & L1.NUL,
         Signal_Data);
   end Send_Signal;

   function Set_Timer
     (Plugin       : Plugin_Ptr;
      Interval     : Duration;
      Align_Second : Natural;
      Max_Calls    : Natural;
      Callback     : On_Timer_Callback) return Timer
   is
      Result : Hook_Ptr;
   begin
      Result := Plugin.Hook_Timer
        (Plugin, long (Interval * 1e3), int (Align_Second), int (Max_Calls),
         Timer_Callback'Access,
         Callback,
         new Data'(Plugin => Plugin));
      pragma Assert (Result /= null);
      return (Result => Result, Plugin => Plugin);
   end Set_Timer;

   procedure Cancel_Timer (Object : Timer) is
   begin
      Object.Plugin.Unhook (Object.Result);
   end Cancel_Timer;

   procedure Set_Title (Plugin : Plugin_Ptr; Title : String) is
   begin
      Plugin.Window_Set_Title (Title & L1.NUL);
   end Set_Title;

   function Get_Info (Plugin : Plugin_Ptr; Name, Arguments : String) return String is
      Args : chars_ptr := New_String (Arguments);
   begin
      return Result : constant String := Value (Plugin.Info_Get (Plugin, Name & L1.NUL, Args)) do
         Free (Args);
      end return;
   exception
      when others =>
         Free (Args);
         raise;
   end Get_Info;

   function Get_Info (Plugin : Plugin_Ptr; Name : String) return String is
     (Value (Plugin.Info_Get (Plugin, Name & L1.NUL, Null_Ptr)));

   -----------------------------------------------------------------------------

   function Reset (Object : Config_Option) return Option_Set is
     (Object.Plugin.Config_Option_Reset (Object.Pointer, 1));

   function Unset (Object : Config_Option) return Option_Unset is
     (Object.Plugin.Config_Option_Unset (Object.Pointer));

   function Set (Object : Config_Option; Value : String) return Option_Set is
     (Object.Plugin.Config_Option_Set (Object.Pointer, Value & L1.NUL, 1));

   function Set_Null (Object : Config_Option) return Option_Set is
     (Object.Plugin.Config_Option_Set_Null (Object.Pointer, 1));

   function Is_Null (Object : Config_Option) return Boolean is
     (Object.Plugin.Config_Option_Is_Null (Object.Pointer) = 1);

   function Is_Default_Null (Object : Config_Option) return Boolean is
     (Object.Plugin.Config_Option_Default_Is_Null (Object.Pointer) = 1);

   procedure Rename (Object : Config_Option; Name : String) is
   begin
      Object.Plugin.Config_Option_Rename (Object.Pointer, Name & L1.NUL);
   end Rename;

   function Kind (Object : Config_Option) return Option_Kind is
      Property_Value : constant String :=
        Value (Object.Plugin.Config_Option_Get_String (Object.Pointer, "type" & L1.NUL));
   begin
      if Property_Value = "boolean" then
         return Boolean_Type;
      elsif Property_Value = "integer" then
         return Integer_Type;
      elsif Property_Value = "string" then
         return String_Type;
      elsif Property_Value = "color" then
         return Color_Type;
      else
         raise Constraint_Error;
      end if;
   end Kind;

   function Value (Object : Config_Option) return Boolean is
     (Object.Plugin.Config_Boolean (Object.Pointer) = 1);

   function Value (Object : Config_Option) return Integer is
     (Integer (Object.Plugin.Config_Integer (Object.Pointer)));

   function Value (Object : Config_Option) return String is
     (Value (Object.Plugin.Config_String (Object.Pointer)));

   function Get_Config_Option (Plugin : Plugin_Ptr; Name : String) return Config_Option is
      Result : constant Config_Option_Ptr := Plugin.Config_Get (Name & L1.NUL);
   begin
      if Result = null then
         raise Constraint_Error with "Option " & Name & " does not exist";
      end if;
      return (Pointer => Result, Plugin => Plugin);
   end Get_Config_Option;

   -----------------------------------------------------------------------------

   function Plugin_Init
     (Object        : Plugin_Ptr;
      On_Initialize : Plugin_Callback) return Callback_Result is
   begin
      On_Initialize (Object);
      return OK;
   exception
      when E : others =>
         Print_Error (Object, E);
         return Error;
   end Plugin_Init;

   function Plugin_End
     (Object      : Plugin_Ptr;
      On_Finalize : Plugin_Callback) return Callback_Result is
   begin
      On_Finalize (Object);
      return OK;
   exception
      when E : others =>
         Print_Error (Object, E);
         return Error;
   end Plugin_End;

end WeeChat;
