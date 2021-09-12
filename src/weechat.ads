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

with System;

with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

private with Interfaces.C.Strings;

package WeeChat is
   pragma Elaborate_Body;

   type Plugin_Ptr is private;

   type Plugin_Callback is not null access procedure (Plugin : Plugin_Ptr);

   type Callback_Result is (Error, OK, Eat);

   function Plugin_Init
     (Object        : Plugin_Ptr;
      On_Initialize : Plugin_Callback) return Callback_Result;

   function Plugin_End
     (Object      : Plugin_Ptr;
      On_Finalize : Plugin_Callback) return Callback_Result;

   package L1 renames Ada.Characters.Latin_1;

   subtype C_String is String
     with Dynamic_Predicate => C_String (C_String'Last) = L1.NUL;

   Plugin_API_Version : constant String := "20200301-03" & L1.NUL;
   --  Generated for version 2.8, the version packaged in Ubuntu 20.04 LTS

   -----------------------------------------------------------------------------

   package SF renames Ada.Strings.Fixed;
   package SU renames Ada.Strings.Unbounded;

   function "+" (Value : String) return SU.Unbounded_String renames SU.To_Unbounded_String;
   function "+" (Value : SU.Unbounded_String) return String renames SU.To_String;

   type String_List is array (Positive range <>) of SU.Unbounded_String;

   function Split
     (Value     : String;
      Separator : String  := " ";
      Maximum   : Natural := 0) return String_List;

   function Join (List : String_List; Separator : String) return String;

   -----------------------------------------------------------------------------

   type Void_Ptr is new System.Address;

   Null_Void : constant Void_Ptr;

   type Data is record
      Plugin : Plugin_Ptr;
   end record;

   type Data_Ptr is access all Data;

   type Buffer_Ptr is private;

   Any_Buffer : constant Buffer_Ptr;

   type Completion_Ptr is private;

   type Hashtable_Ptr is private;

   type Timer is private;

   No_Timer : constant Timer;

   -----------------------------------------------------------------------------

   type Prefix_Kind is (Error, Network, Action, Join, Quit);

   type Data_Kind is (String_Type, Int_Type, Pointer_Type);

   type Completion_Position is (Any_Position, Beginning_Of_List, End_Of_List);

   type Line_Buffer_Kind is (Formatted, Free);

   type Notify_Level is (None, Low, Message, Private_Message, Highlight);

   type Line_Data (Kind : Line_Buffer_Kind) is record
      Buffer    : SU.Unbounded_String;
      Name      : SU.Unbounded_String;
      Message   : SU.Unbounded_String;
      Displayed : Boolean;
      case Kind is
         when Formatted =>
            Date         : Ada.Calendar.Time;
            Date_Printed : Ada.Calendar.Time;
            Date_Display : SU.Unbounded_String;
            Tags         : SU.Unbounded_String;
            Level        : Notify_Level;
            Highlight    : Boolean;
            Prefix       : SU.Unbounded_String;
         when Free =>
            Line_Number : Natural;
      end case;
   end record;

   -----------------------------------------------------------------------------

   type On_Modifier_Callback is not null access function
     (Plugin        : Plugin_Ptr;
      Modifier      : String;
      Modifier_Data : String;
      Text          : String) return String;

   type On_Command_Callback is not null access function
     (Plugin    : Plugin_Ptr;
      Buffer    : Buffer_Ptr;
      Arguments : String_List) return Callback_Result;

   type On_Command_Run_Callback is not null access function
     (Plugin  : Plugin_Ptr;
      Buffer  : Buffer_Ptr;
      Command : String) return Callback_Result;

   type On_Completion_Callback is not null access function
     (Plugin     : Plugin_Ptr;
      Item       : String;
      Buffer     : Buffer_Ptr;
      Completion : Completion_Ptr) return Callback_Result;

   type On_Line_Callback is not null access procedure
     (Plugin : Plugin_Ptr;
      Line   : in out Line_Data);

   type On_Print_Callback is not null access function
     (Plugin    : Plugin_Ptr;
      Buffer    : Buffer_Ptr;
      Date      : Ada.Calendar.Time;
      Tags      : String_List;
      Displayed : Boolean;
      Highlight : Boolean;
      Prefix    : String;
      Message   : String) return Callback_Result;

   type On_Signal_Callback is not null access function
     (Plugin      : Plugin_Ptr;
      Signal      : String;
      Kind        : Data_Kind;
      Signal_Data : Void_Ptr) return Callback_Result;

   type On_Timer_Callback is not null access function
     (Plugin          : Plugin_Ptr;
      Remaining_Calls : Integer) return Callback_Result;

   -----------------------------------------------------------------------------

   function Name (Plugin : Plugin_Ptr) return String;

   procedure Print (Plugin : Plugin_Ptr; Prefix : Prefix_Kind; Message : String);
   --  Print a message with the given prefix to the screen

   procedure Print (Plugin : Plugin_Ptr; Prefix : String; Message : String)
     with Pre => Prefix'Length > 0;
   --  Print a message with an arbitrary prefix to the screen

   procedure Print (Plugin : Plugin_Ptr; Message : String);
   --  Print a message without a prefix to the screen

   procedure Log (Plugin : Plugin_Ptr; Message : String);
   --  Log a message to ~/.weechat/weechat.log

   -----------------------------------------------------------------------------

   procedure Add_Command
     (Plugin                : Plugin_Ptr;
      Command               : String;
      Description           : String;
      Arguments             : String;
      Arguments_Description : String;
      Completion            : String;
      Callback              : On_Command_Callback);
   --  Add a new command and register a callback called when the command
   --  is run
   --
   --  The callback can be given a priority by prefixing the name
   --  with `priority|`.

   procedure On_Command_Run
     (Plugin   : Plugin_Ptr;
      Command  : String;
      Callback : On_Command_Run_Callback);
   --  Register a callback called when the given command is run
   --
   --  Wildcard `*` can be used in the command.
   --
   --  The callback can be given a priority by prefixing the name
   --  with `priority|`.

   procedure On_Completion
     (Plugin      : Plugin_Ptr;
      Item        : String;
      Description : String;
      Callback    : On_Completion_Callback);
   --  Register a callback for a completion
   --
   --  The item should be added to weechat.completion.default_template,
   --  otherwise nothing will happen.
   --
   --  In the callback, the procedure Add_Completion_Word must be called
   --  for each word that must be added to the completion.
   --
   --  The callback can be given a priority by prefixing the name
   --  with `priority|`.

   procedure Add_Completion_Word
     (Plugin     : Plugin_Ptr;
      Completion : Completion_Ptr;
      Word       : String;
      Is_Nick    : Boolean             := False;
      Where      : Completion_Position := Any_Position);
   --  Add a word to a completion

   procedure On_Modifier
     (Plugin   : Plugin_Ptr;
      Modifier : String;
      Callback : On_Modifier_Callback);
   --  Register a callback called to modify certain messages
   --
   --  The callback can be given a priority by prefixing the name
   --  with `priority|`.

   procedure On_Line
     (Plugin       : Plugin_Ptr;
      Buffer_Type  : Line_Buffer_Kind;
      Buffer_Name  : String;
      Tags         : String;
      Callback     : On_Line_Callback);
   --  Register a callback for when a line is to be printed in a buffer
   --
   --  Buffer_Name is a comma-separated list of masks, or empty , or `*`.
   --
   --  Wildcard `*` is allowed in tags. Tags must be separated by a
   --  a `,` ("or" operation) and can be combined with a `+` ("and"
   --  operation).

   procedure On_Print
     (Plugin       : Plugin_Ptr;
      Buffer       : Buffer_Ptr;
      Tags         : String;
      Message      : String;
      Strip_Colors : Boolean;
      Callback     : On_Print_Callback);
   --  Register a callback for when message is printed to the screen
   --
   --  Wildcard `*` is allowed in tags. Tags must be separated by a
   --  a `,` ("or" operation) and can be combined with a `+` ("and"
   --  operation).

   procedure On_Signal
     (Plugin   : Plugin_Ptr;
      Signal   : String;
      Callback : On_Signal_Callback);
   --  Register a callback for when a signal is sent
   --
   --  Wildcard `*` is allowed in name.
   --
   --  The callback can be given a priority by prefixing the name
   --  with `priority|`.

   function Run_Command
     (Plugin  : Plugin_Ptr;
      Buffer  : Buffer_Ptr;
      Message : String) return Boolean
   with Pre => Message'Length > 0;
   --  Execute a command and return true if successful, false otherwise

   procedure Run_Command
     (Plugin  : Plugin_Ptr;
      Buffer  : Buffer_Ptr;
      Message : String)
   with Pre => Message'Length > 0;
   --  Execute a command and raise Program_Error if unsuccessful

   procedure Send_Message
     (Plugin : Plugin_Ptr;
      Server, Recipient, Message : String);
   --  Send an IRC message to a user or channel
   --
   --  For example, to send a message to #ada on freenode:
   --
   --    Send_Message ("freenode", "#ada", "This message was sent by Ada");

   function Get_Nick (Host : String) return String;

   procedure Send_Signal
     (Plugin      : Plugin_Ptr;
      Signal      : String;
      Kind        : Data_Kind;
      Signal_Data : Void_Ptr);
   --  Send a signal

   function Set_Timer
     (Plugin       : Plugin_Ptr;
      Interval     : Duration;
      Align_Second : Natural;
      Max_Calls    : Natural;
      Callback     : On_Timer_Callback) return Timer;

   procedure Cancel_Timer (Object : Timer)
     with Pre => Object /= No_Timer;

   procedure Set_Title (Plugin : Plugin_Ptr; Title : String);

   function Get_Info (Plugin : Plugin_Ptr; Name, Arguments : String) return String;

   function Get_Info (Plugin : Plugin_Ptr; Name : String) return String;

   -----------------------------------------------------------------------------

   type Option_Set is (Error, Same_Value, Changed);

   type Option_Unset is (Error, No_Reset, Reset, Removed);

   type Option_Kind is (Boolean_Type, Integer_Type, String_Type, Color_Type);

   type Config_Option is private;

   function Reset (Object : Config_Option) return Option_Set;
   --  Reset the option to its default value

   function Unset (Object : Config_Option) return Option_Unset;
   --  Unset or reset the option

   function Set (Object : Config_Option; Value : String) return Option_Set;
   --  Set the option to the given value

   function Set_Null (Object : Config_Option) return Option_Set;
   --  Set the option to null (undefined)

   function Is_Null (Object : Config_Option) return Boolean;
   --  Return whether the current value is null

   function Is_Default_Null (Object : Config_Option) return Boolean;
   --  Return whether the default is null

   procedure Rename (Object : Config_Option; Name : String);
   --  Rename the option to the given name

   function Kind (Object : Config_Option) return Option_Kind;

   function Value (Object : Config_Option) return Boolean
     with Pre => Kind (Object) = Boolean_Type;

   function Value (Object : Config_Option) return Integer
     with Pre => Kind (Object) = Integer_Type;

   function Value (Object : Config_Option) return String
     with Pre => Kind (Object) = String_Type;

   function Get_Config_Option (Plugin : Plugin_Ptr; Name : String) return Config_Option;
   --  Return the option for the given name

private

   use Interfaces.C;

   Null_Void : constant Void_Ptr := Void_Ptr (System.Null_Address);

   type Pointer is limited null record;

   type Hook_Ptr is access all Pointer;

   type Buffer_Ptr is access all Pointer;

   Any_Buffer : constant Buffer_Ptr := null;

   type Completion_Ptr is access all Pointer;

   type Hashtable_Ptr is access all Pointer;

   type Timer is record
      Result : Hook_Ptr;
      Plugin : Plugin_Ptr;
   end record;

   type Config_Option_Ptr is access all Pointer;

   type Config_Option is record
      Pointer : Config_Option_Ptr;
      Plugin  : Plugin_Ptr;
   end record;

   for Callback_Result use
     (Error => -1,
      OK    => 0,
      Eat   => 1);
   for Callback_Result'Size use int'Size;

   for Option_Set use
     (Error      => 0,
      Same_Value => 1,
      Changed    => 2);
   for Option_Set'Size use int'Size;

   for Option_Unset use
     (Error    => -1,
      No_Reset => 0,
      Reset    => 1,
      Removed  => 2);
   for Option_Unset'Size use int'Size;

   -----------------------------------------------------------------------------

   type Long_Long_Int is range -(2**63) .. +(2**63 - 1);
   --  Based on C99 long long int

   type Unsigned_Long_Long is mod 2**64;
   --  Based on C99 unsigned long long int

   subtype Time_T is long;

   subtype Sa_Data_Array is Interfaces.C.char_array (0 .. 13);

   type Sockaddr is record
      Sa_Family : aliased unsigned_short;
      Sa_Data   : aliased Sa_Data_Array;
   end record
     with Convention => C_Pass_By_Copy;

   type Timeval is record
      Tv_Sec  : aliased long;
      Tv_Usec : aliased long;
   end record
     with Convention => C_Pass_By_Copy;

   -----------------------------------------------------------------------------

--   Weechat_Config_Read_Ok             : constant := 0;
--   Weechat_Config_Read_Memory_Error   : constant := -1;
--   Weechat_Config_Read_File_Not_Found : constant := -2;

--   Weechat_Config_Write_Ok           : constant := 0;
--   Weechat_Config_Write_Error        : constant := -1;
--   Weechat_Config_Write_Memory_Error : constant := -2;

--   Weechat_Config_Option_Null : aliased constant String := "null" & L1.NUL;

--   Weechat_Config_Option_Set_Option_Not_Found : constant := -1;

--   Weechat_List_Pos_Sort      : aliased constant String := "sort" & L1.NUL;
--   Weechat_List_Pos_Beginning : aliased constant String := "beginning" & L1.NUL;
--   Weechat_List_Pos_End       : aliased constant String := "end" & L1.NUL;

--   Weechat_Hashtable_Integer : aliased constant String := "integer" & L1.NUL;
   Weechat_Hashtable_String : aliased constant String := "string" & L1.NUL;
--   Weechat_Hashtable_Pointer : aliased constant String := "pointer" & L1.NUL;
--   Weechat_Hashtable_Buffer  : aliased constant String := "buffer" & L1.NUL;
--   Weechat_Hashtable_Time    : aliased constant String := "time" & L1.NUL;

--   Weechat_Hdata_Other         : constant := 0;
--   Weechat_Hdata_Char          : constant := 1;
--   Weechat_Hdata_Integer       : constant := 2;
--   Weechat_Hdata_Long          : constant := 3;
--   Weechat_Hdata_String        : constant := 4;
--   Weechat_Hdata_Pointer       : constant := 5;
--   Weechat_Hdata_Time          : constant := 6;
--   Weechat_Hdata_Hashtable     : constant := 7;
--   Weechat_Hdata_Shared_String : constant := 8;

--   Weechat_Hdata_List_Check_Pointers : constant := 1;

--   Weechat_Hotlist_Low       : aliased constant String := "0" & L1.NUL;
--   Weechat_Hotlist_Message   : aliased constant String := "1" & L1.NUL;
--   Weechat_Hotlist_Private   : aliased constant String := "2" & L1.NUL;
--   Weechat_Hotlist_Highlight : aliased constant String := "3" & L1.NUL;

--   Weechat_Hook_Process_Running : constant := -1;
--   Weechat_Hook_Process_Error   : constant := -2;
--   Weechat_Hook_Process_Child   : constant := -3;

--   Weechat_Hook_Connect_Ok                     : constant := 0;
--   Weechat_Hook_Connect_Address_Not_Found      : constant := 1;
--   Weechat_Hook_Connect_Ip_Address_Not_Found   : constant := 2;
--   Weechat_Hook_Connect_Connection_Refused     : constant := 3;
--   Weechat_Hook_Connect_Proxy_Error            : constant := 4;
--   Weechat_Hook_Connect_Local_Hostname_Error   : constant := 5;
--   Weechat_Hook_Connect_Gnutls_Init_Error      : constant := 6;
--   Weechat_Hook_Connect_Gnutls_Handshake_Error : constant := 7;
--   Weechat_Hook_Connect_Memory_Error           : constant := 8;
--   Weechat_Hook_Connect_Timeout                : constant := 9;
--   Weechat_Hook_Connect_Socket_Error           : constant := 10;

--   Weechat_Hook_Connect_Gnutls_Cb_Verify_Cert : constant := 0;
--   Weechat_Hook_Connect_Gnutls_Cb_Set_Cert    : constant := 1;

   type T_Weechat_Plugin is record
      Filename    : Interfaces.C.Strings.chars_ptr;
      Handle      : System.Address;
      Name        : Interfaces.C.Strings.chars_ptr;
      Description : Interfaces.C.Strings.chars_ptr;
      Author      : Interfaces.C.Strings.chars_ptr;
      Version     : Interfaces.C.Strings.chars_ptr;
      License     : Interfaces.C.Strings.chars_ptr;
      Charset     : Interfaces.C.Strings.chars_ptr;
      Priority    : aliased int;
      Initialized : aliased int;
      Debug       : aliased int;
      Upgrading   : aliased int;
      Variables   : Hashtable_Ptr;
      Prev_Plugin : access T_Weechat_Plugin;
      Next_Plugin : access T_Weechat_Plugin;

      --  Plugins
      Plugin_Get_Name : access function
        (Plugin : access T_Weechat_Plugin) return Interfaces.C.Strings.chars_ptr;

      --  Strings
      Charset_Set : access procedure
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr);
      Iconv_To_Internal : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Iconv_From_Internal : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Gettext : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Ngettext : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int) return Interfaces.C.Strings.chars_ptr;
      Strndup : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int) return Interfaces.C.Strings.chars_ptr;
      String_Tolower : access procedure (Arg1 : Interfaces.C.Strings.chars_ptr);
      String_Toupper : access procedure (Arg1 : Interfaces.C.Strings.chars_ptr);
      Strcasecmp     : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Strcasecmp_Range : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int) return int;
      Strncasecmp : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int) return int;
      Strncasecmp_Range : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int;
         Arg4 : int) return int;
      Strcmp_Ignore_Chars : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : int) return int;
      Strcasestr : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Strlen_Screen : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      String_Match  : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int) return int;
      String_Match_List  : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : access Interfaces.C.Strings.chars_ptr;
         Arg3 : int) return int;
      String_Replace : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Expand_Home : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Eval_Path_Home : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : System.Address;
         Arg3 : System.Address;
         Arg4 : System.Address) return Interfaces.C.Strings.chars_ptr;
      String_Remove_Quotes : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Strip : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int;
         Arg3 : int;
         Arg4 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Convert_Escaped_Chars : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Mask_To_Regex : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Regex_Flags : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int;
         Arg3 : access int) return Interfaces.C.Strings.chars_ptr;
      String_Regcomp : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int) return int;
      String_Has_Highlight : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      String_Has_Highlight_Regex : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      String_Replace_Regex : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : char;
         Arg5 : access function
           (Arg1 : System.Address;
            Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
         Arg6 : System.Address) return Interfaces.C.Strings.chars_ptr;
      String_Split : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : int;
         Arg5 : int;
         Arg6 : access int) return System.Address;
      String_Split_Shell : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : access int) return System.Address;
      String_Free_Split              : access procedure (Arg1 : System.Address);
      String_Build_With_Split_String : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Split_Command : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : char) return System.Address;
      String_Free_Split_Command : access procedure (Arg1 : System.Address);
      String_Format_Size        : access function
        (Arg1 : Unsigned_Long_Long) return Interfaces.C.Strings.chars_ptr;
      String_Remove_Color : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Base_Encode : access procedure
        (Arg1 : int;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int;
         Arg4 : Interfaces.C.Strings.chars_ptr);
      String_Base_Decode : access function
        (Arg1 : int;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr) return int;
      String_Hex_Dump : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int;
         Arg3 : int;
         Arg4 : Interfaces.C.Strings.chars_ptr;
         Arg5 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Is_Command_Char  : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      String_Input_For_Buffer : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      String_Eval_Expression : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : System.Address;
         Arg3 : System.Address;
         Arg4 : System.Address) return Interfaces.C.Strings.chars_ptr;
      String_Dyn_Alloc : access function (Arg1 : int) return System.Address;
      String_Dyn_Copy  : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      String_Dyn_Concat : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      String_Dyn_Free : access function
        (Arg1 : System.Address;
         Arg2 : int) return Interfaces.C.Strings.chars_ptr;

      --  UTF-8
      Utf8_Has_8bits : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Is_Valid  : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int;
         Arg3 : System.Address) return int;
      Utf8_Normalize : access procedure (Arg1 : Interfaces.C.Strings.chars_ptr; Arg2 : char);
      Utf8_Prev_Char : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Utf8_Next_Char : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Utf8_Char_Int  : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Char_Size : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Strlen    : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Strnlen   : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int) return int;
      Utf8_Strlen_Screen : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Charcmp       : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Charcasecmp : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Char_Size_Screen : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      Utf8_Add_Offset       : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int) return Interfaces.C.Strings.chars_ptr;
      Utf8_Real_Pos : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int) return int;
      Utf8_Pos : access function (Arg1 : Interfaces.C.Strings.chars_ptr; Arg2 : int) return int;
      Utf8_Strndup : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int) return Interfaces.C.Strings.chars_ptr;

      --  Crypto
      Crypto_Hash : access function
        (Arg1 : System.Address;
         Arg2 : int;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : System.Address;
         Arg5 : access int) return int;
      Crypto_Hash_Pbkdf2 : access function
        (Arg1 : System.Address;
         Arg2 : int;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : System.Address;
         Arg5 : int;
         Arg6 : int;
         Arg7 : System.Address;
         Arg8 : access int) return int;

      --  Directories
      Mkdir_Home : access function (Arg1 : Interfaces.C.Strings.chars_ptr; Arg2 : int) return int;
      Mkdir : access function (Arg1 : Interfaces.C.Strings.chars_ptr; Arg2 : int) return int;
      Mkdir_Parents : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int) return int;
      Exec_On_Files : access procedure
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : int;
         Arg3 : int;
         Arg4 : access procedure (Arg1 : System.Address; Arg2 : Interfaces.C.Strings.chars_ptr);
         Arg5 : System.Address);
      File_Get_Content : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;

      --  Util
      Util_Timeval_Cmp : access function (Arg1 : access Timeval; Arg2 : access Timeval) return int;
      Util_Timeval_Diff : access function
        (Arg1 : access Timeval;
         Arg2 : access Timeval) return Long_Long_Int;
      Util_Timeval_Add     : access procedure (Arg1 : access Timeval; Arg2 : Long_Long_Int);
      Util_Get_Time_String : access function
        (Arg1 : access Time_T) return Interfaces.C.Strings.chars_ptr;
      Util_Version_Number : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return int;

      --  Sorted lists
      List_New : access function return System.Address;
      List_Add : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : System.Address) return System.Address;
      List_Search : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      List_Search_Pos : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      List_Casesearch : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      List_Casesearch_Pos : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      List_Get        : access function (Arg1 : System.Address; Arg2 : int) return System.Address;
      List_Set : access procedure (Arg1 : System.Address; Arg2 : Interfaces.C.Strings.chars_ptr);
      List_Next       : access function (Arg1 : System.Address) return System.Address;
      List_Prev       : access function (Arg1 : System.Address) return System.Address;
      List_String : access function (Arg1 : System.Address) return Interfaces.C.Strings.chars_ptr;
      List_User_Data  : access function (Arg1 : System.Address) return System.Address;
      List_Size       : access function (Arg1 : System.Address) return int;
      List_Remove     : access procedure (Arg1 : System.Address; Arg2 : System.Address);
      List_Remove_All : access procedure (Arg1 : System.Address);
      List_Free       : access procedure (Arg1 : System.Address);

      --  Array lists
      Arraylist_New : access function
        (Arg1 : int;
         Arg2 : int;
         Arg3 : int;
         Arg4 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : System.Address) return int;
         Arg5 : System.Address;
         Arg6 : access procedure
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address);
         Arg7 : System.Address) return System.Address;
      Arraylist_Size   : access function (Arg1 : System.Address) return int;
      Arraylist_Get    : access function (Arg1 : System.Address; Arg2 : int) return System.Address;
      Arraylist_Search : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : access int;
         Arg4 : access int) return System.Address;
      Arraylist_Insert : access function
        (Arg1 : System.Address;
         Arg2 : int;
         Arg3 : System.Address) return int;
      Arraylist_Add    : access function (Arg1 : System.Address; Arg2 : System.Address) return int;
      Arraylist_Remove : access function (Arg1 : System.Address; Arg2 : int) return int;
      Arraylist_Clear  : access function (Arg1 : System.Address) return int;
      Arraylist_Free   : access procedure (Arg1 : System.Address);

      --  Hashtables
      Hashtable_New : access function
        (Size                 : int;
         Type_Keys            : C_String;
         Type_Values          : C_String;
         Callback_Hash_Key    : access function
           (Table : Hashtable_Ptr;
            Key   : System.Address) return Unsigned_Long_Long;
         Callback_Key_Compare : access function
           (Table : Hashtable_Ptr;
            Key_1 : System.Address;
            Key_2 : System.Address) return int)
         return Hashtable_Ptr;
      Hashtable_Set_With_Size : access function
        (Table      : Hashtable_Ptr;
         Key        : System.Address;
         Key_Size   : int;
         Value      : System.Address;
         Value_Size : int) return System.Address;
      Hashtable_Set : access function
        (Table  : Hashtable_Ptr;
         Key    : C_String;
         Value  : C_String) return System.Address;
      Hashtable_Get : access function
        (Table : Hashtable_Ptr;
         Key   : C_String) return Interfaces.C.Strings.chars_ptr;
      Hashtable_Has_Key : access function
        (Table : Hashtable_Ptr;
         Key   : System.Address) return int;
      Hashtable_Map : access procedure
        (Table         : Hashtable_Ptr;
         Callback      : access procedure
           (Data  : Data_Ptr;
            Table : Hashtable_Ptr;
            Key   : System.Address;
            Value : System.Address);
         Callback_Data : Data_Ptr);
      Hashtable_Map_String : access procedure
        (Table    : Hashtable_Ptr;
         Callback : access procedure
           (Data  : Data_Ptr;
            Table : Hashtable_Ptr;
            Key   : Interfaces.C.Strings.chars_ptr;
            Value : Interfaces.C.Strings.chars_ptr);
         Callback_Data : Data_Ptr);
      Hashtable_Dup         : access function (Table : Hashtable_Ptr) return Hashtable_Ptr;
      Hashtable_Get_Integer : access function
        (Table    : Hashtable_Ptr;
         Property : C_String) return int;
      Hashtable_Get_String : access function
        (Table    : Hashtable_Ptr;
         Property : C_String) return Interfaces.C.Strings.chars_ptr;
      Hashtable_Set_Pointer : access procedure
        (Table    : Hashtable_Ptr;
         Property : C_String;
         Pointer  : System.Address);
      Hashtable_Add_To_Infolist : access function
        (Table  : Hashtable_Ptr;
         Item   : System.Address;
         Prefix : C_String) return int;
      Hashtable_Add_From_Infolist : access function
        (Table  : Hashtable_Ptr;
         List   : System.Address;
         Prefix : C_String) return int;
      Hashtable_Remove     : access procedure (Table : Hashtable_Ptr; Key : System.Address);
      Hashtable_Remove_All : access procedure (Table : Hashtable_Ptr);
      Hashtable_Free       : access procedure (Table : Hashtable_Ptr);

      --  Configuration files
      Config_New : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address) return int;
         Arg4 : System.Address;
         Arg5 : System.Address) return System.Address;
      Config_New_Section : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int;
         Arg4 : int;
         Arg5 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : System.Address;
            Arg5 : Interfaces.C.Strings.chars_ptr;
            Arg6 : Interfaces.C.Strings.chars_ptr) return int;
         Arg6 : System.Address;
         Arg7 : System.Address;
         Arg8 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : Interfaces.C.Strings.chars_ptr) return int;
         Arg9  : System.Address;
         Arg10 : System.Address;
         Arg11 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : Interfaces.C.Strings.chars_ptr) return int;
         Arg12 : System.Address;
         Arg13 : System.Address;
         Arg14 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : System.Address;
            Arg5 : Interfaces.C.Strings.chars_ptr;
            Arg6 : Interfaces.C.Strings.chars_ptr) return int;
         Arg15 : System.Address;
         Arg16 : System.Address;
         Arg17 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : System.Address;
            Arg5 : System.Address) return int;
         Arg18 : System.Address;
         Arg19 : System.Address) return System.Address;
      Config_Search_Section : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Config_New_Option : access function
        (Arg1  : System.Address;
         Arg2  : System.Address;
         Arg3  : Interfaces.C.Strings.chars_ptr;
         Arg4  : Interfaces.C.Strings.chars_ptr;
         Arg5  : Interfaces.C.Strings.chars_ptr;
         Arg6  : Interfaces.C.Strings.chars_ptr;
         Arg7  : int;
         Arg8  : int;
         Arg9  : Interfaces.C.Strings.chars_ptr;
         Arg10 : Interfaces.C.Strings.chars_ptr;
         Arg11 : int;
         Arg12 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : Interfaces.C.Strings.chars_ptr) return int;
         Arg13 : System.Address;
         Arg14 : System.Address;
         Arg15 : access procedure
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address);
         Arg16 : System.Address;
         Arg17 : System.Address;
         Arg18 : access procedure
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address);
         Arg19 : System.Address;
         Arg20 : System.Address) return System.Address;
      Config_Search_Option : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Config_Search_Section_Option : access procedure
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : System.Address;
         Arg5 : System.Address);
      Config_Search_With_String : access procedure
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : System.Address;
         Arg3 : System.Address;
         Arg4 : System.Address;
         Arg5 : System.Address);
      Config_String_To_Boolean : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return int;
      Config_Option_Reset : access function
        (Option       : Config_Option_Ptr;
         Run_Callback : int) return Option_Set;
      Config_Option_Set   : access function
        (Option       : Config_Option_Ptr;
         Value        : C_String;
         Run_Callback : int) return Option_Set;
      Config_Option_Set_Null : access function
        (Option       : Config_Option_Ptr;
         Run_Callback : int) return Option_Set;
      Config_Option_Unset    : access function
        (Option : Config_Option_Ptr) return Option_Unset;
      Config_Option_Rename   : access procedure
        (Option : Config_Option_Ptr;
         Name   : C_String);
      Config_Option_Get_String : access function
        (Option   : Config_Option_Ptr;
         Property : C_String) return Interfaces.C.Strings.chars_ptr;
      Config_Option_Get_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Config_Option_Is_Null         : access function (Option : Config_Option_Ptr) return int;
      Config_Option_Default_Is_Null : access function (Option : Config_Option_Ptr) return int;
      Config_Boolean                : access function (Option : Config_Option_Ptr) return int;
      Config_Boolean_Default        : access function (Arg1 : System.Address) return int;
      Config_Integer                : access function (Option : Config_Option_Ptr) return int;
      Config_Integer_Default        : access function (Arg1 : System.Address) return int;
      Config_String                 : access function
        (Option : Config_Option_Ptr) return Interfaces.C.Strings.chars_ptr;
      Config_String_Default : access function
        (Arg1 : System.Address) return Interfaces.C.Strings.chars_ptr;
      Config_Color : access function (Arg1 : System.Address) return Interfaces.C.Strings.chars_ptr;
      Config_Color_Default : access function
        (Arg1 : System.Address) return Interfaces.C.Strings.chars_ptr;
      Config_Write_Option : access function
        (Arg1 : System.Address;
         Arg2 : System.Address) return int;
      Config_Write_Line : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr  -- , ...
         ) return int;
      Config_Write                : access function (Arg1 : System.Address) return int;
      Config_Read                 : access function (Arg1 : System.Address) return int;
      Config_Reload               : access function (Arg1 : System.Address) return int;
      Config_Option_Free          : access procedure (Arg1 : System.Address);
      Config_Section_Free_Options : access procedure (Arg1 : System.Address);
      Config_Section_Free         : access procedure (Arg1 : System.Address);
      Config_Free                 : access procedure (Arg1 : System.Address);
      Config_Get : access function (Option_Name : C_String) return Config_Option_Ptr;
      Config_Get_Plugin           : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Config_Is_Set_Plugin : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Config_Set_Plugin : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr) return int;
      Config_Set_Desc_Plugin : access procedure
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr);
      Config_Unset_Plugin : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;

      --  Key bindings
      Key_Bind : access function
        (Context : Interfaces.C.Strings.chars_ptr;
         Keys    : Hashtable_Ptr) return int;
      Key_Unbind : access function
        (Context : Interfaces.C.Strings.chars_ptr;
         Key     : Interfaces.C.Strings.chars_ptr) return int;

      --  Display
      Prefix : access function
        (Prefix : C_String) return Interfaces.C.Strings.chars_ptr;
      Color : access function
        (Name : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Printf_Date_Tags : access procedure
        (Buffer  : System.Address;
         Date    : Time_T;
         Tags    : Interfaces.C.Strings.chars_ptr;
         Message : C_String);
      Printf_Y : access procedure
        (Buffer  : System.Address;
         Y       : int;
         Message : Interfaces.C.Strings.chars_ptr);
      Log_Printf : access procedure (Message : C_String);

      --  Hooks
      Hook_Command : access function
        (Plugin           : access T_Weechat_Plugin;
         Command          : C_String;
         Description      : C_String;
         Args             : C_String;
         Args_Description : C_String;
         Completion       : C_String;
         Callback         : access function
           (Callback : On_Command_Callback;
            Data     : Data_Ptr;
            Buffer   : Buffer_Ptr;
            Argc     : int;
            Argv     : access Interfaces.C.Strings.chars_ptr;
            Argv_EOL : access Interfaces.C.Strings.chars_ptr) return Callback_Result;
         Callback_Pointer : On_Command_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Command_Run : access function
        (Plugin   : access T_Weechat_Plugin;
         Command  : C_String;
         Callback : access function
           (Callback : On_Command_Run_Callback;
            Data     : Data_Ptr;
            Buffer   : Buffer_Ptr;
            Command  : Interfaces.C.Strings.chars_ptr) return Callback_Result;
         Callback_Pointer : On_Command_Run_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Timer : access function
        (Plugin       : access T_Weechat_Plugin;
         Interval     : long;
         Align_Second : int;
         Max_Calls    : int;
         Callback     : access function
           (Callback        : On_Timer_Callback;
            Data            : Data_Ptr;
            Remaining_Calls : int) return Callback_Result;
         Callback_Pointer : On_Timer_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_FD : access function
        (Plugin         : access T_Weechat_Plugin;
         FD             : int;
         Flag_Read      : int;
         Flag_Write     : int;
         Flag_Exception : int;
         Callback       : access function
           (Pointer : System.Address;
            Data    : Data_Ptr;
            Fd      : int) return int;
         Callback_Pointer : System.Address;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Process : access function
        (Plugin   : access T_Weechat_Plugin;
         Command  : Interfaces.C.Strings.chars_ptr;
         Timeout  : int;
         Callback : access function
           (Pointer      : System.Address;
            Data         : Data_Ptr;
            Command      : Interfaces.C.Strings.chars_ptr;
            Return_Code  : int;
            Standard_Out : Interfaces.C.Strings.chars_ptr;
            Standard_Err : Interfaces.C.Strings.chars_ptr) return int;
         Callback_Pointer : System.Address;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Process_Hashtable : access function
        (Plugin   : access T_Weechat_Plugin;
         Command  : Interfaces.C.Strings.chars_ptr;
         Options  : Hashtable_Ptr;
         Timeout  : int;
         Callback : access function
           (Pointer      : System.Address;
            Data         : Data_Ptr;
            Command      : Interfaces.C.Strings.chars_ptr;
            Return_Code  : int;
            Standard_Out : Interfaces.C.Strings.chars_ptr;
            Standard_Err : Interfaces.C.Strings.chars_ptr) return int;
         Callback_Pointer : System.Address;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Connect : access function
        (Plugin          : access T_Weechat_Plugin;
         Proxy           : Interfaces.C.Strings.chars_ptr;
         Address         : Interfaces.C.Strings.chars_ptr;
         Port            : int;
         Ipv6            : int;
         Retry           : int;
         Tls_Session     : System.Address;
         Tls_Callback    : System.Address;
         Tls_Dh_Key_Size : int;
         Tls_Priorities  : Interfaces.C.Strings.chars_ptr;
         Local_Hostname  : Interfaces.C.Strings.chars_ptr;
         Callback        : access function
           (Pointer    : System.Address;
            Data       : Data_Ptr;
            Status     : int;
            Tls_Rc     : int;
            Sock       : int;
            Error      : Interfaces.C.Strings.chars_ptr;
            Ip_Address : Interfaces.C.Strings.chars_ptr) return int;
         Callback_Pointer : System.Address;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Line : access function
        (Plugin       : access T_Weechat_Plugin;
         Buffer_Type  : C_String;
         Buffer_Name  : C_String;
         Tags         : C_String;
         Callback     : access function
           (Callback : On_Line_Callback;
            Data     : Data_Ptr;
            Line     : Hashtable_Ptr) return Hashtable_Ptr;
         Callback_Pointer : On_Line_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Print : access function
        (Plugin       : access T_Weechat_Plugin;
         Buffer       : Buffer_Ptr;
         Tags         : C_String;
         Message      : C_String;
         Strip_Colors : int;
         Callback     : access function
           (Callback   : On_Print_Callback;
            Data       : Data_Ptr;
            Buffer     : Buffer_Ptr;
            Date       : Time_T;
            Tagc       : int;
            Tagv       : access Interfaces.C.Strings.chars_ptr;
            Displayed  : int;
            Highlight  : int;
            Prefix     : Interfaces.C.Strings.chars_ptr;
            Message    : Interfaces.C.Strings.chars_ptr) return Callback_Result;
         Callback_Pointer : On_Print_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Signal : access function
        (Plugin   : access T_Weechat_Plugin;
         Signal   : C_String;
         Callback : access function
           (Callback    : On_Signal_Callback;
            Data        : Data_Ptr;
            Signal      : Interfaces.C.Strings.chars_ptr;
            Type_Data   : Interfaces.C.Strings.chars_ptr;
            Signal_Data : Void_Ptr) return Callback_Result;
         Callback_Pointer : On_Signal_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Signal_Send : access function
        (Signal      : C_String;
         Type_Data   : C_String;
         Signal_Data : Void_Ptr) return Callback_Result;
      Hook_Hsignal : access function
        (Plugin   : access T_Weechat_Plugin;
         Signal   : Interfaces.C.Strings.chars_ptr;
         Callback : access function
           (Pointer   : System.Address;
            Data      : Data_Ptr;
            Signal    : Interfaces.C.Strings.chars_ptr;
            Hashtable : Hashtable_Ptr) return int;
         Callback_Pointer : System.Address;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Hsignal_Send : access function
        (Signal    : Interfaces.C.Strings.chars_ptr;
         Hashtable : Hashtable_Ptr) return int;
      Hook_Config : access function
        (Plugin   : access T_Weechat_Plugin;
         Option   : Interfaces.C.Strings.chars_ptr;
         Callback : access function
           (Pointer : System.Address;
            Data    : System.Address;
            Option  : Interfaces.C.Strings.chars_ptr;
            Value   : Interfaces.C.Strings.chars_ptr) return int;
         Callback_Pointer : System.Address;
         Callback_Data    : System.Address) return Hook_Ptr;
      Hook_Completion : access function
        (Plugin      : access T_Weechat_Plugin;
         Item        : C_String;
         Description : C_String;
         Callback    : access function
           (Callback   : On_Completion_Callback;
            Data       : Data_Ptr;
            Item       : Interfaces.C.Strings.chars_ptr;
            Buffer     : Buffer_Ptr;
            Completion : Completion_Ptr) return Callback_Result;
         Callback_Pointer : On_Completion_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Completion_Get_String : access function
        (Completion : System.Address;
         Property   : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Hook_Completion_List_Add : access procedure
        (Completion      : Completion_Ptr;
         Word            : C_String;
         Nick_Completion : int;
         Where           : C_String);
      Hook_Modifier : access function
        (Plugin   : access T_Weechat_Plugin;
         Modifier : C_String;
         Callback : access function
           (Callback      : On_Modifier_Callback;
            Data          : Data_Ptr;
            Modifier      : Interfaces.C.Strings.chars_ptr;
            Modifier_Data : Interfaces.C.Strings.chars_ptr;
            Text          : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
         Callback_Pointer : On_Modifier_Callback;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Modifier_Exec : access function
        (Plugin   : access T_Weechat_Plugin;
         Modifier : Interfaces.C.Strings.chars_ptr;
         Data     : Interfaces.C.Strings.chars_ptr;
         String   : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Hook_Info : access function
        (Plugin           : access T_Weechat_Plugin;
         Name             : Interfaces.C.Strings.chars_ptr;
         Description      : Interfaces.C.Strings.chars_ptr;
         Args_Description : Interfaces.C.Strings.chars_ptr;
         Callback         : access function
           (Pointer   : System.Address;
            Data      : System.Address;
            Name      : Interfaces.C.Strings.chars_ptr;
            Arguments : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
         Callback_Pointer : System.Address;
         Callback_Data    : System.Address) return Hook_Ptr;
      Hook_Info_Hashtable : access function
        (Plugin             : access T_Weechat_Plugin;
         Name               : Interfaces.C.Strings.chars_ptr;
         Description        : Interfaces.C.Strings.chars_ptr;
         Args_Description   : Interfaces.C.Strings.chars_ptr;
         Output_Description : Interfaces.C.Strings.chars_ptr;
         Callback           : access function
           (Pointer   : System.Address;
            Data      : Data_Ptr;
            Name      : Interfaces.C.Strings.chars_ptr;
            Hashtable : System.Address) return Hashtable_Ptr;
         Callback_Pointer : System.Address;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Infolist : access function
        (Plugin              : access T_Weechat_Plugin;
         Name                : Interfaces.C.Strings.chars_ptr;
         Description         : Interfaces.C.Strings.chars_ptr;
         Pointer_Description : Interfaces.C.Strings.chars_ptr;
         Args_Description    : Interfaces.C.Strings.chars_ptr;
         Callback            : access function
           (Pointer        : System.Address;
            Data           : System.Address;
            Name           : Interfaces.C.Strings.chars_ptr;
            Object_Pointer : System.Address;
            Arguments      : Interfaces.C.Strings.chars_ptr) return System.Address;
         Callback_Pointer : System.Address;
         Callback_Data    : System.Address) return Hook_Ptr;
      Hook_Hdata : access function
        (Plugin      : access T_Weechat_Plugin;
         Name        : Interfaces.C.Strings.chars_ptr;
         Description : Interfaces.C.Strings.chars_ptr;
         Callback    : access function
           (Pointer : System.Address;
            Data    : System.Address;
            Name    : Interfaces.C.Strings.chars_ptr) return System.Address;
         Callback_Pointer : System.Address;
         Callback_Data    : System.Address) return Hook_Ptr;
      Hook_Focus : access function
        (Plugin   : access T_Weechat_Plugin;
         Area     : Interfaces.C.Strings.chars_ptr;
         Callback : access function
           (Pointer : System.Address;
            Data    : Data_Ptr;
            Info    : Hashtable_Ptr) return Hashtable_Ptr;
         Callback_Pointer : System.Address;
         Callback_Data    : Data_Ptr) return Hook_Ptr;
      Hook_Set : access procedure
        (Hook     : Hook_Ptr;
         Property : Interfaces.C.Strings.chars_ptr;
         Value    : Interfaces.C.Strings.chars_ptr);
      Unhook     : access procedure (Hook : Hook_Ptr);
      Unhook_All : access procedure
        (Plugin    : access T_Weechat_Plugin;
         Subplugin : Interfaces.C.Strings.chars_ptr);

      --  Buffers
      Buffer_New : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : Interfaces.C.Strings.chars_ptr) return int;
         Arg4 : System.Address;
         Arg5 : System.Address;
         Arg6 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address) return int;
         Arg7 : System.Address;
         Arg8 : System.Address) return System.Address;
      Buffer_Search : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Buffer_Search_Main : access function return System.Address;
      Buffer_Clear       : access procedure (Arg1 : System.Address);
      Buffer_Close       : access procedure (Arg1 : System.Address);
      Buffer_Merge       : access procedure (Arg1 : System.Address; Arg2 : System.Address);
      Buffer_Unmerge     : access procedure (Arg1 : System.Address; Arg2 : int);
      Buffer_Get_Integer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Buffer_Get_String : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Buffer_Get_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Buffer_Set : access procedure
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr);
      Buffer_Set_Pointer : access procedure
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : System.Address);
      Buffer_String_Replace_Local_Var : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Buffer_Match_List : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;

      --  Windows
      Window_Search_With_Buffer : access function (Buffer : System.Address) return System.Address;
      Window_Get_Integer        : access function
        (Window   : System.Address;
         Property : Interfaces.C.Strings.chars_ptr) return int;
      Window_Get_String : access function
        (Window   : System.Address;
         Property : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Window_Get_Pointer : access function
        (Window   : System.Address;
         Property : Interfaces.C.Strings.chars_ptr) return System.Address;
      Window_Set_Title : access procedure (Title : C_String);

      --  Nicklist
      Nicklist_Add_Group : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : Interfaces.C.Strings.chars_ptr;
         Arg5 : int) return System.Address;
      Nicklist_Search_Group : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Nicklist_Add_Nick : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : Interfaces.C.Strings.chars_ptr;
         Arg5 : Interfaces.C.Strings.chars_ptr;
         Arg6 : Interfaces.C.Strings.chars_ptr;
         Arg7 : int) return System.Address;
      Nicklist_Search_Nick : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Nicklist_Remove_Group  : access procedure (Arg1 : System.Address; Arg2 : System.Address);
      Nicklist_Remove_Nick   : access procedure (Arg1 : System.Address; Arg2 : System.Address);
      Nicklist_Remove_All    : access procedure (Arg1 : System.Address);
      Nicklist_Get_Next_Item : access procedure
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : System.Address);
      Nicklist_Group_Get_Integer : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return int;
      Nicklist_Group_Get_String : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Nicklist_Group_Get_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Nicklist_Group_Set : access procedure
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : Interfaces.C.Strings.chars_ptr);
      Nicklist_Nick_Get_Integer : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return int;
      Nicklist_Nick_Get_String : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Nicklist_Nick_Get_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Nicklist_Nick_Set : access procedure
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : Interfaces.C.Strings.chars_ptr);

      --  Bars
      Bar_Item_Search : access function
        (Arg1 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Bar_Item_New : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : System.Address;
            Arg5 : System.Address;
            Arg6 : System.Address) return Interfaces.C.Strings.chars_ptr;
         Arg4 : System.Address;
         Arg5 : System.Address) return System.Address;
      Bar_Item_Update : access procedure (Arg1 : Interfaces.C.Strings.chars_ptr);
      Bar_Item_Remove : access procedure (Arg1 : System.Address);
      Bar_Search : access function (Arg1 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Bar_New         : access function
        (Arg1  : Interfaces.C.Strings.chars_ptr;
         Arg2  : Interfaces.C.Strings.chars_ptr;
         Arg3  : Interfaces.C.Strings.chars_ptr;
         Arg4  : Interfaces.C.Strings.chars_ptr;
         Arg5  : Interfaces.C.Strings.chars_ptr;
         Arg6  : Interfaces.C.Strings.chars_ptr;
         Arg7  : Interfaces.C.Strings.chars_ptr;
         Arg8  : Interfaces.C.Strings.chars_ptr;
         Arg9  : Interfaces.C.Strings.chars_ptr;
         Arg10 : Interfaces.C.Strings.chars_ptr;
         Arg11 : Interfaces.C.Strings.chars_ptr;
         Arg12 : Interfaces.C.Strings.chars_ptr;
         Arg13 : Interfaces.C.Strings.chars_ptr;
         Arg14 : Interfaces.C.Strings.chars_ptr;
         Arg15 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Bar_Set : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr) return int;
      Bar_Update : access procedure (Arg1 : Interfaces.C.Strings.chars_ptr);
      Bar_Remove : access procedure (Arg1 : System.Address);

      --  Commands
      Command : access function
        (Plugin  : access T_Weechat_Plugin;
         Buffer  : Buffer_Ptr;
         Command : C_String) return Callback_Result;
      Command_Options : access function
        (Plugin  : access T_Weechat_Plugin;
         Buffer  : Buffer_Ptr;
         Command : C_String;
         Options : Hashtable_Ptr) return Callback_Result;

      --  Network
      Network_Pass_Proxy : access function
        (Proxy   : Interfaces.C.Strings.chars_ptr;
         Sock    : int;
         Address : Interfaces.C.Strings.chars_ptr;
         Port    : int) return int;
      Network_Connect_To : access function
        (Proxy   : Interfaces.C.Strings.chars_ptr;
         Address : access Sockaddr;
         Length  : unsigned) return int;

      --  Infos
      Info_Get : access function
        (Plugin    : access T_Weechat_Plugin;
         Info_Name : C_String;
         Arguments : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Info_Get_Hashtable : access function
        (Plugin    : access T_Weechat_Plugin;
         Info_Name : Interfaces.C.Strings.chars_ptr;
         Hashtable : Hashtable_Ptr) return Hashtable_Ptr;

      --  Infolists
      Infolist_New : access function (Arg1 : access T_Weechat_Plugin) return System.Address;
      Infolist_New_Item        : access function (Arg1 : System.Address) return System.Address;
      Infolist_New_Var_Integer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int) return System.Address;
      Infolist_New_Var_String : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Infolist_New_Var_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : System.Address) return System.Address;
      Infolist_New_Var_Buffer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : System.Address;
         Arg4 : int) return System.Address;
      Infolist_New_Var_Time : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Time_T) return System.Address;
      Infolist_Search_Var : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Infolist_Get : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : System.Address;
         Arg4 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Infolist_Next              : access function (Arg1 : System.Address) return int;
      Infolist_Prev              : access function (Arg1 : System.Address) return int;
      Infolist_Reset_Item_Cursor : access procedure (Arg1 : System.Address);
      Infolist_Fields            : access function
        (Arg1 : System.Address) return Interfaces.C.Strings.chars_ptr;
      Infolist_Integer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Infolist_String : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Infolist_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Infolist_Buffer : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : access int) return System.Address;
      Infolist_Time : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Time_T;
      Infolist_Free : access procedure (Arg1 : System.Address);

      --  Hdata
      Hdata_New : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : Interfaces.C.Strings.chars_ptr;
         Arg5 : int;
         Arg6 : int;
         Arg7 : access function
           (Arg1 : System.Address;
            Arg2 : System.Address;
            Arg3 : System.Address;
            Arg4 : System.Address) return int;
         Arg8 : System.Address) return System.Address;
      Hdata_New_Var : access procedure
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : int;
         Arg4 : int;
         Arg5 : int;
         Arg6 : Interfaces.C.Strings.chars_ptr;
         Arg7 : Interfaces.C.Strings.chars_ptr);
      Hdata_New_List : access procedure
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr;
         Arg3 : System.Address;
         Arg4 : int);
      Hdata_Get : access function
        (Arg1 : access T_Weechat_Plugin;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Hdata_Get_Var_Offset : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Hdata_Get_Var_Type : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return int;
      Hdata_Get_Var_Type_String : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Hdata_Get_Var_Array_Size : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return int;
      Hdata_Get_Var_Array_Size_String : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Hdata_Get_Var_Hdata : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Hdata_Get_Var : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Hdata_Get_Var_At_Offset : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : int) return System.Address;
      Hdata_Get_List : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Hdata_Check_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : System.Address) return int;
      Hdata_Move : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : int) return System.Address;
      Hdata_Search : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : int) return System.Address;
      Hdata_Char : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return char;
      Hdata_Integer : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return int;
      Hdata_Long : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return long;
      Hdata_String : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
      Hdata_Pointer : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Hdata_Time : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return Time_T;
      Hdata_Hashtable : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr) return System.Address;
      Hdata_Compare : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : System.Address;
         Arg4 : Interfaces.C.Strings.chars_ptr;
         Arg5 : int) return int;
      Hdata_Set : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : Interfaces.C.Strings.chars_ptr;
         Arg4 : Interfaces.C.Strings.chars_ptr) return int;
      Hdata_Update : access function
        (Arg1 : System.Address;
         Arg2 : System.Address;
         Arg3 : System.Address) return int;
      Hdata_Get_String : access function
        (Arg1 : System.Address;
         Arg2 : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;

      --  Upgrade
      Upgrade_New : access function
        (File_Name : Interfaces.C.Strings.chars_ptr;
         Callback  : access function
           (Pointer   : System.Address;
            Data      : System.Address;
            File      : System.Address;
            Object_Id : int;
            Infolist  : System.Address) return int;
         Callback_Pointer : System.Address;
         Callback_Data    : System.Address) return System.Address;
      Upgrade_Write_Object : access function
        (File      : System.Address;
         Object_Id : int;
         Infolist  : System.Address) return int;
      Upgrade_Read  : access function (File : System.Address) return int;
      Upgrade_Close : access procedure (File : System.Address);
   end record
     with Convention => C_Pass_By_Copy;

   type Plugin_Ptr is access all T_Weechat_Plugin;

   No_Timer : constant Timer := (Result => null, Plugin => null);

end WeeChat;
