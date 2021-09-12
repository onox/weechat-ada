[![Build status](https://github.com/onox/weechat-ada/actions/workflows/build.yaml/badge.svg)](https://github.com/onox/weechat-ada/actions/workflows/build.yaml)
[![License](https://img.shields.io/github/license/onox/weechat-ada.svg?color=blue)](https://github.com/onox/weechat-ada/blob/master/LICENSE)
[![Alire crate](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/weechat_ada.json)](https://alire.ada.dev/crates/weechat_ada.html)
[![GitHub release](https://img.shields.io/github/release/onox/weechat-ada.svg)](https://github.com/onox/weechat-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20libera.chat-orange.svg)](https://libera.chat)
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/ada-lang/Lobby)

# weechat-ada

Ada 2012 library for [WeeChat][url-weechat] plug-ins.

The WeeChat plug-in datastructure currently only works with WeeChat 1.9.1 or 2.8.
If the library does not
work for the version provided by your distribution, then you need to
open an issue so we can take a look at solving this.

Because WeeChat plug-ins are not backward compatible, a plug-in intended
for a specific version of WeeChat requires a specific release of weechat-ada:

| Release | WeeChat version |
|---------|-----------------|
| v2.x.y  | 1.9.1           |
| v3.x.y  | 2.8             |

WeeChat provides many functions to plug-ins and many of them are not needed
for a language like Ada:

| Category      | Binded |
|---------------|--------|
| Strings       |        |
| UTF-8         |        |
| Directories   |        |
| Util          |        |
| Sorted lists  |        |
| Array lists   |        |
| Hashtables    |        |
| Configuration | Yes    |
| Key bindings  |        |
| Display       | Yes    |
| Hooks         | Yes    |
| Buffers       |        |
| Windows       |        |
| Nicklist      |        |
| Bars          |        |
| Commands      | Yes    |
| Network       |        |
| Infos         | Yes    |
| Infolists     |        |
| Hdata         |        |
| Upgrade       |        |

## Usage

```ada
package Plugin_My_Plugin_Name
   pragma Elaborate_Body;
end Plugin_My_Plugin_Name;
```

Because your plug-in is build as a stand-alone library, the Ada package
of your plug-in should have a unique name in order to avoid unexpected
behavior due to symbol name collisions when you load multiple Ada plug-ins.

You must export several C strings and two functions, called when the plug-in
is loaded and unloaded:

```ada
with System;

with Interfaces.C;

with WeeChat;

package body Plugin_My_Plugin_Name is

   use WeeChat;

   procedure Plugin_Initialize (Plugin : Plugin_Ptr) is
   begin
      Print (Plugin, "Hello world from Ada");

      --  Call the various On_* subprograms here to set-up hooks
   end Plugin_Initialize;

   procedure Plugin_Finalize (Plugin : Plugin_Ptr) is null;

   Plugin_Name : constant C_String := "mypluginname" & L1.NUL
     with Export, Convention => C, External_Name => "weechat_plugin_name";

   Plugin_Author : constant C_String := "Ada Lovelace" & L1.NUL
     with Export, Convention => C, External_Name => "weechat_plugin_author";

   Plugin_Description : constant C_String := "My WeeChat plug-in written in Ada 2012" & L1.NUL
     with Export, Convention => C, External_Name => "weechat_plugin_description";

   Plugin_Version : constant C_String := "1.0" & L1.NUL
     with Export, Convention  => C, External_Name => "weechat_plugin_version";

   Plugin_License : constant C_String := "Apache-2.0" & L1.NUL
     with Export, Convention => C, External_Name => "weechat_plugin_license";

   Plugin_API_Version : constant C_String := WeeChat.Plugin_API_Version
     with Export, Convention => C, External_Name => "weechat_plugin_api_version";

   function Plugin_Init
     (Object : Plugin_Ptr;
      Argc   : Interfaces.C.int;
      Argv   : System.Address) return Callback_Result
   with Export, Convention => C, External_Name => "weechat_plugin_init";

   function Plugin_End (Object : Plugin_Ptr) return Callback_Result
     with Export, Convention => C, External_Name => "weechat_plugin_end";

   function Plugin_Init
     (Object : Plugin_Ptr;
      Argc   : Interfaces.C.int;
      Argv   : System.Address) return Callback_Result is
   begin
      return Plugin_Init (Object, Plugin_Initialize'Access);
   end Plugin_Init;

   function Plugin_End (Object : Plugin_Ptr) return Callback_Result is
   begin
      return Plugin_End (Object, Plugin_Finalize'Access);
   end Plugin_End;

end Plugin_My_Plugin_Name;
```

You must be careful not to spawn any tasks that cannot be terminated. For
example, `Ada.Real_Time.Timing_Events` creates an internal task that will
not be terminated if you reload the plug-in. Instead, you can use the
`Set_Timer` subprogram in package `WeeChat`. If you do create your own task,
you must call an entry that signals to the task that it must stop and then
you must wait in a loop for the `'Terminated` attribute to become true:

```
My_Task.Stop;
loop
   exit when My_Task'Terminated;
end loop;
```

If you create any tasks, you should add this code to an exception handler
in `Plugin_Initialize` and `Plugin_Finalize`.

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler

 * [Alire][url-alire]

## Installing dependencies on Ubuntu 20.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-10 gprbuild weechat-dev
```

## Using weechat-ada in your project

Use the library in your crates as follows:

```
alr with weechat_ada
```

Specify the dependency in your \*.gpr project file:

```ada
with "weechat_ada";
```

You must make sure to build the plug-in as a Stand-Alone Library:

```ada
library project WeeChat_My_Plugin is

   for Library_Name use "weechat_my_plugin";
   for Library_Version use "ada-my-plugin.so";
   for Library_Kind use "relocatable";

   for Library_Interface use ("plugin_my_plugin_name");
   for Library_Standalone use "encapsulated";

end WeeChat_My_Plugin;
```

You can copy ada-my-plugin.so to ~/.weechat/plugins/.

`Library_Standalone` should either be "standard" or "encapsulated".
"encapsulated" will put the GNAT RTS inside your library and will force any
third-party Ada libraries that you import to be compiled as "static-pic" instead
of "dynamic" or "relocatable". "standard" does not have these restrictions,
but reloading or unloading your plug-in may segfault WeeChat if there is
another plug-in that uses "standard" as well. Therefore the safest option
is to choose "encapsulated" if you can live with the restrictions.

If you need to link to some specific library, you can use `Library_Options`.
For example:

```ada
for Library_Options use ("-lcanberra");
```

For a complete example see [weechat-canberra][url-weechat-canberra] or
[weechat-emoji][url-weechat-emoji].

## Contributing

Read the [contributing guidelines][url-contributing] if you want to add
a bugfix or an improvement.

## License

This library is licensed under the [Apache License 2.0][url-apache].
The first line of each Ada file should contain an SPDX license identifier tag that
refers to this license:

    SPDX-License-Identifier: Apache-2.0

Note that WeeChat itself is licensed under GPL3+ and if you use "encapsulated"
for `Library_Standalone` then the GNAT RTS will be put inside your library.

  [url-alire]: https://alire.ada.dev/
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-contributing]: /CONTRIBUTING.md
  [url-weechat]: https://weechat.org/
  [url-weechat-canberra]: https://github.com/onox/weechat-canberra
  [url-weechat-emoji]: https://github.com/onox/weechat-emoji
