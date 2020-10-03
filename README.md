[![License](https://img.shields.io/github/license/onox/weechat-ada.svg?color=blue)](https://github.com/onox/weechat-ada/blob/master/LICENSE)
[![Build status](https://img.shields.io/shippable/5f778515ebfcd50007db63b9/master.svg)](https://app.shippable.com/github/onox/weechat-ada)
[![GitHub release](https://img.shields.io/github/release/onox/weechat-ada.svg)](https://github.com/onox/weechat-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20freenode-orange.svg)](https://webchat.freenode.net/?channels=ada)

# weechat-ada

Ada 2012 library for [WeeChat][url-weechat] plug-ins.

The WeeChat plug-in datastructure currently only works with the one
found in Ubuntu 18.04 LTS (WeeChat 1.9.1). If the library does not
work for the version provided by your distribution, then you need to
open an issue so we can take a look at solving this (probably by maintaining
a modified version in a separate branch).

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
package Plugin
   pragma Elaborate_Body;
end Plugin;
```

```ada
with WeeChat;

package body Plugin is
   use WeeChat;

   procedure Plugin_Initialize is
   begin
      Print ("Hello world from Ada");

      --  Call the various On_* subprograms here to set-up hooks
   end Plugin_Initialize;

   procedure Plugin_Finalize is null;
begin
   WeeChat.Register
     ("ada", "Ada Lovelace", "Plug-in written in Ada 2012", "1.0", "Apache-2.0",
      Plugin_Initialize'Access, Plugin_Finalize'Access);
end Plugin;
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

You should add this code to an exception handler in `Plugin_Initialize` and
to `Plugin_Finalize`.

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler

 * GPRBuild and `make`

## Installing dependencies on Ubuntu 18.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild make weechat-dev
```

## Installation

A Makefile is provided to build the source code. Use `make` to build
the source code:

```
$ make
```

You can override CFLAGS if desired. After having compiled the source code,
the library can be installed by executing:

```
$ make PREFIX=/usr install
```

Change `PREFIX` to the preferred destination folder, for example `~/.local`.

## Using weechat-ada in your project

Specify the dependency in your \*.gpr project file:

```ada
with "weechat";
```

You must make sure to build the plug-in as a Stand-Alone Library:

```ada
for Library_Version use "my-weechat-plugin.so";
for Library_Interface use ("plugin");
```

If you need to link to some specific library, you can use `Library_Options`.
For example:

```ada
for Library_Options use ("-lcanberra");
```

## Contributing

Read the [contributing guidelines][url-contributing] if you want to add
a bugfix or an improvement.

## License

This library is licensed under the [Apache License 2.0][url-apache].
The first line of each Ada file should contain an SPDX license identifier tag that
refers to this license:

    SPDX-License-Identifier: Apache-2.0

  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-contributing]: /CONTRIBUTING.md
  [url-weechat]: https://weechat.org/
