CFLAGS  ?= -O2 -march=native

GNATMAKE = gprbuild -dm -p
GNATCLEAN = gprclean -q
GNATINSTALL = gprinstall

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

installcmd = $(GNATINSTALL) -p \
	--sources-subdir=$(includedir) \
	--project-subdir=$(gprdir) \
	--lib-subdir=$(libdir) \
	--ali-subdir=$(alidir) \
	--prefix=$(PREFIX)

.PHONY: all debug clean install uninstall

all:
	$(GNATMAKE) -P tools/weechat.gpr -cargs $(CFLAGS)

debug:
	$(GNATMAKE) -P tools/weechat.gpr -XMode=debug -cargs $(CFLAGS)

clean:
	$(GNATCLEAN) -P tools/weechat.gpr
	rm -rf build

install:
	$(installcmd) -f --install-name='weechat' -P tools/weechat.gpr

uninstall:
	$(installcmd) --uninstall --install-name='weechat' -P tools/weechat.gpr
