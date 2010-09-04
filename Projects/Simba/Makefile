# Makefile for Simba
# Currently written for and tested to run only on Linux i386 and x86_64.
# Currently supports cross compilation to fpc targets i386-linux x86_64-linux i386-win32 x86_64-win64
# Obviously your system must have the needed binaries and libraries to cross-compile for that to work,
# but this will of course compile a native binary for your system only if you wish.

.PHONY: default build rebuild unknown recursive_build clean i386-linux x86_64-linux i386-win32 x86_64-win64 all

# Set this
#lazaruspath := /usr/lib/lazarus/0.9.28.2
lazaruspath := /home/mopar/simba/lazarus

# Shouldn't need to touch below here, unless you add a unit or something...

# let's detect what linux platform we are running on, so if we run build or the default target
# we by default build the correct executable for our architecture...
# (cross-compiling is near-impossible on windows, so this makefile doesn't need to try and support it)
# as Simba supports more OSs (BSD, OSX etc) we can add support for cross compilation here too
uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
ifeq ($(uname_S),Linux)
	uname_M := $(shell sh -c 'uname -m 2>/dev/null || echo not')
	ifeq ($(uname_M),x86_64)
		our_target := x86_64-linux
	else ifeq ($(uname_M),i386)
		our_target := i386-linux
	else
		our_target := unknown
	endif
else
	our_target := unknown
endif

CC := fpc
build := $(CC)

# -Xd doesn't seem to cause problems when not cross-compiling, and is needed when cross-compiling, so I'm leaving it here
common_flags := -Xd -MObjFPC -Scgi -O2 -OoREGVAR -gl -vewnhi -l -Fu. -dM_MEMORY_DEBUG -dLCL
units := -Fu../../Units/MMLCore/ -Fu../../Units/MMLAddon/ -Fu../../Units/MMLAddon/PSInc/ -Fu../../Units/PascalScript/ -Fu../../Units/Misc/ -Fu../../Units/Linux/ -Fu../../Units/Synapse/ -Fu../../Units/RUTIS/

binary := Simba.$(platform)
lclplatpath := $(lazaruspath)/lcl/units/$(platform)/
lazarusunits := -Fu$(lazaruspath)/components/synedit/units/$(platform)/ -Fu$(lazaruspath)ideintf/units/$(platform)/ -Fu$(lclplatpath) -Fu$(lclplatpath)$(widgetset)/ -Fu$(lazaruspath)/packager/units/$(platform)/ -Fu$(lazaruspath)/components/mouseandkeyinput/

default: build

build: $(our_target)

rebuild: clean build

recursive_build: $(binary)

unknown:
	@echo Unable to auto-detect the OS and architecture you wish to build for, please specify target manually.

clean:
	rm -f *.o *.ppu Simba.i386-* Simba.x86_64-*

$(binary):
	$(CC) $(common_flags) $(platform_flags) -dLCL$(widgetset) $(units) $(lazarusunits) -o$(binary) Simba.lpr
	strip $(binary) 

i386-linux:
	$(MAKE) recursive_build platform="i386-linux" widgetset="gtk2" platform_flags="-Tlinux -Pi386 -dUseCThreads"

x86_64-linux:
	$(MAKE) recursive_build platform="x86_64-linux" widgetset="gtk2" platform_flags="-Tlinux -Px86_64 -dUseCThreads"

i386-win32:
	$(MAKE) recursive_build platform="i386-win32" widgetset="win32" platform_flags="-Twin32 -Pi386"

x86_64-win64:
	$(MAKE) recursive_build platform="x86_64-win64" widgetset="win32" platform_flags="-Twin64 -Px86_64"

all: i386-linux x86_64-linux i386-win32 x86_64-win64
