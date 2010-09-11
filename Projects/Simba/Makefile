# Makefile for Simba
# Currently written for and tested to run only on Linux i386 and x86_64.
# Currently supports cross compilation to fpc targets i386-linux x86_64-linux i386-win32 x86_64-win64
# Obviously your system must have the needed binaries and libraries to cross-compile for that to work,
# but this will of course compile a native binary for your system only, if you wish.

.PHONY: default build rebuild unknown recursive_build clean i386-linux x86_64-linux i386-win32 x86_64-win64 all

# Set these
#lazaruspath := /usr/lib/lazarus/0.9.28.2
lazaruspath := ../../../lazarus
# set this correctly only if you want to apply and reverse the patch automatically
rutis_x64_patch := ../../../0-rutis-x64.patch

# uncomment this or send distrib=1 via command line to build the distributable archive as well
# if you set distrib, be sure to set the next option as well
#distrib := 1

# if you didn't set distrib, the following option isn't used

# The plugins directory needs to have the following structure (for SMART, you can add any other plugins you want):
# all-in-one/libsmart64.so:      ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, stripped
# all-in-one/libsmart.so:        ELF 32-bit LSB shared object, Intel 80386, version 1 (SYSV), dynamically linked, stripped
# all-in-one/Public_SMART64.dll: PE32+ executable for MS Windows (DLL) (GUI)
# all-in-one/Public_SMART.dll:   PE32 executable for MS Windows (DLL) (GUI) Intel 80386 32-bit
# i386-linux/libsmart.so:        ELF 32-bit LSB shared object, Intel 80386, version 1 (SYSV), dynamically linked, stripped
# i386-win32/Public_SMART.dll:   PE32 executable for MS Windows (DLL) (GUI) Intel 80386 32-bit
# x86_64-linux/libsmart.so:      ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, stripped
# x86_64-win64/Public_SMART.dll: PE32+ executable for MS Windows (DLL) (GUI)
plugins_dir := ../../../plugins

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
	else ifeq ($(uname_M),i486)
		our_target := i386-linux
	else ifeq ($(uname_M),i586)
		our_target := i386-linux
	else ifeq ($(uname_M),i686)
		our_target := i386-linux
	else
		our_target := unknown
	endif
else
	our_target := unknown
endif

# now we need to see if we have the rutis_x64_patch or not
# we define 'have_rutis_x64_patch' if we have it
# we don't want to try to apply it or reverse it if it 
# doesn't exist or the make will error out
ifneq ($(strip $(wildcard $(rutis_x64_patch))),)
	# the patch file exists
	have_rutis_x64_patch := 1
endif

CC := fpc
build := $(CC)

# -Xd doesn't seem to cause problems when not cross-compiling, and is needed when cross-compiling, so I'm leaving it here
common_flags := -Xd -MObjFPC -Scgi -O2 -OoREGVAR -gl -vewnhi -l -Fu. -dM_MEMORY_DEBUG -dLCL
units := -Fu../../Units/MMLCore/ -Fu../../Units/MMLAddon/ -Fu../../Units/MMLAddon/PSInc/ -Fu../../Units/PascalScript/ -Fu../../Units/Misc/ -Fu../../Units/Linux/ -Fu../../Units/Synapse/ -Fu../../Units/RUTIS/

binary := Simba.$(platform)
# windows wants executables to end in .exe
ifeq ($(widgetset),win32)
	binary_extension := .exe
endif
lclplatpath := $(lazaruspath)/lcl/units/$(platform)/
lazarusunits := -Fu$(lazaruspath)/components/synedit/units/$(platform)/ -Fu$(lazaruspath)ideintf/units/$(platform)/ -Fu$(lclplatpath) -Fu$(lclplatpath)$(widgetset)/ -Fu$(lazaruspath)/packager/units/$(platform)/ -Fu$(lazaruspath)/components/mouseandkeyinput/

default: build

build: $(our_target)

rebuild: clean build

recursive_build: $(binary)$(binary_extension)

unknown:
	@echo Unable to auto-detect the OS and architecture you wish to build for, please specify target manually.

clean:
#       clean this directory
	rm -f *.a *.o *.obj *.ppu *.rst Simba.i386-* Simba.x86_64-* ../../*.7z
#       now clean all the unit directories (I love Bash...)
	rm -f ../../Units/*/*.a ../../Units/*/*.o ../../Units/*/*.ppu ../../Units/*/*.rst

$(binary)$(binary_extension):
	$(CC) $(common_flags) $(platform_flags) -dLCL$(widgetset) $(units) $(lazarusunits) -o$(binary)$(binary_extension) Simba.lpr
	strip $(binary)$(binary_extension)
ifdef distrib
# you may not want this down below here, just don't define distrib (it makes a distribution .7z file in the top-level directory)

# setup the Scripts directory correctly
	mkdir -p ../../Scripts
	mv ../../Tests ../../Scripts
# now setup the Plugins directory
	mv ../../Plugins ../../Plugins_bak
	mkdir -p ../../Plugins
	cp $(plugins_dir)/$(platform)/* ../../Plugins/
	7z a ../../$(binary).7z $(binary)$(binary_extension) ../../Extensions/ ../../Fonts/ ../../Includes/ ../../Plugins/ ../../Scripts
# now undo everything we did above
	rm -rf ../../Plugins
	mv ../../Plugins_bak ../../Plugins
	mv ../../Scripts/Tests ../../Tests
	rm -rf ../../Scripts
endif

i386-linux:
# reverse it if it has been applied
ifdef have_rutis_x64_patch
	-patch -R -N -r- -p1 -d../../ < $(rutis_x64_patch)
endif
	$(MAKE) recursive_build platform="i386-linux" widgetset="gtk2" platform_flags="-Tlinux -Pi386 -dUseCThreads"

x86_64-linux:
# apply the patch
ifdef have_rutis_x64_patch
	-patch -N -r- -p1 -d../../ < $(rutis_x64_patch)
endif
	$(MAKE) recursive_build platform="x86_64-linux" widgetset="gtk2" platform_flags="-Tlinux -Px86_64 -dUseCThreads"
# reverse it
ifdef have_rutis_x64_patch
	-patch -R -N -r- -p1 -d../../ < $(rutis_x64_patch)
endif

i386-win32:
# reverse it if it has been applied
ifdef have_rutis_x64_patch
	-patch -R -N -r- -p1 -d../../ < $(rutis_x64_patch)
endif
	$(MAKE) recursive_build platform="i386-win32" widgetset="win32" platform_flags="-Twin32 -Pi386"

x86_64-win64:
# apply the patch
ifdef have_rutis_x64_patch
	-patch -N -r- -p1 -d../../ < $(rutis_x64_patch)
endif
	$(MAKE) recursive_build platform="x86_64-win64" widgetset="win32" platform_flags="-Twin64 -Px86_64"
# reverse it
ifdef have_rutis_x64_patch
	-patch -R -N -r- -p1 -d../../ < $(rutis_x64_patch)
endif

all: i386-linux x86_64-linux i386-win32 x86_64-win64

all-in-one-distrib: all
# setup the Scripts directory correctly
	mkdir -p ../../Scripts
	mv ../../Tests ../../Scripts
# now setup the Plugins directory
	mv ../../Plugins ../../Plugins_bak
	mkdir -p ../../Plugins
	cp $(plugins_dir)/all-in-one/* ../../Plugins/
	7z a ../../Simba.all-in-one.7z Simba.i386-* Simba.x86_64-* ../../Extensions/ ../../Fonts/ ../../Includes/ ../../Plugins/ ../../Scripts
# now undo everything we did above
	rm -rf ../../Plugins
	mv ../../Plugins_bak ../../Plugins
	mv ../../Scripts/Tests ../../Tests
	rm -rf ../../Scripts
