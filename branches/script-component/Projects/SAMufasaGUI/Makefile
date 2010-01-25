#$ fpc  -MObjFPC -Scgi -O2 -OoREGVAR -gl -vewnhi -l -Fu../../Units/MMLCore/ -Fu../../Units/MMLAddon/ -Fu../../Units/PascalScript/ -Fu../../Units/Misc/ -Fu../../../lazarus/components/synedit/units/x86_64-linux/ -Fu../../../lazarus/ideintf/units/x86_64-linux/ -Fu../../../lazarus/lcl/units/x86_64-linux/ -Fu../../../lazarus/lcl/units/x86_64-linux/gtk2/ -Fu../../../lazarus/packager/units/x86_64-linux/ -Fu. -oSAMufasaGUI -dUseCThreads -dM_MEMORY_DEBUG -dLCL -dLCLgtk2 project1.lpr

.PHONY: default clean

#Set these ----------
platform := x86_64-linux
widgetset := gtk2
lazaruspath := /home/merlijn/Programs/lazarus

#-dUseCThreads on linux, none on windows. :) 
platformdefines := -dUseCThreads
#End set there -------

CC := fpc
build := $(CC)
binary := SAMufasaGUI
flags := -MObjFPC -Scgi -O2 -OoREGVAR -gl -vewnhi -l -Fu.
defines := $(platformdefines) -dM_MEMORY_DEBUG -dLCL -dLCL$(widgetset)
units := -Fu../../Units/MMLCore/ -Fu../../Units/MMLAddon/ -Fu../../Units/PascalScript/ -Fu../../Units/Misc/



lclplatpath := $(lazaruspath)/lcl/units/$(platform)/

lazarusunits := -Fu$(lazaruspath)/components/synedit/units/$(platform)/ -Fu$(lazaruspath)ideintf/units/$(platform)/ -Fu$(lclplatpath) -Fu$(lclplatpath)$(widgetset)/ -Fu$(lazaruspath)/packager/units/$(platform)/

default: $(binary)

clean:
	rm -f *.o *.ppu $(binary) 

$(binary):
	$(CC) $(flags) $(units) $(lazarusunits) -o$(binary) $(defines) project1.lpr 
	
	-Fu/usr/local/share/lazarus/ideintf/units/x86_64-linux/
