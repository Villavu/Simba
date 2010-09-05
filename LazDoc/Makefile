
.PHONY:	default clean

docunits := os_linux dtm mufasatypes bitmaps colour_conv
coreunits := os_linux dtm mufasatypes bitmaps colour_conv

default:
	
	fpdoc --package=Simba \
		`echo $(docunits) | sed -r 's/\w+/--descr=&.xml/g'` \
		`echo $(coreunits) | sed -r 's/\w+/--input=..\/Units\/MMLCore\/&.pas/g'` \
		--output=simbafpcdoc/


clean:
	rm -rf simbafpcdoc
