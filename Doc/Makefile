
.PHONY: default clean

self := mufasa_intro

default: createreport

clean:
	rm -f *.{pdf,log, aux}
	$(MAKE) -C Pics/ clean

createreport:
	$(MAKE) -C Pics/
	texi2pdf $(self).tex --silent
