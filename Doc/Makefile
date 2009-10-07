
.PHONY: default clean

self := mufasa_intro

default: createreport

clean:
	find -iname "$(self)*" | grep -v svn | grep -v tex | xargs rm -vf
	$(MAKE) -C Pics/ clean

createreport:
	$(MAKE) -C Pics/
	texi2pdf $(self).tex #--silent
