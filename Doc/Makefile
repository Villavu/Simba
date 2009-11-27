
.PHONY: default clean intro psbook

intro_ := mufasa_intro
psbook_ := mufasa_ps_handbook


default: createreport

clean:
	find -iname "$(intro_)*" | grep -v svn | grep -v tex | xargs rm -vf
	find -iname "$(psbook_)*" | grep -v svn | grep -v tex | xargs rm -vf
	$(MAKE) -C Pics/ clean

createreport:
	$(MAKE) -C Pics/
	texi2pdf $(intro_).tex #--silent
	texi2pdf $(psbook_).tex #--silent
