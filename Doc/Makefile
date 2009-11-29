
.PHONY: default clean intro psbook html all

intro_ := mufasa_intro
psbook_ := mufasa_ps_handbook


default: intro

clean:
	rm -rvf $(intro_)
	rm -rvf $(psbook_)
	find -iname "$(intro_)*" | grep -v svn | grep -v tex | xargs rm -vf
	find -iname "$(psbook_)*" | grep -v svn | grep -v tex | xargs rm -vf
	$(MAKE) -C Pics/ clean

intro:
	$(MAKE) -C Pics/
	texi2pdf $(intro_).tex #--silent
	texi2pdf $(psbook_).tex #--silent

html:
	$(MAKE) -C Pics/
	latex2html $(intro_).tex -local_icons -nofootnode
	latex2html $(psbook_).tex -local_icons -nofootnode
