
.PHONY: default clean intro psbook html all

intro_ := mufasa_intro
psbook_ := mufasa_ps_handbook
book_ := mufasa_handbook
dev_ := mufasa_developers

default: tex

clean:
	rm -rvf $(intro_)
	rm -rvf $(psbook_)
	rm -rvf $(book_)
	rm -rvf $(dev_)
	find -iname "$(intro_)*" | grep -v svn | grep -v tex | xargs rm -vf
	find -iname "$(psbook_)*" | grep -v svn | grep -v tex | xargs rm -vf
	find -iname "$(book_)*" | grep -v svn | grep -v tex | xargs rm -vf
	find -iname "$(dev_)*" | grep -v svn | grep -v tex | xargs rm -vf
	$(MAKE) -C Pics/ clean

tex:
	$(MAKE) -C Pics/
	texi2pdf $(intro_).tex #--silent
	texi2pdf $(psbook_).tex #--silent
	texi2pdf $(book_).tex #--silent
	texi2pdf $(dev_).tex #--silent


html:
	$(MAKE) -C Pics/
	latex2html $(intro_).tex -local_icons -nofootnode
	latex2html $(psbook_).tex -local_icons -nofootnode
	latex2html $(book_).tex -local_icons -nofootnode
	latex2html $(dev_).tex -local_icons -nofootnode
