.PHONY: default pics sphinx sphinx-pdf sphinx-html test release clean tarball

default: all
all:     sphinx
sphinx:  sphinx-html sphinx-pdf

pics:
	$(MAKE) -C Pics/

sphinx-pdf: pics
	$(MAKE) -C sphinx/ latexpdf

sphinx-html: pics
	$(MAKE) -C sphinx/ html

test:
	$(MAKE) -C sphinx/ linkcheck

release: sphinx-html
	mv sphinx/_build/html ../Documentation
	ln -s ../Documentation/html/index.html ../Documentation/Documentation.html

clean:
	$(MAKE) -C Pics/ clean
	$(MAKE) -C sphinx/ clean

tarball: html
	$(MAKE) -cjf doc.tar.bz2 -C sphinx/_build/html

