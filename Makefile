## Makefile to export org in blog file to html

emacs ?= emacs
OUTDIR := ""
FORCE := "no"

all:
	$(emacs) -Q --script blog.el $(OUTDIR) $(FORCE)

update:
	git pull
	$(emacs) -Q --script blog.el $(OUTDIR)

clean:
	rm -rf _site/*
