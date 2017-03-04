## Makefile to export org in blog file to html

emacs ?= emacs
OUTDIR := ""

all:
	$(emacs) -Q --script blog.el $(OUTDIR)

clean:
	rm -rf _site/*
