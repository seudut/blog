## Makefile to export org in blog file to html

emacs ?= emacs
OUTDIR := ""
FORCE := "no"

all:
	$(emacs) -Q --script blog.el $(OUTDIR) $(FORCE)

# used for automation on server
update:
	git pull
	$(emacs) -Q --script blog.el "~/www/html" "true"

clean:
	rm -rf _site/*
