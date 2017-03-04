## Makefile to export org in blog file to html

emacs ?= emacs

OUT_DIR=/Users/peli3/Private/blog/_site

all:
	$(emacs) -Q --batch \
		-l ./blog.el \
		index.org \
		--eval '(org-publish-current-project t)'
clean:
	rm -rf _site/*
