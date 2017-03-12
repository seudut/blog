## Makefile to export org in blog file to html

emacs ?= emacs
OUTDIR := 
FORCE := "no"
BLOGDIR := $(PWD)

all:
	$(emacs) -Q --script blog.el $(OUTDIR) $(FORCE)

# used for automation on server
update:
	git pull && $(emacs) -Q --script blog.el "~/www/html" "true" 

publish:
	$(emacs) -Q --batch -l my-publish.el index.org \
		--eval '(setq debug-on-error t)' \
		--eval '(blog-setup-project-alist "$(BLOGDIR)" "$(OUTDIR)")' \
		--eval '(org-publish-current-project)'

compile:
	$(emacs) -Q --batch -l my-publish.el index.org \
		--eval '(setq debug-on-error t)' \
		--eval '(blog-setup-project-alist "$(BLOGDIR)" "$(OUTDIR)")' \
		--eval '(org-publish-current-project t)'

clean:
	rm -rf _site/*
