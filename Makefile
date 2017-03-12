## Makefile to export org in blog file to html

emacs ?= emacs
OUTDIR := 
FORCE := "no"
BLOGDIR := $(PWD)

# To color the code block, install htmlize package
INIT_PACKAGES = "(progn \
				(require 'package) \
				(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\")) \
				(package-initialize) \
				(unless (package-installed-p 'htmlize) (package-refresh-contents) (package-install 'htmlize)))"

all:
	$(emacs) -Q --script blog.el $(OUTDIR) $(FORCE)

# used for automation on server
update:
	git pull && $(emacs) -Q --script blog.el "~/www/html" "true" 

publish:
	$(emacs) -Q --batch -l my-publish.el index.org \
		--eval $(INIT_PACKAGES) \
		--eval '(setq debug-on-error t)' \
		--eval '(blog-setup-project-alist "$(BLOGDIR)" "$(OUTDIR)")' \
		--eval '(org-publish-current-project)'

# test will force publishing all files in the porject 
test:
	$(emacs) -Q --batch -l my-publish.el index.org \
		--eval $(INIT_PACKAGES) \
		--eval '(setq debug-on-error t)' \
		--eval '(blog-setup-project-alist "$(BLOGDIR)")' \
		--eval '(org-publish-current-project t)'

clean:
	rm -rf _site/*
