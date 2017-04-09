## Makefile to export org in blog file to html

emacs ?= emacs
OUTDIR := _site
FORCE := "no"
BLOGDIR := $(PWD)

# To color the code block, install htmlize package
INIT_PACKAGES = "(progn \
				(require 'package) \
				(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\")) \
				(package-initialize) \
				(unless (package-installed-p 'htmlize) (package-refresh-contents) (package-install 'htmlize)))"

# used for automation on server
CSS := $(wildcard css/*.css)
OBJS := $(addprefix $(OUTDIR)/,$(CSS))

#$(OUTDIR)/css/%.css:css/%.css
#	echo "***** css changed remove timestamps *****\n"
#	rm -rf ~/.org-timestamps
#$(OBJS): $(CSS) my-publish.el
$(OBJS): _site/css/%.css:css/%.css
	rm -rf ~/.org-timestamps

.PHONY: update publish test clean

update:
	git pull

publish: update $(OBJS)
	$(emacs) -Q --batch  \
		--eval $(INIT_PACKAGES) \
		--eval '(setq debug-on-error t)' \
		-l my-publish.el index.org \
		--eval '(blog-setup-project-alist "$(BLOGDIR)" "$(OUTDIR)")' \
		--eval '(org-publish-current-project)'

# test will force publishing all files in the porject 
test:
	$(emacs) -Q --batch \
		--eval $(INIT_PACKAGES) \
		--eval '(setq debug-on-error t)' \
		-l my-publish.el index.org \
		--eval '(blog-setup-project-alist "$(BLOGDIR)")' \
		--eval '(org-publish-current-project t)'

clean:
	rm -rf _site/*
