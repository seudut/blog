## Makefile to export org in blog file to html

emacs ?= emacs
OUTDIR := _site
BLOGDIR := $(PWD)
UPDATE :=

# To color the code block, install htmlize package
INIT_PACKAGES = "(progn \
				(require 'package) \
				(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\")) \
				(package-initialize) \
				(unless (package-installed-p 'htmlize) (package-refresh-contents) (package-install 'htmlize)))"

CSS := $(wildcard css/*.css)

.PHONY: update publish clean

publish: update temp
	$(emacs) -Q --batch  \
		--eval $(INIT_PACKAGES) \
		--eval '(setq debug-on-error t)' \
		-l my-publish.el index.org \
		--eval '(blog-setup-project-alist "$(BLOGDIR)" "$(OUTDIR)")' \
		--eval '(org-publish-current-project)'

update:
ifeq ($(UPDATE), true)
	git pull
endif

# if the source files (css) file changed, remote the org-timestamps to 
# re-generate all the html files
temp: $(CSS) my-publish.el
	rm -rf ~/.org-timestamps
	touch temp

clean:
	rm -rf _site/*
