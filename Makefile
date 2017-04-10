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

.PHONY: update publish clean

publish: update temp
	$(emacs) -Q --batch  \
		--eval $(INIT_PACKAGES) \
		--eval '(setq debug-on-error t)' \
		-l my-publish.el index.org \
		--eval '(blog-setup-project-alist "$(BLOGDIR)" "$(OUTDIR)")' \
		--eval '(org-publish-current-project)'

update:
	git pull

# if the source files (css) file changed, remote the org-timestamps to 
# re-generate all the html files
temp: $(CSS) my-publish.el
	rm -rf ~/.org-timestamps
	touch temp


## test will force publishing all files in the porject 
#test:
#	$(emacs) -Q --batch \
#		--eval $(INIT_PACKAGES) \
#		--eval '(setq debug-on-error t)' \
#		-l my-publish.el index.org \
#		--eval '(blog-setup-project-alist "$(BLOGDIR)")' \
#		--eval '(org-publish-current-project t)'

clean:
	rm -rf _site/*
