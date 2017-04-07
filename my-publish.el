;;; my-publish.el --- Publish blog with org-mode        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Peng Li

;; Author: Peng Li <seudut@gmail.com>
;; Keywords: lisp, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Using this emacs script to publish the blog org file 

;;; Code:

;; (defconst root-dir "~/Private/blog/")

(require 'org)
(require 'ox-publish)
(require 'htmlize)

;; To prevent inline-css when exporting html. will use external css
(setq org-html-htmlize-output-type 'css)

(setq blog-extra-head
      (concat
       "<link rel='stylesheet' href='/css/my.css' />\n"
       "<link rel='stylesheet' href='/css/code.css' />"))

(setq blog-header
      (concat
       "<header id= 'banner'>"
       "<a class=\"title\"href= '/' >Peng Li</a>"
       "<nav><ul>"
       "<li><a href= '/about.html' >About</a></li>"
       "<li><a href= '/posts/sitemap.html' >Blog</a></li>"
       "<li><a href= '/index.html' >Home</a></li>"
       "</ul></nav>"
       "</header>"
       "<hr>"))

(setq blog-footer
      (concat 
       "<hr />\n"
       "<p><span><a href= \"http://orgmode.org\"/>Org-mode </a>" (org-version) "</span>"))

(defun blog-setup-project-alist (root-dir &optional output-dir)
  "Set project alist. `output-dir' is the directory of publish-directory.
`root-dir' is the root directory of blog repository."
  (unless (> (length output-dir) 0)
    (setq output-dir (concat root-dir "/_site/")))
  (message "Blog dir is: %s\nOut dir is: %s"
	   (format root-dir)
	   (format output-dir))
  (setq org-publish-project-alist
	`(
	  ("blog-pages"
	   :base-directory ,root-dir
	   :base-extension "org"
	   :publishing-directory ,output-dir
	   :recursive nil
	   :publishing-function org-html-publish-to-html
	   
	   ;; html style
	   :htlm-link-home "/"
	   ;; disable home/up div
	   :html-home/up-format ""
	   :html-link-home ""
	   :html-link-up ""
	   ;;
	   :html-head nil
	   :html-head-include-default-style nil
	   :html-head-include-scripts nil
	   :html-head-extra  ,blog-extra-head
	   :html-preamble ,blog-header
	   :html-postamble ,blog-footer

	   ;; other generic options
	   :with-toc nil
	   )

	  ("blog-posts"
	   :base-directory ,(concat root-dir "/posts")
	   :base-extension "org"
	   :publishing-directory ,(concat output-dir "/posts")
	   :recursive t
	   :publishing-function org-html-publish-to-html

	   ;; html style
	   :html-link-home "/"
	   ;; disable Home/Up
	   :html-home/up-format ""
	   :html-link-up ""
	   :html-link-home ""
	   ;; Add css file, preamble and posamble
	   :html-head nil
	   :html-head-include-default-style nil
	   :html-head-include-scripts nil
	   :html-head-extra ,blog-extra-head
	   :html-preamble ,blog-header
	   :html-postamble ,blog-footer

	   ;; sitemap
	   :auto-sitemap t
	   :sitemap-filename "sitemap.org"
	   :sitemap-title "Sitemap"

	   ;; other generic options
	   :with-toc nil 		; table of contents
	   )
	  
	  ("blog-css"
	   :base-directory ,(concat root-dir "/css")
	   :base-extension ".*"
	   :publishing-directory ,(concat output-dir "/css")
	   :publishing-function org-publish-attachment
	   :recursive t)
	  ("blog-cgi"
	   :base-directory ,(concat root-dir "/cgi-bin")
	   :base-extension ".*"
	   :publishing-directory ,(concat output-dir "/cgi-bin")
	   :publishing-function org-publish-attachment
	   :recursive t)
	  ("blog" :components ("blog-pages" "blog-posts" "blog-css" "blog-cgi")))))

(provide 'my-publish)
;;; publish.el ends here
