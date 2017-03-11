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

;; To prevent inline-css when exporting html. will use external css
(setq org-html-htmlize-output-type 'css)

(setq blog-extra-head
      (concat
       ;; "<link rel='stylesheet' href='" css-file "' />\n"
       "<link rel='stylesheet' href='../css/main.css' />\n"
       "<link rel='stylesheet' href='../css/code.css' />"
       ))

(setq blog-header
      (concat
       " <header id= 'banner' > "
       "<h1><a href= '/' >Peng Li</a></h1>"
       "<hr>"
       "<nav><ul>"
       "<li><a href= '/' >About</a></li>"
       "<li><a href= '/blog.html' >Blog</a></li>"
       "<li><a href= '/home.html' >Home</a></li>"
       "</ul></nav>"
       "</header>"))

(setq blog-footer
      "<hr />\n
<p><span style=\"float: left;\"><a href= \"/blog.xml\">RSS</a></span>
License: <a href= \"https://creativecommons.org/licenses/by-sa/4.0/\">CC BY-SA 4.0</a></p\n")

(defun blog-setup-project-alist (root-dir &optinal output-dir)
  "Set project alist. `output-dir' is the directory of publish-directory.
`root-dir' is the root directory of blog repository."
  (unless output-dir (setq output-dir (concat root-dir "_site/")))
  (setq org-publish-project-alist
	`(
	  ("blog-pages"
	   ;; publishing
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
	   :html-postamble ,blog-footer)

	  ("blog-posts"
	   ;; publishing
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
	   :sitemap-title "Sitemap")
	  
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


(defun blog-publish (out-dir force)
  "publish the project"
  (interactive)
  (blog-setup-project out-dir)
  (org-publish-project "blog" force))


(provide 'my-publish)
;;; publish.el ends here
