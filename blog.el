;;; blog.el --- Config file used to export blog      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Peng Li

;; Author: Peng Li <seudut@gmail.com>
;; Keywords: lisp

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

;; This is emacs config used to export Blog Org files in batch mode.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq debug-on-error t)

;; package initialise
(unless (package-installed-p 'htmlize)
  (package-refresh-contents)
  (package-install 'htmlize))


;; constants
;; (defconst root-dir "~/Private/blog/")
(defconst root-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar publish-dir (concat root-dir "_site/"))
(defconst css-file "../css/worg.css")
(defvar force-publish nil)
;; (defconst css-file (concat root-dir "css/worg.css"))

(let ((aa (pop command-line-args-left)))
  (if (>  (length aa) 0)
      (setq publish-dir aa)))


(let ((force (pop command-line-args-left)))
  (if (string= force "true")
      (setq force-publish t)))


;; (setq publish-dir (or (pop command-line-args-left) publish-dir))
(message publish-dir)

(require 'org)
(require 'ox-publish)
(require 'htmlize)

(message "Org-mode version %s" (org-version))
(message "publish directory is %s" publish-dir)
(message "force %s" force-publish)

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

;; don't know why setting this null in `org-publish-project-alist' doesn't work
;; (setf org-html-home/up-format "")

(setq org-publish-project-alist
      `(
	("blog-pages"
	 ;; publishing
	 :base-directory ,root-dir
	 :base-extension "org"
	 :publishing-directory ,publish-dir
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
	 :publishing-directory ,(concat publish-dir "/posts")
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
	 )
	
	("blog-css"
	 :base-directory ,(concat root-dir "/css")
	 :base-extension ".*"
	 :publishing-directory ,(concat publish-dir "/css")
	 :publishing-function org-publish-attachment
	 :recursive t)
	("blog-cgi"
	 :base-directory ,(concat root-dir "/cgi-bin")
	 :base-extension ".*"
	 :publishing-directory ,(concat publish-dir "/cgi-bin")
	 :publishing-function org-publish-attachment
	 :recursive t)
        ("blog" :components ("blog-pages" "blog-posts" "blog-css" "blog-cgi"))))

(org-publish-project "blog" force-publish)

;; (provide 'blog)
(kill-emacs 0)
;;; blog.el ends here
