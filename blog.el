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

(setq org-publish-project-alist
      `(
        ;; ("org-notes"
        ;;  :base-directory ,root-dir
        ;;  :base-extension "org"
        ;;  :publishing-directory ,publish-dir
        ;;  :recursive t
        ;;  :publishing-function org-html-publish-to-html
        ;;  :headline-levels 4
        ;;  :section-numbers nil
        ;;  :auto-preamble t
        ;;  :auto-sitemap t          ;Generate sitmap.org automagicaly...
        ;;  :sitemap-filename "sitemap.org" ;... call it sitemap.org (it's the default )...
        ;;  :sitemap-title "Sitemap"

        ;;  :html-table-of-contents nil
        ;;  :html-postamble nil ;dont export creator auto validation info in html postamble div
        ;;  :html-link-home "/"
        ;;  :html-head  ,(concat  "<link rel='stylesheet' href='" css-file  "' />")
        ;;  :html-head-include-default-style nil
        ;;  :html-head-include-scripts nil)
	("blog-pages"
	 ;; publishing
	 :base-directory ,root-dir
	 :base-extension "org"
	 :publishing-directory ,publish-dir
	 :recursive nil
	 :publishing-function org-html-publish-to-html
	 
	 ;; html style
	 :html-head  ,(concat  "<link rel='stylesheet' href='" css-file  "' />")
	 :html-postamble nil)
	("blog-posts"
	 ;; publishing
	 :base-directory ,(concat root-dir "posts")
	 :base-extension "org"
	 :publishing-directory ,(concat publish-dir "posts")
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :html-link-home "/"
	 
	 ;; html style
	 :html-head ,(concat "<link rel='stylesheet' href='" css-file "' />")
	 ;; don't export creator auto validation info in html postamble div
	 :html-postamble nil)
	("blog-css"
	 :base-directory ,(concat root-dir "css")
	 :base-extension ".*"
	 :publishing-directory ,(concat publish-dir "css")
	 :publishing-function org-publish-attachment
	 :recursive t)
	("blog-cgi"
	 :base-directory ,(concat root-dir "cgi-bin")
	 :base-extension ".*"
	 :publishing-directory ,(concat publish-dir "cgi-bin")
	 :publishing-function org-publish-attachment
	 :recursive t)
        ("blog" :components ("blog-pages" "blog-posts" "blog-css" "blog-cgi"))))

(org-publish-project "blog" force-publish)

;; (provide 'blog)
(kill-emacs 0)
;;; blog.el ends here
