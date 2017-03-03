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

(require 'org)
(require 'ox-publish)
(require 'htmlize)

;; To prevent inline-css when exporting html. will use external css
(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      `(
        ("org-notes"
         :base-directory "~/Private/blog/"
         :base-extension "org"
         :publishing-directory "~/Private/publish_html"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :section-numbers nil
         :auto-preamble t
         :auto-sitemap t          ;Generate sitmap.org automagicaly...
         :sitemap-filename "sitemap.org" ;... call it sitemap.org (it's the default )...
         :sitemap-title "Sitemap"

         :html-table-of-contents nil
         :html-postamble nil ;dont export creator auto validation info in html postamble div
         :html-link-home "/"
         :html-head "<link rel='stylesheet' href='/Users/peli3/Private/blog/res/worg.css' />"
         :html-head-include-default-style nil
         :html-head-include-scripts nil)
        ("org-static"
         :base-directory "~/Private/blog/"
         :base-extension "css\\|js\\|png\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Private/publish_html"
         :recursive t
         :publishing-function org-publish-attachment
         :table-of-contents nil)
        ("org" :components ("org-notes" "org-static"))))


(provide 'blog)
;;; blog.el ends here
