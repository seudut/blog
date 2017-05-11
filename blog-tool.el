;;; blog-tool.el --- Some utility functions of blog  -*- lexical-binding: t; -*-

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

;; Some functions to create post/page

;;; Code:

(require 'org)

(defconst blog-root-dir (file-name-directory (buffer-file-name)))


(defun blog-tool-create-post ()
  "Create a post."
  (interactive)
  (let ((title (read-from-minibuffer "Title: "))
	(filename ""))
    (setq filename
	  (concat blog-root-dir "/posts/"
		  (replace-regexp-in-string " " "-" title)
		  ".org"))
    (find-file filename)
    (insert (concat
	     "#+TITLE: " title "\n"
	     "#+AUTHOR: " my-name "\n"
	     "#+EMAIL: " my-email "\n"
	     "#+DATE: "))
    (org-insert-time-stamp (current-time) nil nil nil "\n")
    (insert "\n")
    (save-buffer)))




(provide 'blog-tool)
;;; blog-tool.el ends here
