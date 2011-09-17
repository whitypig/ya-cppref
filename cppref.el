;;; cppref.el --- An interface using w3m with cppreference.com references on Emacs

;; Copyright (C) 2011  whitypig

;; Author: whitypig <whitypig@gmail.com>
;; Keywords: convenience, help

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

;; 

;;; Code:

(require 'w3m)

;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;
(defgroup cppref nil
  "Elisp for viewing cppreference.com references in Emacs."
  :prefix "cppref"
  :group 'convenience)

(defcustom cppref-path-to-doc-root "../doc/reference/"
  "The path name to the root directory of references."
  :type 'directory
  :group 'cppref)

;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;
(defvar cppref-mapping-to-html-hash-table (make-hash-table :test 'equal))

(defvar cppref-path-to-container "w/cpp/container/"
  "")

(defvar cppref-path-to-algorithm "w/cpp/algorithm/"
  "")

(defvar cppref-path-to-error "w/cpp/error/"
  "")

(defvar cppref-path-to-io "w/cpp/io/"
  "")

(defvar cppref-path-to-iterator "w/cpp/iterator/"
  "")

(defvar cppref-path-to-memory "w/cpp/memory/"
  "")

(defvar cppref-path-to-numberic "w/cpp/numeric/"
  "")

(defvar cppref-path-to-regex "w/cpp/regex/"
  "")

(defvar cppref-path-to-string "w/cpp/string/"
  "")

(defvar cppref-path-to-thread "w/cpp/thread/"
  "")

(defvar cppref-path-to-types "w/cpp/types/"
  "")

(defvar cppref-path-to-utility "w/cpp/utility/"
  "")

;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;
(defun cppref-init (root tbl)
  (let ((dir (concat root "en.cppreference.com/w/")))
    ;; index.html has no corresponding class name.
    (push `(nil . ,(expand-file-name (concat dir "index.html"))) (gethash "index" tbl))
    (cppref-insert-html-into-table root tbl)
    tbl))

(defun cppref-insert-html-into-table (docroot tbl)
  "DOCROOT should end with `reference/'."
  (let ((files (cppref-get-all-html docroot))
        (parent nil))
    (dolist (f files)
      (push `(,(cppref-get-parent-directory f) . ,f)
            (gethash (cppref-get-node-name f) tbl)))
    tbl))

(defun cppref-name-to-html (key tbl)
  (gethash key tbl nil))

(defun cppref-get-all-html (docroot)
  "Get all html files under the DOCROOT and return them as a
list."
  (flet ((f (l)
            (mapcan (lambda (e)
                      (if (file-directory-p e)
                          (f (directory-files e t "[^.]$"))
                        (list e)))
                    l)))
    (sort (f (directory-files (expand-file-name (concat docroot "en.cppreference.com/w/cpp/"))
                        t
                        "[^.]$"))
          #'string<)))

(defun cppref-get-parent-directory (path)
  (when (and (string-match "\\.html$" path)
             (string-match ".*/\\([^/]+\\)/[^/]+\\.html$" path))
    (match-string-no-properties 1 path)))

(defun cppref-get-node-name (path)
  (when (string-match "\\([^/]+\\)\\.html$" path)
    (match-string-no-properties 1 path)))

(provide 'cppref)
;;; cppref.el ends here
