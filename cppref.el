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

(defvar cppref-node-names nil
  "A list containing all node names, i.e. insert, remove_if,...")

;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;
(defun cppref-init (root tbl)
  "Return a hash table with its contents being `(node . (path1
path2))'."
  (let ((dir (concat root "en.cppreference.com/w/")))
    ;; index.html has no corresponding class name.
    (push `(nil . ,(expand-file-name (concat dir "index.html"))) (gethash "index" tbl))
    ;; Put all the paths to html files under the root.
    (setq cppref-mapping-to-html-hash-table
          (cppref-insert-html-into-table root tbl))
    (setq cppref-node-names (cppref-get-all-node-names tbl))))

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
  "Get all html files under the DOCROOT including its
subdirectories and return them as a list."
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
  "Return the name of the parent directory of PAHT."
  (when (and (string-match "\\.html$" path)
             (string-match ".*/\\([^/]+\\)/[^/]+\\.html$" path))
    (match-string-no-properties 1 path)))

(defun cppref-get-node-name (path)
  "Return the node name of PATH, i.e. the file name without its
extension."
  (when (string-match "\\([^/]+\\)\\.html$" path)
    (match-string-no-properties 1 path)))

(defun cppref-get-all-node-names (table)
  "Return a list of names of nodes in hash table TABLE."
  (let ((keys nil))
    (flet ((f (k v)
              (push k keys)))
      (maphash #'f table))
    (sort keys #'string<)))

(defun cppref-read-node-name-from-minibuffer (name)
  "Read from minibuffer the name to search for."
  (interactive `,(completing-read "cppref: " cppref-node-names nil t))
  (or name "index"))

(defun cppref-get-path-to-visit (name table)
  "Return a path to a html file to visit."
  (let ((lst (cppref-name-to-html name table)))
    (if (= 1 (length lst))
        (car lst)
      (progn
        (let* ((classes (sort (mapcar #'car lst) #'string<))
               (class (completing-read (format "`%s' in: " name)
                                       classes nil t)))
          (cdr (assoc class lst)))))))

(provide 'cppref)
;;; cppref.el ends here
