;;; ya-cppref.el --- An interface using w3m with cppreference.com references on Emacs

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

;; Prerequisites:
;; To use ya-cppref.el, you need to have w3m and emacs-w3m installed
;; in your system.  You need also to get cppreference.com reference,
;; inflating it, and put it in your system.  The document of
;; cppreference.com is available on
;; http://en.cppreference.com/w/Cppreference:Archives

;; Installation:
;; Put ya-cppref.el in the directory which is included in load path,
;; and add the following to your emacs setting file:
;; (require 'ya-cppref)
;;
;; Also you need to specify the root directory of documents in your
;; emacs setting file.
;; The below is a few examples.
;; If you place the documents in /usr/local/doc/cpp, then
;; (setq ya-cppref-path-to-doc-root "/usr/local/doc/cpp/reference/")
;;
;; If you place the documents in ~/doc/cppreference, then
;; (setq ya-cppref-path-to-doc-root "~/doc/cppreference/reference/")
;;
;; Please note that the path should end with `/', and the reference
;; directory above is the root direcotry in the zip file.
;;
;; Usage scenario:
;; * Searching for the doc about vector::push_back
;; M-x ya-cppref [RET]
;; ya-cppref: push_back [RET]    ; type push_back
;; `push_back' in: vector [RET]  ; choose class.
;;
;; * Searching for the doc about list
;; M-x ya-cppref [RET]
;; ya-cppref: list [RET]         ; type list
;; `list' in: container [RET]    ; choose container

;; Note that when you type function names or class names, completion
;; is available.

;; I hope this will help. Thanks.

;;; Code:

(require 'w3m)

;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;
(defgroup ya-cppref nil
  "Elisp for viewing cppreference.com references in Emacs."
  :prefix "ya-cppref"
  :group 'convenience)

(defcustom ya-cppref-path-to-doc-root nil
  "The path name to the root directory of references.
This should end with `reference/'."
  :type 'directory
  :group 'ya-cppref)

(defcustom ya-cppref-split-window-horizontal t
  "Specify which way to split window when opening a reference.
If you want to split the window vertically, set this variable
`nil'. The default is `t'"
  :type 'boolean
  :group 'ya-cppref)


;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;
(defvar ya-cppref-mapping-to-html-hash-table (make-hash-table :test 'equal)
  "node => ((class1 . path1) (class2 . path2)...")

(defvar ya-cppref-node-names nil
  "A list containing all node names, i.e. insert, remove_if,...")

(defvar ya-cppref-dummy-key "ya-cppref-index")

(defconst ya-cppref-en-docroot "en.cppreference.com/w/")
(defconst ya-cppref-en-cpp-docroot (concat ya-cppref-en-docroot "cpp/"))

;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;
(defun ya-cppref-init ()
  "Return a hash table with its contents being `(node . (path1
path2))'."
  (let ((dir (concat ya-cppref-path-to-doc-root ya-cppref-en-docroot)))
    ;; Put all the paths to html files under the root.
    (ya-cppref-init-hash-table)
    (ya-cppref-init-node-names)
    ya-cppref-mapping-to-html-hash-table))

(defun ya-cppref-init-hash-table ()
  (unless (and ya-cppref-mapping-to-html-hash-table
               (hash-table-p ya-cppref-mapping-to-html-hash-table)
               (< 66 (hash-table-size ya-cppref-mapping-to-html-hash-table)))
    (let* ((dir (concat ya-cppref-path-to-doc-root ya-cppref-en-docroot))
           (index-path (expand-file-name (concat dir "index.html"))))
      (setq ya-cppref-mapping-to-html-hash-table (make-hash-table :test #'equal))
      (push `(,ya-cppref-dummy-key . ,index-path)
            (gethash ya-cppref-dummy-key ya-cppref-mapping-to-html-hash-table))
      (push `("index" . ,index-path)
            (gethash "index" ya-cppref-mapping-to-html-hash-table))
      (setq ya-cppref-mapping-to-html-hash-table
            (ya-cppref-insert-html-into-table
             ya-cppref-path-to-doc-root
             ya-cppref-mapping-to-html-hash-table)))))

(defun ya-cppref-insert-html-into-table (docroot tbl)
  "DOCROOT should end with `reference/'."
  (let ((files (ya-cppref-get-all-html docroot)))
    (dolist (f files)
      (push `(,(ya-cppref-get-parent-directory f) . ,f)
            (gethash (ya-cppref-get-node-name f) tbl)))
    tbl))

(defun ya-cppref-init-node-names ()
  (unless (and ya-cppref-node-names
               (listp ya-cppref-node-names)
               (< 2 (length ya-cppref-node-names)))
    (setq ya-cppref-node-names
          (ya-cppref-get-all-node-names ya-cppref-mapping-to-html-hash-table))))

(defun ya-cppref-name-to-html (key tbl)
  (let ((k (if (or (string= "index" key)
                   (string= "" key))
               ya-cppref-dummy-key
             key)))
    (gethash k tbl nil)))

(defun ya-cppref-get-all-html (docroot)
  "Get all html files under the DOCROOT including its
subdirectories and return them as a list."
  (flet ((f (l)
            (mapcan (lambda (e)
                      (if (file-directory-p e)
                          (f (directory-files e t "[^.]$"))
                        (list e)))
                    l)))
    (sort (f (directory-files (expand-file-name (concat docroot ya-cppref-en-cpp-docroot))
                        t
                        "[^.]$"))
          #'string<)))

(defun ya-cppref-get-parent-directory (path)
  "Return the name of the parent directory of PAHT."
  (when (and (string-match "\\.html$" path)
             (string-match ".*/\\([^/]+\\)/[^/]+\\.html$" path))
    (match-string-no-properties 1 path)))

(defun ya-cppref-get-node-name (path)
  "Return the node name of PATH, i.e. the file name without its
extension."
  (when (string-match "\\([^/]+\\)\\.html$" path)
    ;; Remove the trailing html if any.
    (replace-regexp-in-string "html$" ""
                              (match-string-no-properties 1 path))))

(defun ya-cppref-get-all-node-names (table)
  "Return a list of names of nodes in hash table TABLE."
  (let ((keys nil))
    (flet ((f (k v)
              (push k keys)))
      (maphash #'f table))
    (sort keys #'string<)))

(defun ya-cppref-read-node-name-from-minibuffer (&optional name)
  "Read from minibuffer the name to search for."
  (let ((name (or name (completing-read "ya-cppref: " ya-cppref-node-names nil t))))
    (if (or (null name) (string= "index" name))
        ya-cppref-dummy-key
      name)))

(defun ya-cppref-get-path-to-visit (name table)
  "Return a path to a html file to visit."
  (let ((lst (ya-cppref-name-to-html name table)))
    (cond
     ((null lst) (message "Sorry, but no entry found"))
     ((or (string= "index" name)
          (string= ya-cppref-dummy-key name))
      (cdr (assoc ya-cppref-dummy-key lst)))
     ((= 1 (length lst)) (cdar lst))
     (t
      (let* ((classes (sort (mapcar #'car lst) #'string<))
             (class (completing-read (format "`%s' in: " name)
                                     classes nil t)))
        (cdr (assoc class lst)))))))

(defun ya-cppref-clear ()
  (interactive)
  (setq ya-cppref-mapping-to-html-hash-table nil
        ya-cppref-node-names nil))

(defun ya-cppref ()
  (interactive)
  (let ((path nil) (name nil))
    (ya-cppref-init)
    (setq name (ya-cppref-read-node-name-from-minibuffer))
    (setq path (ya-cppref-get-path-to-visit
                name
                ya-cppref-mapping-to-html-hash-table))
    (ya-cppref-visit-reference path)))


(defun ya-cppref-visit-reference (path)
  (let ((w (get-buffer-window "*w3m*")))
    (unless w
      (if (one-window-p)
          (setq w (split-window nil nil ya-cppref-split-window-horizontal))
        (setq w (next-window))))
    (select-window w)
    (w3m-find-file path)
    (ignore-errors
      (goto-char (point-min))
      (goto-char (or (save-excursion (re-search-forward "^Defined in" nil t))
                     (save-excursion (search-forward (concat name "(") nil t))
                     (save-excursion (re-search-forward "^Member functions" nil t))
                     (save-excursion (re-search-forward "^Contents" nil t)))))))

(provide 'ya-cppref)
;;; ya-cppref.el ends here
