(load "../cppref.el")
(require 'ert)

(ert-deftest cppref-test-name-to-html:index ()
  (let* ((cppref-path-to-doc-root "../doc/reference/")
         (cppref-mapping-to-html-hash-table nil)
         (cppref-node-names nil))
    (cppref-init)
    (setq l (cppref-name-to-html cppref-dummy-key cppref-mapping-to-html-hash-table))
    (should (= 1 (length l)))
    (should
     (string=
      (expand-file-name "../doc/reference/en.cppreference.com/w/index.html")
      ;; index.html should have no corresponding class.
      (cdr (assoc cppref-dummy-key l))))))

(ert-deftest cppref-test-name-to-html:insert ()
  (let* ((root "../doc/reference")
         (tbl (make-hash-table :test #'equal)))
    (push
     `("map" . ,(expand-file-name
               "../doc/reference/en.cppreference.com/w/cpp/container/map/insert.html"))
     (gethash "insert" tbl))
    (push
     `("list" . ,(expand-file-name
                  "../doc/reference/en.cppreference.com/w/cpp/container/list/insert.html"))
     (gethash "insert" tbl))
    (should (= 2 (length (cppref-name-to-html "insert" tbl))))
    (let ((l (cppref-name-to-html "insert" tbl)))
      (should
       (string=
        (cdr (assoc "map" l))
        (expand-file-name
         "../doc/reference/en.cppreference.com/w/cpp/container/map/insert.html")))
      (should
       (string=
        (cdr (assoc "list" l))
        (expand-file-name
         "../doc/reference/en.cppreference.com/w/cpp/container/list/insert.html"))))))

(ert-deftest cppref-test-get-all-html ()
  (let ((l (cppref-get-all-html (expand-file-name "../doc/reference/"))))
    (should (> (length l) 1000))
    ;; check if all the elements in l end with .html
    (should (loop for f in l
                  do (unless (string-match "\\.html$" f) (return nil))
                  finally return t))))

(ert-deftest cppref-test-get-parent-directory ()
  (should
   (string= "html"
            (cppref-get-parent-directory "/some/path/to/html/file.html")))
  (should (string= "map"
                   (cppref-get-parent-directory
                    "/reference/en.cppreference.com/w/cpp/container/map/insert.html"))))

(ert-deftest cppref-test-get-node-name ()
  (should (string= "insert"
                   (cppref-get-node-name "/some/path/to/map/insert.html")))
  (should (null (cppref-get-node-name "/some/path/to/directory/")))
  (should (null (cppref-get-node-name "/some/path/to/file/but/not/html.txt")))
  (should (string= "copy_n"
                   (cppref-get-node-name "/some/path/to/copy_n/copy_nhtml.html"))))

(ert-deftest cppref-test-insert-html-into-table ()
  (let* ((root "../doc/reference/")
         (tbl (make-hash-table :test #'equal))
         (tbl (cppref-insert-html-into-table root tbl)))
    (should (< 2 (length
                  (gethash "insert" tbl))))))

(ert-deftest cppref-test-get-all-node-names ()
  (let* ((root "../doc/reference/")
         (tbl (make-hash-table :test #'equal))
         (tbl (cppref-insert-html-into-table root tbl))
         (names (cppref-get-all-node-names tbl)))
    (should (< 1 (length names)))
    (should (member "insert" names))
    (should (member "algorithm" names))
    (should (member "tuple" names))))

(ert-deftest cppref-test-init:check-node-names-and-mapping-hash-table ()
  (let ((cppref-path-to-doc-root "../doc/reference/")
        (cppref-mapping-to-html-hash-table (make-hash-table :test #'equal))
        (cppref-node-names nil))
    (cppref-init)
    (should (< 100 (length cppref-node-names)))
    (should (< 100 (hash-table-size cppref-mapping-to-html-hash-table)))))

(ert-deftest cppref-test-read-node-name-from-minibuffer ()
    (should (string= "insert" (cppref-read-node-name-from-minibuffer)))
    (should (string= "" (cppref-read-node-name-from-minibuffer))))

(ert-deftest cppref-test-get-path-to-visit:multiple-choices ()
  (let ((cppref-path-to-doc-root "../doc/reference/")
        (cppref-mapping-to-html-hash-table nil)
        (cppref-node-names nil))
    (cppref-init)
    (should
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/cpp/container/list/insert.html")
      (cppref-get-path-to-visit "insert" cppref-mapping-to-html-hash-table)))))

(ert-deftest cppref-test-get-path-to-visit:single-choice ()
  (let ((cppref-path-to-doc-root "../doc/reference/")
        (cppref-mapping-to-html-hash-table nil)
        (cppref-node-names nil))
    (cppref-init)
    (should
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/index.html")
      (cppref-get-path-to-visit "index" cppref-mapping-to-html-hash-table)))
    (should
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/cpp/container/map.html")
      (cppref-get-path-to-visit "map" cppref-mapping-to-html-hash-table)))))

(ert-deftest cppref-test-init-hash-table ()
  (let ((cppref-path-to-doc-root "../doc/reference/")
        (cppref-mapping-to-html-hash-table nil))
    (cppref-init-hash-table)
    (should (hash-table-p cppref-mapping-to-html-hash-table))
    (should (< 66 (hash-table-size cppref-mapping-to-html-hash-table)))
    (should (< 2 (length (gethash "insert" cppref-mapping-to-html-hash-table))))))

(ert-deftest cppref-test-init-node-names ()
  (let ((cppref-path-to-doc-root "../doc/reference/")
        (cppref-mapping-to-html-hash-table nil))
    (cppref-init)
    (should (< 10 (length cppref-node-names)))
    (should (member "insert" cppref-node-names))
    (should (member "algorithm" cppref-node-names))
    (should (member "utility" cppref-node-names))))

(ert-deftest cppref-test-get-path-to-visit:nhtml ()
  (let ((cppref-path-to-doc-root "../doc/reference/")
        (cppref-mapping-to-html-hash-table nil)
        (cppref-node-names nil))
    (cppref-init)
    (should
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/cpp/algorithm/copy_nhtml.html")
      (cppref-get-path-to-visit "copy_n" cppref-mapping-to-html-hash-table)))))
