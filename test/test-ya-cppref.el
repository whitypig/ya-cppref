(load-file "../ya-cppref.el")
(require 'ert)

(ert-deftest ya-cppref-test-name-to-html:index ()
  (let* ((ya-cppref-path-to-doc-root "../doc/reference/")
         (ya-cppref-mapping-to-html-hash-table nil)
         (ya-cppref-node-names nil))
    (ya-cppref-init)
    (setq l (ya-cppref-name-to-html ya-cppref-dummy-key ya-cppref-mapping-to-html-hash-table))
    (should (= 1 (length l)))
    (should
     (string=
      (expand-file-name "../doc/reference/en.cppreference.com/w/index.html")
      ;; index.html should have no corresponding class.
      (cdr (assoc ya-cppref-dummy-key l))))))

(ert-deftest ya-cppref-test-name-to-html:insert ()
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
    (should (= 2 (length (ya-cppref-name-to-html "insert" tbl))))
    (let ((l (ya-cppref-name-to-html "insert" tbl)))
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

(ert-deftest ya-cppref-test-get-all-html ()
  (let ((l (ya-cppref-get-all-html (expand-file-name "../doc/reference/"))))
    (should (> (length l) 1000))
    ;; check if all the elements in l end with .html
    (should (loop for f in l
                  do (unless (string-match "\\.html$" f) (return nil))
                  finally return t))))

(ert-deftest ya-cppref-test-get-parent-directory ()
  (should
   (string= "html"
            (ya-cppref-get-parent-directory "/some/path/to/html/file.html")))
  (should (string= "map"
                   (ya-cppref-get-parent-directory
                    "/reference/en.cppreference.com/w/cpp/container/map/insert.html"))))

(ert-deftest ya-cppref-test-get-node-name ()
  (should (string= "insert"
                   (ya-cppref-get-node-name "/some/path/to/map/insert.html")))
  (should (null (ya-cppref-get-node-name "/some/path/to/directory/")))
  (should (null (ya-cppref-get-node-name "/some/path/to/file/but/not/html.txt")))
  (should (string= "copy_n"
                   (ya-cppref-get-node-name "/some/path/to/copy_n/copy_nhtml.html"))))

(ert-deftest ya-cppref-test-insert-html-into-table ()
  (let* ((root "../doc/reference/")
         (tbl (make-hash-table :test #'equal))
         (tbl (ya-cppref-insert-html-into-table root tbl)))
    (should (< 2 (length
                  (gethash "insert" tbl))))))

(ert-deftest ya-cppref-test-get-all-node-names ()
  (let* ((root "../doc/reference/")
         (tbl (make-hash-table :test #'equal))
         (tbl (ya-cppref-insert-html-into-table root tbl))
         (names (ya-cppref-get-all-node-names tbl)))
    (should (< 1 (length names)))
    (should (member "insert" names))
    (should (member "algorithm" names))
    (should (member "tuple" names))))

(ert-deftest ya-cppref-test-init:check-node-names-and-mapping-hash-table ()
  (let ((ya-cppref-path-to-doc-root "../doc/reference/")
        (ya-cppref-mapping-to-html-hash-table (make-hash-table :test #'equal))
        (ya-cppref-node-names nil))
    (ya-cppref-init)
    (should (< 100 (length ya-cppref-node-names)))
    (should (< 100 (hash-table-size ya-cppref-mapping-to-html-hash-table)))))

(ert-deftest ya-cppref-test-read-node-name-from-minibuffer ()
    (should (string= "insert" (ya-cppref-read-node-name-from-minibuffer)))
    (should (string= "" (ya-cppref-read-node-name-from-minibuffer))))

(ert-deftest ya-cppref-test-get-path-to-visit:multiple-choices ()
  (let ((ya-cppref-path-to-doc-root "../doc/reference/")
        (ya-cppref-mapping-to-html-hash-table nil)
        (ya-cppref-node-names nil))
    (ya-cppref-init)
    (should
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/cpp/container/list/insert.html")
      (ya-cppref-get-path-to-visit "insert" ya-cppref-mapping-to-html-hash-table)))))

(ert-deftest ya-cppref-test-get-path-to-visit:single-choice ()
  (let ((ya-cppref-path-to-doc-root "../doc/reference/")
        (ya-cppref-mapping-to-html-hash-table nil)
        (ya-cppref-node-names nil))
    (ya-cppref-init)
    (should
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/index.html")
      (ya-cppref-get-path-to-visit "index" ya-cppref-mapping-to-html-hash-table)))
    (should
     ;; Enter `container'.
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/cpp/container/map.html")
      (ya-cppref-get-path-to-visit "map" ya-cppref-mapping-to-html-hash-table)))))

(ert-deftest ya-cppref-test-init-hash-table ()
  (let ((ya-cppref-path-to-doc-root "../doc/reference/")
        (ya-cppref-mapping-to-html-hash-table nil))
    (ya-cppref-init-hash-table)
    (should (hash-table-p ya-cppref-mapping-to-html-hash-table))
    (should (< 66 (hash-table-size ya-cppref-mapping-to-html-hash-table)))
    (should (< 2 (length (gethash "insert" ya-cppref-mapping-to-html-hash-table))))))

(ert-deftest ya-cppref-test-init-node-names ()
  (let ((ya-cppref-path-to-doc-root "../doc/reference/")
        (ya-cppref-mapping-to-html-hash-table nil))
    (ya-cppref-init)
    (should (< 10 (length ya-cppref-node-names)))
    (should (member "insert" ya-cppref-node-names))
    (should (member "algorithm" ya-cppref-node-names))
    (should (member "utility" ya-cppref-node-names))))

(ert-deftest ya-cppref-test-get-path-to-visit:nhtml ()
  (let ((ya-cppref-path-to-doc-root "../doc/reference/")
        (ya-cppref-mapping-to-html-hash-table nil)
        (ya-cppref-node-names nil))
    (ya-cppref-init)
    (should
     (string=
      (expand-file-name
       "../doc/reference/en.cppreference.com/w/cpp/algorithm/copy_nhtml.html")
      (ya-cppref-get-path-to-visit "copy_n" ya-cppref-mapping-to-html-hash-table)))))
