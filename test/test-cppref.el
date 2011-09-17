(load "../cppref.el")
(require 'ert)

(ert-deftest cppref-test-name-to-html:index ()
  (let* ((root "../doc/reference/")
         (tbl (make-hash-table :test 'equal))
         (tbl (cppref-init root tbl))
         (l (cppref-name-to-html "index" tbl)))
    (should (= 1 (length l)))
    (should
     (string=
      (expand-file-name "../doc/reference/en.cppreference.com/w/index.html")
      ;; index.html should have no corresponding class.
      (cdr (assoc nil (cppref-name-to-html "index" tbl)))))))

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
  (should (null (cppref-get-node-name "/some/path/to/file/but/not/html.txt"))))

(ert-deftest cppref-test-insert-html-into-table ()
  (let* ((root "../doc/reference/")
         (tbl (make-hash-table :test #'equal))
         (tbl (cppref-insert-html-into-table root tbl)))
    (should (< 2 (length
                  (gethash "insert" tbl))))))