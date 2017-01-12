;;; test-lbb --- Unit tests for my little black book library

;;; Commentary:
;; Every library needs a test suite.  And it's kind of fun to learn.

;;; Code:
(require 'ert)
(require 'little-black-book)

(ert-deftest lbb-test-build-one-input ()
  "Test parsing of an org headline into the hash table format."
  (let ((expected-table
         #s(hash-table
            test equal
            data ("j" #s(hash-table
                         test equal
                         data ("jm" ("Joseph Marquez"))))))
        (actual-table
         (with-temp-buffer
           (insert "* Joseph Marquez\n")
           (org-mode)
           (lbb-build-table-of-inputs (current-buffer)))))
    (should (hash-table-equal expected-table actual-table))))

(ert-deftest lbb-test-format-one-capture ()
  "Test formatting just one capture template."
  (let* ((expected-list
          `((,black-book-capture-prefix "People")
            (,(concat black-book-capture-prefix "j") "People - Js")
            (,(concat black-book-capture-prefix "jm")
             "Joseph Marquez" entry
             (file+headline ,black-book-file "Joseph Marquez")
             "* %^{Title|A Quick Obs}\n%U\n\n%?")))
         (actual-list
          (lbb-build-list-of-captures
           #s(hash-table
              test equal
              data ("j" #s(hash-table
                           test equal
                           data ("jm" ("Joseph Marquez"))))))))
    (should (equal expected-list actual-list))))

(ert-deftest lbb-test-format-one-agenda ()
  "Test formatting just one agenda template."
  (let* ((expected-list
          `((,black-book-agenda-prefix . "People")
            (,(concat black-book-agenda-prefix "j") . "People - Js")
            (,(concat black-book-agenda-prefix "jm")
             "Joseph Marquez" tags "josephmarquez+chat"
             ((org-agenda-files '(,black-book-file))))))
         (actual-list
          (lbb-build-list-of-agendas
           #s(hash-table
              test equal
              data ("j" #s(hash-table
                           test equal
                           data ("jm" ("Joseph Marquez"))))))))
    (should (equal expected-list actual-list))))

(ert-deftest lbb-test-build-two-inputs ()
  "Test parsing of two org headlines into the hash table format."
  (let ((expected-table
         #s(hash-table
            test equal
            data ("j" #s(hash-table
                         test equal
                         data ("jm" ("Joseph Marquez")))
                  "a" #s(hash-table
                         test equal
                         data ("al" ("Adam Lindgren"))))))
        (actual-table
         (with-temp-buffer
           (insert "* Joseph Marquez\n")
           (insert "* Adam Lindgren\n")
           (org-mode)
           (lbb-build-table-of-inputs (current-buffer)))))
    (should (hash-table-equal expected-table actual-table))))

(ert-deftest lbb-test-format-two-captures ()
  "Test formatting two capture templates."
  (let* ((expected-list
          `((,black-book-capture-prefix "People")
            (,(concat black-book-capture-prefix "a") "People - As")
            (,(concat black-book-capture-prefix "al")
             "Adam Lindgren" entry
             (file+headline ,black-book-file "Adam Lindgren")
             "* %^{Title|A Quick Obs}\n%U\n\n%?")
            (,(concat black-book-capture-prefix "j") "People - Js")
            (,(concat black-book-capture-prefix "jm")
             "Joseph Marquez" entry
             (file+headline ,black-book-file "Joseph Marquez")
             "* %^{Title|A Quick Obs}\n%U\n\n%?")))
         (actual-list
          (lbb-build-list-of-captures
           #s(hash-table
              test equal
              data ("j" #s(hash-table
                           test equal
                           data ("jm" ("Joseph Marquez")))
                    "a" #s(hash-table
                           test equal
                           data ("al" ("Adam Lindgren"))))))))
    (should (equal expected-list actual-list))))

(ert-deftest lbb-test-format-two-agendas ()
  "Test formatting two agenda templates."
  (let* ((expected-list
          `((,black-book-agenda-prefix . "People")
            (,(concat black-book-agenda-prefix "a") . "People - As")
            (,(concat black-book-agenda-prefix "al")
             "Adam Lindgren" tags "adamlindgren+chat"
             ((org-agenda-files '(,black-book-file))))
            (,(concat black-book-agenda-prefix "j") . "People - Js")
            (,(concat black-book-agenda-prefix "jm")
             "Joseph Marquez" tags "josephmarquez+chat"
             ((org-agenda-files '(,black-book-file))))))
         (actual-list
          (lbb-build-list-of-agendas
           #s(hash-table
              test equal
              data ("j" #s(hash-table
                           test equal
                           data ("jm" ("Joseph Marquez")))
                    "a" #s(hash-table
                           test equal
                           data ("al" ("Adam Lindgren"))))))))
    (should (equal expected-list actual-list))))

(ert-deftest lbb-test-build-two-inputs-same-initials ()
  "Test parsing of two org headlines into the hash table format."
  (let ((expected-table
         #s(hash-table
            test equal
            data ("j" #s(hash-table
                         test equal
                         data ("jm" ("Just Mango" "Joseph Marquez"))))))
        (actual-table
         (with-temp-buffer
           (insert "* Joseph Marquez\n")
           (insert "* Just Mango\n")
           (org-mode)
           (lbb-build-table-of-inputs (current-buffer)))))
    (should (hash-table-equal expected-table actual-table))))

(ert-deftest lbb-test-format-two-captures-same-initials ()
  "Test formatting two capture templates."
  (let* ((expected-list
          `((,black-book-capture-prefix "People")
            (,(concat black-book-capture-prefix "j") "People - Js")
            (,(concat black-book-capture-prefix "jm") "People - All JMs")
            (,(concat black-book-capture-prefix "jm2")
             "Joseph Marquez" entry
             (file+headline ,black-book-file "Joseph Marquez")
             "* %^{Title|A Quick Obs}\n%U\n\n%?")
            (,(concat black-book-capture-prefix "jm1")
             "Just Mango" entry
             (file+headline ,black-book-file "Just Mango")
             "* %^{Title|A Quick Obs}\n%U\n\n%?")
            ))
         (actual-list
          (lbb-build-list-of-captures
           #s(hash-table
              test equal
              data ("j" #s(hash-table
                           test equal
                           data ("jm" ("Just Mango" "Joseph Marquez"))))))))
    (should (equal expected-list actual-list))))

(ert-deftest lbb-test-format-two-agendas-same-initials ()
  "Test formatting two agenda templates."
  (let* ((expected-list
          `((,black-book-agenda-prefix . "People")
            (,(concat black-book-agenda-prefix "j") . "People - Js")
            (,(concat black-book-agenda-prefix "jm") . "People - All JMs")
            (,(concat black-book-agenda-prefix "jm2")
             "Joseph Marquez" tags "josephmarquez+chat"
             ((org-agenda-files '(,black-book-file))))
            (,(concat black-book-agenda-prefix "jm1")
             "Just Mango" tags "justmango+chat"
             ((org-agenda-files '(,black-book-file))))))
         (actual-list
          (lbb-build-list-of-agendas
           #s(hash-table
              test equal
              data ("j" #s(hash-table
                           test equal
                           data ("jm" ("Just Mango" "Joseph Marquez"))))))))
    (should (equal expected-list actual-list))))

(provide 'test-lbb)
;;; test-lbb.el ends here
