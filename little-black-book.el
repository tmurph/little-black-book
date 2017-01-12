;;; little-black-book --- Add my names to Org Captures and Agendas

;; Copyright (C) 2017  Trevor Murphy

;; Author: Trevor Murphy <trevor.m.murphy@gmail.com>
;; Maintainer: Trevor Murphy <trevor.m.murphy@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I'm trying to keep a little black book on everybody.  It's tough to
;; manage capture templates / agenda commands through the customize
;; interface, though.  So let's use code instead.

;;; Code:

(require 'org-capture)
(require 'dash)
(require 'ert)

(defcustom black-book-file "people.org"
  "The file where I keep notes on people."
  :group 'black-book
  :type 'file)

(defcustom black-book-capture-prefix "p"
  "The prefix key for all capture templates."
  :group 'black-book
  :type 'string)

(defcustom black-book-agenda-prefix "p"
  "The prefix key for all agenda commands."
  :group 'black-book
  :type 'string)

(defvar *table-of-inputs* (make-hash-table :test 'equal)
  "Use a dynamically scoped variable to keep track of inputs.")

(defvar *list-of-outputs* nil
  "Use a dynamically scoped variable to keep track of outputs.")

(defun get-first-letter (string)
  "Get the (lowercase) first letter of STRING."
  (downcase (substring-no-properties string 0 1)))

(defun get-first-initials (string)
  "Get the (lowercase) first initials of all words in STRING."
  (mapconcat 'get-first-letter (split-string string) ""))

(defun get-or-make-hash (key table)
  "Use RAII to associate KEY with a hash table in TABLE."
  (or (gethash key table)
      (puthash key (make-hash-table :test 'equal) table)))

(defun get-or-make-list (key table)
  "Use RAII to associate KEY with a list in TABLE."
  (or (gethash key table)
      (puthash key nil table)))

(defun get-agenda-search-string (name)
  "Get the tag search string for NAME."
  (concat (mapconcat 'downcase (split-string name) "")
          "+chat"))

(defun add-person-at-point-to-big-table ()
  "Parse the person (headline) at point into global variable *big-table*."
  (let* ((name (org-element-property :title (org-element-at-point)))
         (first-initial (get-first-letter name))
         (first-hash (get-or-make-hash first-initial *table-of-inputs*))
         (initials (get-first-initials name))
         (initials-list (get-or-make-list initials first-hash))
         (new-initials-list (cons name initials-list)))
    (puthash initials new-initials-list first-hash)
    (puthash first-initial first-hash *table-of-inputs*)))

(defun make-a-capture-entry (key name)
  "Create a capture template on KEY for NAME."
  `(,key ,name entry
         (file+headline ,black-book-file ,name)
         "* %^{Title|A Quick Obs}
%U

%?"))

(defun make-an-agenda-entry (key name)
  "Create an agenda command on KEY for NAME."
  `(,key ,name tags ,(get-agenda-search-string name)
         ((org-agenda-files '(,black-book-file)))))

(defun make-a-capture-prefix (key label)
  "Create a prefix capture template on KEY using LABEL."
  (list key label))

(defun make-an-agenda-prefix (key label)
  "Create a prefix agenda command on KEY using LABEL."
  (cons key label))

(defun format-first-letter-table-one-name-for-captures (initials name-list)
  "Make a capture template for INITIALS when NAME-LIST is just one name."
  (let ((prefix (concat black-book-capture-prefix initials))
        (label (car name-list)))
    (push (make-a-capture-entry prefix label) *list-of-outputs*)))

(defun format-first-letter-table-one-name-for-agendas (initials name-list)
  "Make an agenda command for INITIALS when NAME-LIST is just one name."
  (let ((prefix (concat black-book-agenda-prefix initials))
        (label (car name-list)))
    (push (make-an-agenda-entry prefix label) *list-of-outputs*)))

(defun format-first-letter-table-multi-names-for-captures (initials name-list)
  "Make capture templates for INITIALS when NAME-LIST is more than one name."
  (let ((prefix (concat black-book-capture-prefix initials))
        (label (format "People - All %ss" (upcase initials)))
        (i 0)
        one-name)
    (while (setq i (1+ i) one-name (pop name-list))
      (push (make-a-capture-entry (concat prefix (int-to-string i)) one-name)
            *list-of-outputs*))
    (push (make-a-capture-prefix prefix label) *list-of-outputs*)))

(defun format-first-letter-table-multi-names-for-agendas (initials name-list)
  "Make agenda commands for INITIALS when NAME-LIST is more than one name."
  (let ((prefix (concat black-book-agenda-prefix initials))
        (label (format "People - All %ss" (upcase initials)))
        (i 0)
        one-name)
    (while (setq i (1+ i) one-name (pop name-list))
      (push (make-an-agenda-entry (concat prefix (int-to-string i)) one-name)
            *list-of-outputs*))
    (push (make-an-agenda-prefix prefix label) *list-of-outputs*)))

(defun format-first-letter-table-for-captures (initials name-list)
  "Make capture templates for INITIALS and NAME-LIST."
  (if (null (cadr name-list))
      (format-first-letter-table-one-name-for-captures initials name-list)
    (format-first-letter-table-multi-names-for-captures initials name-list)))

(defun format-first-letter-table-for-agendas (initials name-list)
  "Make agenda commands for INITIALS and NAME-LIST."
  (if (null (cadr name-list))
      (format-first-letter-table-one-name-for-agendas initials name-list)
    (format-first-letter-table-multi-names-for-agendas initials name-list)))

(defun format-big-table-for-captures (first-letter initials-hash)
  "Make capture templates for FIRST-LETTER and INITIALS-HASH."
  (let ((prefix (concat black-book-capture-prefix first-letter))
        (label (format "People - %ss" (upcase first-letter))))
    (maphash 'format-first-letter-table-for-captures initials-hash)
    (push (make-a-capture-prefix prefix label) *list-of-outputs*)))

(defun format-big-table-for-agendas (first-letter initials-hash)
  "Make agenda commands for FIRST-LETTER and INITIALS-HASH."
  (let ((prefix (concat black-book-agenda-prefix first-letter))
        (label (format "People - %ss" (upcase first-letter))))
    (maphash 'format-first-letter-table-for-agendas initials-hash)
    (push (make-an-agenda-prefix prefix label) *list-of-outputs*)))

(defun hash-table-equal (h1 h2)
  "Return t if H1 and H2 are equal, nil otherwise."
  (and (= (hash-table-count h1) (hash-table-count h2))
       (catch 'flag
         (maphash #'(lambda (k v1)
                      (let ((v2 (gethash k h2))
                            to-throw)
                        (cond
                         ((hash-table-p v1)
                          (setq to-throw (hash-table-equal v1 v2)))
                         (t
                          (setq to-throw (equal v1 v2))))
                        (throw 'flag to-throw))) h1)
         (throw 'flag t))))

(defun lbb-build-table-of-inputs (buf)
  "Parse headlines in BUF, and return a hash table."
  (let ((*table-of-inputs* (make-hash-table :test 'equal)))
    (with-current-buffer buf
      (org-map-entries 'add-person-at-point-to-big-table "+LEVEL=1"))
    *table-of-inputs*))

(defun lbb-build-list-of-captures (table)
  "Format TABLE entries into capture output format."
  (let (*list-of-outputs*)
    (maphash 'format-big-table-for-captures table)
    (push (make-a-capture-prefix black-book-capture-prefix "People")
          *list-of-outputs*)
    *list-of-outputs*))

(defun lbb-build-list-of-agendas (table)
  "Format TABLE entries into agenda command format."
  (let (*list-of-outputs*)
    (maphash 'format-big-table-for-agendas table)
    (push (make-an-agenda-prefix black-book-agenda-prefix "People")
          *list-of-outputs*)
    *list-of-outputs*))

(defun lbb-add-capture-templates ()
  "Add capture templates for names in the little black book."
  (setf org-capture-templates
        (append org-capture-templates
                (lbb-build-list-of-captures
                 (lbb-build-table-of-inputs
                  (find-file-noselect
                   (expand-file-name black-book-file org-directory)))))))

(defun lbb-add-agenda-commands ()
  "Add agenda commands for names in the little black book."
  (setf org-agenda-custom-commands
        (append org-agenda-custom-commands
                (lbb-build-list-of-agendas
                 (lbb-build-table-of-inputs
                  (find-file-noselect
                   (expand-file-name black-book-file org-directory)))))))

(defun lbb-remove-capture-templates ()
  "Remove capture templates created from the little black book."
  (setf org-capture-templates
        (-remove #'(lambda (elt)
                     (equal black-book-capture-prefix
                            (substring-no-properties (car elt) 0 1)))
                 org-capture-templates)))

(defun lbb-remove-agenda-commands ()
  "Remove agenda commands created from the little black book."
  (setf org-agenda-custom-commands
        (-remove #'(lambda (elt)
                     (equal black-book-agenda-prefix
                            (substring-no-properties (car elt) 0 1)))
                 org-agenda-custom-commands)))

(defun lbb-set-up-capture-templates ()
  "Set up little black book capture templates."
  (interactive)
  (lbb-remove-capture-templates)
  (lbb-add-capture-templates))

(defun lbb-set-up-agenda-commands ()
  "Set up little black book agenda commands."
  (interactive)
  (lbb-remove-agenda-commands)
  (lbb-add-agenda-commands))

;;; testing

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

(provide 'little-black-book)
;;; little-black-book.el ends here
