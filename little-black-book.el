;;; little-black-book --- Add my names to Org Capture Templates
;;; Commentary:

;;; I'm trying to keep a little black book on everybody.  It's tough
;;; to manage all these captures through the customize interface,
;;; though.  So let's just not do that, and instead use this code
;;; instead.

;;; Code:
(require 'org-capture)
(require 'dash)

(defcustom black-book-file "people.org"
  "The file where I keep notes on people."
  :group 'black-book
  :type 'file)

(defcustom black-book-prefix "p"
  "The prefix key for all notes on people."
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

(defun make-an-entry (key name)
  "Create a capture template on KEY for NAME."
  `(,key ,name entry
         (file+headline ,black-book-file ,name)
         "* %^{Title|A Quick Obs}
%U

%?"))

(defun format-first-letter-table-one-name (initials name-list)
  "Make a capture template for INITIALS when NAME-LIST is just one name."
  (let ((prefix (concat black-book-prefix initials))
        (label (car name-list)))
    (push (make-an-entry prefix label) *list-of-outputs*)))

(defun format-first-letter-table-multi-names (initials name-list)
  "Make capture templates for INITIALS when NAME-LIST is more than one name."
  (let ((prefix (concat black-book-prefix initials))
        (label (format "People - All %ss" (upcase initials)))
        (i 0)
        one-name)
    (while (setq i (1+ i) one-name (pop name-list))
      (push (make-an-entry (concat prefix (int-to-string i)) one-name)
            *list-of-outputs*))
    (push `(,prefix ,label) *list-of-outputs*)))

(defun format-first-letter-table (initials name-list)
  "Make capture templates for INITIALS and NAME-LIST."
  (if (null (cadr name-list))
      (format-first-letter-table-one-name initials name-list)
    (format-first-letter-table-multi-names initials name-list)))

(defun format-big-table (first-letter initials-hash)
  "Make capture templates for FIRST-LETTER and INITIALS-HASH."
  (let ((prefix (concat black-book-prefix first-letter))
        (label (format "People - %ss" (upcase first-letter))))
    (maphash 'format-first-letter-table initials-hash)
    (push `(,prefix ,label) *list-of-outputs*)))

(defun add-lbb-capture-templates ()
  "Add capture templates for names in the little black book."
  (let ((*table-of-inputs* (make-hash-table :test 'equal))
        *list-of-outputs*)
    (with-current-buffer (find-file-noselect
                          (expand-file-name black-book-file org-directory))
      (org-map-entries 'add-person-at-point-to-big-table "+LEVEL=1")
      (maphash 'format-big-table *table-of-inputs*)
      (push `(,black-book-prefix "People") *list-of-outputs*)
      (setf org-capture-templates
            (append org-capture-templates *list-of-outputs*)))))

(defun remove-lbb-capture-templates ()
  "Remove capture templates created from the little black book."
  (setf org-capture-templates
        (-remove #'(lambda (elt)
                     (equal black-book-prefix
                            (substring-no-properties (car elt) 0 1)))
                 org-capture-templates)))

(defun set-up-lbb-capture-templates ()
  "Set up little black book capture templates."
  (interactive)
  (remove-lbb-capture-templates)
  (add-lbb-capture-templates))

(provide 'little-black-book)
;;; little-black-book.el ends here
