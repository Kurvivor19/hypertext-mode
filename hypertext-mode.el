;;; hypertext-mode.el --- support for writing interlinking data

;; Copyright @ 2017 Ivan Truskov <trus19@gmail.com>

;; URL: ?

(require 'dash)
(require 'hypertext-prop-edit "./hypertext-prop-edit.el")

(defvar hypertext-directories-cache nil
  "A hashmap used to distinguish between all the directories that have different contexts")

(defvar hypertext-context-cache nil
  "A hashmap that contains contexts for the currently active hypertext projects")

(defconst hypertext-data-file-name ".hypertext"
  "Name for the file used to hold values")

(defconst hypertext-node-types '("TRANSCRIPT"
                                 "RAW"
                                 "BIO"
                                 "INFO"
                                 "LEAF"))

(defun hypertext-scan-directories-upward (dirname)
  "Check given directory and those upward of it in the file tree for .hypertext file"
  (setq curdir (abbreviate-file-name dirname))
  (let ((root nil)
        try)
    (while (not (or root
                    (null curdir)
                    (string-match locate-dominating-stop-dir-regexp curdir)))
      (setq try (file-exists-p (expand-file-name hypertext-data-file-name curdir)))
      (cond (try (setq root curdir))
            ((equal curdir (setq curdir (file-name-directory
                                         (directory-file-name curdir))))
             (setq curdir nil))))
    (and root (expand-file-name (file-name-as-directory root)))))

(defun hypertext-find-root ()
  "Locate folder containing the data file"
  (let* ((dir default-directory)
         (cache-value (gethash dir hypertext-directories-cache)))
    (if cache-value
        cache-value
      (or (hypertext-scan-directories-upward dir) dir))))

(defun hypertext-find-root-cache ()
  (or (gethash default-directory hypertext-directories-cache)
      (puthash default-directory (hypertext-find-root) hypertext-directories-cache)))

(defun hypertext-save-context (&optional root context)
  "Save given context for a given root, values not provided are retrieved from cache
If values are not present in cache, nothing is saved"
  (let* ((real-root (or root (gethash default-directory hypertext-directories-cache)))
         (real-context (or context (gethash real-root hypertext-context-cache))))
    (if (and real-root real-context)
        (with-temp-file (expand-file-name hypertext-data-file-name real-root)
          (insert (let (print-length) (prin1-to-string real-context))))
      (message "No context found for %s" real-root))))

(defun hypertext-load-context (root)
  "Read context from .hypertext file in ROOT directory; return NIL otherwise"
  (with-demoted-errors
      "Error during file deserialization: %S"
    (let ((filename (expand-file-name hypertext-data-file-name root)))
      (when (file-exists-p filename)
        (with-temp-buffer
          (insert-file-contents filename)
          ;; this will blow up if the contents of the file aren't
          ;; lisp data structures
          (puthash root (read (buffer-string)) hypertext-context-cache))))))

(defun hypertext-activate ()
  "Entry function
Finds and loads specific values for this directory"
  (interactive)
  ;; after this call we definitely have something in that folder
  (when (null hypertext-directories-cache)
    (setq hypertext-directories-cache (make-hash-table :test 'equal)))
  (when (null hypertext-context-cache)
    (setq hypertext-context-cache (make-hash-table :test 'equal)))
  (let ((root (hypertext-find-root-cache)))
    (unless (gethash root hypertext-context-cache)
      (unless (hypertext-load-context root)
        (hpe/hypertext-edit-properties
         ((:author "Author")
          (:project_name "Project name")
          (:description "Description"))
         ;; outside variables are no longer available
         (let ((root (hypertext-find-root-cache))
               (new-context `((:author . ,:author)
                              (:project_name . ,:project_name)
                              (:description . ,:description)
                              (:created . ,(format-time-string "%F %R" (current-time)))
                              (:lastentry . 0))))
            (puthash root new-context hypertext-context-cache)
            (hypertext-save-context root new-context)))))))

(defmacro hypertext-with-context (&rest body)
  `(let* ((root (hypertext-find-root-cache))
          (context (gethash root hypertext-context-cache)))
     (if (null context)
         (message "No context available")
       ,@body)))

(defun hypertext-insert-new-node ()
  "Insert new org node into org document"
  (interactive)
  (hypertext-with-context
   ;; go to the begiiing of next heading
   (org-forward-heading-same-level 1)
   (save-current-buffer
     (set-buffer (get-buffer-create "*hypertext*"))
     (erase-buffer)
     ;; prompt for the header
     (let ((heading (read-string "Input heading: "))
           (outline-regexp "\\*+ ")
           (last-index (assoc :lastentry context))
           (type (completing-read "Type of the info node: " hypertext-node-types nil 'confirm)))
       (when (and heading type)
         (insert (format "* %s\n" heading))
         (org-set-property "ADDED" (format-time-string "%d-%m-%Y %H:%M"))
         (org-set-property "INDEX" (number-to-string (cdr last-index)))
         (setcdr last-index (1+ (cdr last-index)))
         (hypertext-save-context root context)
         (org-set-property "TYPE" type))))
   (insert-buffer "*hypertext*")))
