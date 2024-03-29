;;; hypertext-mode.el --- support for writing interlinking data

;; Copyright @ 2017 Ivan Truskov <trus19@gmail.com>

;; URL: ?

(require 'dash)
(load-file (expand-file-name "./hypertext-prop-edit.el"))
(require 'hypertext-prop-edit)

(defvar hypertext-directories-cache nil
  "A hashmap used to distinguish between all the directories that have different contexts")

(defvar hypertext-context-cache nil
  "A hashmap that contains contexts for the currently active hypertext projects")

(defconst hypertext-data-file-name ".hypertext"
  "Name for the file used to hold values")

(defconst hypertext-index-file-name "index.org"
  "Name for the file containing index of org files in project")

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
  (or
   (cl-loop with prev-dir
            for dir = (expand-file-name default-directory)
                    then (progn (setq prev-dir dir)
                                (file-name-directory (directory-file-name dir)))
            for cache-value = (gethash dir hypertext-directories-cache)
            thereis cache-value
            until (equal dir prev-dir))
   (hypertext-scan-directories-upward default-directory)
   default-directory))

(defun hypertext-find-root-cache ()
  (or (gethash default-directory hypertext-directories-cache)
      (puthash default-directory (hypertext-find-root) hypertext-directories-cache)))

(defun hypertext-save-context (&optional root context)
  "Save given context for a given root, values not provided are retrieved from cache
If values are not present in cache, nothing is saved"
  (let* ((real-root (or root (gethash default-directory hypertext-directories-cache)))
         (real-context (or context (gethash real-root hypertext-context-cache))))
    (if (and real-root real-context)
        (progn
          (with-temp-file (expand-file-name hypertext-data-file-name real-root)
            (insert (let (print-length) (prin1-to-string real-context))))
          (puthash root context hypertext-context-cache))
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

(defun hypertext-create-new-context ()
  "Create new context and place it in the current directory
Context is a plist with following symbols:
:author, :project-name, :description, :created, :lastentry, :file-index
:lastentry is a running index of the nodes for persistent hyperlinks
:file-index contains alist of form (file dir (links)) for files in project"
  (hpe/hypertext-edit-properties
   ((:author "Author")
    (:email "Email")
    (:project-name "Project name")
    (:description "Description"))
   ;; outside variables are no longer available
   (let ((root (hypertext-find-root-cache))
         (new-context `(:author ,:author
                        :email ,:email
                        :project-name ,:project-name
                        :description ,:description
                        :created ,(format-time-string "%F %R" (current-time))
                        :lastentry 0
                        :file-index nil))
     (puthash root new-context hypertext-context-cache)
     (hypertext-save-context root new-context)))))

(defun hypertext-activate ()
  "Activete hypertext mode context
Finds and loads specific context for this directory or creates a new one if necessary.
See also 'hypertext-create-new-context'."
  (interactive)
  ;; after this call we definitely have something in that folder
  (when (null hypertext-directories-cache)
    (setq hypertext-directories-cache (make-hash-table :test 'equal)))
  (when (null hypertext-context-cache)
    (setq hypertext-context-cache (make-hash-table :test 'equal)))
  (let ((root (hypertext-find-root-cache)))
    (unless (gethash root hypertext-context-cache)
      (unless (hypertext-load-context root)
        (hypertext-create-new-context)))))

(defun hypertext-deactivate ()
  "Deactivate hypertext mode context"
  (interactive)
  (clrhash hypertext-context-cache))

(defmacro hypertext-with-context (&rest body)
  "Execute BODY with hypertext-mode context if there is one present.
See 'hypertext-create-new-context' for context contents"
  `(let* ((root (hypertext-find-root-cache))
          (context (gethash root hypertext-context-cache)))
     (if (null context)
         (message "No context available")
       ,@body
       (hypertext-save-context root context))))

(defun hypertext-regenerate-index-file (index-file-name context)
  "Regenerate index of org file in the project as INDEX-FILE-NAME from CONTEXT"
  (with-temp-buffer
    (find-file index-file-name)
    (erase-buffer)
    (let ((author (plist-get context :author))
          (index (plist-get context :file-index))
          (project-name (plist-get context :project-name)))
      (insert
       (with-output-to-string
         (princ (format "#+DATE: %s\n" (format-time-string "%F %R" (current-time))))
         (princ "#+TITLE: Index\n")
	 (princ "# You shouldn't try to modify this buffer manually\n\n")
	 (princ (format "* Index for %s\n\n" project-name))
	 (cl-loop for entry in index
                  do (princ (format "** %s \n\n" (car entry))))))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun hypertext-add-org-file (file)
  "Add new file to project"
  (interactive "FNew org file name:")
  (hypertext-with-context
   (when (not (file-exists-p file))
     ;; make some initial file content
     (let* ((title-hint (file-name-base file))
            (title (read-from-minibuffer "Document title: " title-hint)))
       (with-temp-file file
         (insert (format "#+TITLE: %s\n" title))
         (insert (format "#+AUTHOR: %s\n" (plist-get context :author)))
         (insert (format "#+EMAIL: %s\n" (plist-get context :email)))
         (insert (format "#+DATE: %s\n" (format-time-string "%F %R" (current-time)))))))
   (let ((normal-file-name (file-relative-name (expand-file-name file) root))
         (index (plist-get context :file-index)))
     (unless (assq normal-file-name index)
       (plist-put context :file-index (cons (list normal-file-name root nil) index))
       (hypertext-regenerate-index-file (file-name-concat root hypertext-index-file-name)
                                        context :file-index))
     (find-file file))))

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
