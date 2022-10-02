;;; hypertext-prop-edit.el --- convenience functions for setting values of properties

;; based on cus-edit.el

(require 'wid-edit)

(defmacro hpe/make-lambda-set (bindings &rest callback-actions)
  "Make set of closures with shared context
BINDINGS are a list of form (SYMBOL NAME), where SYMBOL is not defined yet
CALLBACK-ACTIONS are forms that use symbols from bindings
Return created captures a as a list of forms:
(do-callback-actions (set-symbol NAME) ...)"
  (let ((unwrapped-bindings (map 'list (lambda (binding) (car binding)) bindings)))
    `(lexical-let ,unwrapped-bindings
       (list (lambda () ,@callback-actions)
             ,@(map 'list
                    (lambda (binding) `(list
                                        (lambda (val) (setq ,(car binding) val))
                                        ,(cadr binding)))
                    bindings)))))

(defmacro hpe/initialize-and-execute (bindings initialiser &rest callback-actions)
  "BINGINGS have form of ((BINDING-SYMBOL PROMPT) ...)
   INITIALISER somehow assigns values to all of BINDING-SYMBOL
   then it calls CALLBACK-ACTIONS with values of BINDING-SYMBOL set"
  `(let ((closure-parts (hpe/make-lambda-set ,bindings ,@callback-actions)))
    (,initialiser (cdr closure-parts)
                  (car closure-parts))))

;; (defun init-values (bindings callback)
;;   (loop for (setter desc) in bindings
;;         for idx = 0 then (incf idx)
;;         do (message "Setting %s" desc)
;;            (funcall setter idx))
;;   (funcall callback))

(defun hpe/get-fresh-buffer (name)
  "Get a fresh new buffer with name NAME.
If the buffer already exist, clean it up to be like new.
Beware: it's not quite like new.  Good enough for custom, but maybe
not for everybody."
  ;; To be more complete, we should also kill all permanent-local variables,
  ;; but it's not needed for custom.
  (let ((buf (get-buffer name)))
    (when (and buf (buffer-local-value 'buffer-file-name buf))
      ;; This will check if the file is not saved.
      (kill-buffer buf)
      (setq buf nil))
    (if (null buf)
	(get-buffer-create name)
      (with-current-buffer buf
	(kill-all-local-variables)
	(run-hooks 'kill-buffer-hook)
	;; Delete overlays before erasing the buffer so the overlay hooks
	;; don't get run spuriously when we erase the buffer.
	(let ((ols (overlay-lists)))
	  (dolist (ol (nconc (car ols) (cdr ols)))
	    (delete-overlay ol)))
	(erase-buffer)
	buf))))

(defvar hypertext-prop-edit-return-point nil
  "(BUFFER . POS) that we should return to after finishing property editing")

(defvar hypertext-prop-edit-callback nil
  "Variable to hold callback for successful set")

(defun hypertext-prop-edit-finish ()
  "Finish editing properties and use the values as set"
  (interactive  "" 'hypertext-prop-edit-mode)
  (cl-assert (eq major-mode 'hypertext-prop-edit-mode) nil "Must be in hypertext-prop-edit-mode")
  (cl-assert hypertext-prop-edit-callback nil "Callback for hypertext-prop-edit must be set")
  (pcase hypertext-prop-edit-return-point
    (`(,buffer . ,pos)
     (let ((curbuf (current-buffer)))
       (pop-to-buffer-same-window buffer)
       (goto-char pos)
       (bury-buffer curbuf)
       (funcall hypertext-prop-edit-callback)))
    (_ (error "Previous buffer and return points are not set"))))

(defun hypertext-prop-edit-cancel ()
  "Cancel editing operation and whatever we wanted to do in `hypertext-mode' as well"
  (interactive "" 'hypertext-prop-edit-mode)
  (cl-assert (eq major-mode 'hypertext-prop-edit-mode) nil "Must be in hypertext-prop-edit-mode")
  (pcase hypertext-prop-edit-return-point
    (`(,buffer . ,pos)
     (let ((curbuf (current-buffer)))
       (pop-to-buffer-same-window buffer)
       (goto-char pos)
       (bury-buffer curbuf)))
    (_ (error "Previous buffer and return points are not set"))))


(defvar hypertext-prop-edit-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "\C-c\C-c" 'hypertext-prop-edit-finish)
    (define-key map "\C-c\C-x" 'hypertext-prop-edit-cancel)
    (define-key map "q" 'hypertext-prop-edit-cancel)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `hypertext-edit-prop-mode'.")

(define-derived-mode hypertext-prop-edit-mode nil "Hypertext propedit"
  "Major mode for temp buffers that allow to set properties for hypertext modes

The following commands are available:

\\<widget-keymap>\
Move to next button, link or editable field.      \\[widget-forward]
Move to previous button, link or editable field.  \\[widget-backward]
\\<hypertext-prop-edit-mode-map>\
Apply the values entered.                         \\[hypertext-prop-edit-finish]
Cancel edit.                                      \\[hypertext-prop-edit-cancel]
"
  (use-local-map hypertext-prop-edit-mode-map)
  (buffer-disable-undo))

(defvar hypertext-prop-edit-commands
  '((" Finish " (lambda (widget &optional event) (hypertext-prop-edit-finish)) "Finish editing and return with values")
    (" Cancel " (lambda (widget &optional event) (hypertext-prop-edit-cancel)) "Cancel editing and discard values"))
  "Default commands for the `hypertext-prop-edit-mode', displayed in the buffer
Commands are defined as alist with format of (TAG ACTION HELP)
TAG is a string, used as the :tag property of a widget.
COMMAND is the command that the item or button runs.
HELP should be a string that can be used as the help echo property for tooltips
and the like.")

(defun hpe/prop-edit-buffer-create (options callback)
  "Create a buffer containing OPTIONS.
OPTIONS should be an alist of the form ((SYMBOL LABEL)...), where
SYMBOL is a customization option, and LABEL is a text for a widget
 for editing that option.
CALLBACK will be executed after user finishes with options"
  (setq hypertext-prop-edit-callback callback)
  (let ((cur-buf (current-buffer))
        (cur-pos (point)))
    (pop-to-buffer-same-window
     (hpe/get-fresh-buffer "*Hypertext-Properties*")
     t)
    (setq hypertext-prop-edit-return-point (cons cur-buf cur-pos))
    (hypertext-prop-edit-mode)
    ;; Insert verbose help at the top of the custom buffer.
    (widget-insert "Input values for properties listed below:\n\n")
    ;; Now populate the custom buffer.
    (message "Creating edit items...")
    (cl-loop for (var-setter desc) in options
          for desc-format = (format "%-20s %%v" (concat desc ":"))
          do
          (widget-create 'editable-field
                         :size 20
                         :format desc-format
                         :action (lambda (widget &optional event)
                                   (message (concat "value set to " (widget-value widget)))
                                   (widget-apply widget :notify widget widget event)
                                   (widget-forward 1))
                         :notify (eval `(lambda (widget &rest ignore)
                                          (message "value changed to %s"
                                                   (widget-value widget))
                                          (funcall ,var-setter (widget-value widget))))
                         "")
          (widget-insert "\n\n"))
    (message "Creating edit items... done")
    (let ((button (lambda (tag action help)
		    (widget-insert " ")
                    (push (widget-create
                           'push-button :tag tag
                           :help-echo help :action action)
                          custom-command-buttons)))
	  (commands hypertext-prop-edit-commands))
      (apply button (pop commands))   ; Finish
      (apply button (pop commands))   ; Cancel
      (message "Buttons created"))
    (widget-setup)
    (goto-char (point-min))))

(defmacro hpe/hypertext-edit-properties (bindings &rest callback-actions)
  `(hpe/initialize-and-execute ,bindings hpe/prop-edit-buffer-create ,@callback-actions))

(provide 'hypertext-prop-edit)
