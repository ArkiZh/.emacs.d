;; 使用新的自动保存
;; 参考自：
;; https://github.com/bbatsov/super-save/blob/2a905b8bdfc93bee16e2d62a61c6211bbe009331/super-save.el
;; https://www.emacswiki.org/emacs/auto-save.el
(defgroup arki/auto-save nil
  "Smart-saving of buffers."
  :group 'arki/config)

(defvar arki/auto-save-mode-map (make-sparse-keymap)
  "arki/auto-save mode's keymap.")

(defcustom arki/auto-save-triggers
  '(switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right next-buffer previous-buffer)
  "A list of commands which would trigger `arki/auto-save-command'."
  :group 'arki/auto-save
  :type '(repeat symbol))

(defcustom arki/auto-save-silent t
  "Save buffer without modify minibuffer message."
  :group 'arki/auto-save
  :type 'boolean)

(defcustom arki/auto-save-hook-triggers
  '(mouse-leave-buffer-hook focus-out-hook)
  "A list of hooks which would trigger `arki/auto-save-command'."
  :group 'arki/auto-save
  :type '(repeat symbol))

(defcustom arki/auto-save-auto-save-when-idle t
  "Save current buffer automatically when Emacs is idle."
  :group 'arki/auto-save
  :type 'boolean)

(defcustom arki/auto-save-idle-duration 5
  "The number of seconds Emacs has to be idle, before auto-saving the current buffer.
See `arki/auto-save-auto-save-when-idle'."
  :group 'arki/auto-save
  :type 'integer)

(defcustom arki/auto-save-remote-files nil
  "Save remote files when t, ignore them otherwise."
  :group 'arki/auto-save
  :type 'boolean)

(defcustom arki/auto-save-max-filesize 10
  "File size in MB.

If set, when the file is larger than VAL, don't do auto-save for performance issues.
If VAL is nil, ignore the file size impact."
  :group 'arki/auto-save
  :type 'number)

(defcustom arki/auto-save-exclude '(".emacs.d/elpa")
  "A list of regexps for buffer-file-name excluded from arki/auto-save.
When a buffer-file-name matches any of the regexps it is ignored."
  :group 'arki/auto-save
  :type '(repeat (choice regexp)))

(defun arki/auto-save--include-p (filename)
  "Return non-nil if FILENAME doesn't match any of the `arki/auto-save-exclude'."
  (let ((checks arki/auto-save-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
                          (if (stringp (car checks))
                              (string-match (car checks) filename))))
            checks (cdr checks)))
    keepit))



(defun arki/auto-save--check-buffer-status ()
  "Check whether the buffer is ready for save"
  (and 
   ;; And Yassnippet is not active
   (or (not (boundp 'yas--active-snippets))
       (not yas--active-snippets))
   ;; And  Company is not active
   (or (not (boundp 'company-candidates))
       (not company-candidates))
   )
  )

;; file size check
(defvar arki/auto-save--cur-file-size nil "Size of current file in MB")
(make-variable-buffer-local 'arki/auto-save--cur-file-size)

(defun arki/auto-save--check-file-size ()
  "Check file size. According to `arki/auto-save-max-filesize'"
  (if arki/auto-save-max-filesize
      (if arki/auto-save--cur-file-size
	  (<= arki/auto-save--cur-file-size arki/auto-save-max-filesize)
	(let* ((file-size-bytes (file-attribute-size (file-attributes (buffer-file-name))))
	       ;; When the file is newly created, file--attributes is nil, so let file-size be 0 now.
	       (file-size-mega-bytes (if file-size-bytes (/ file-size-bytes 1024.0 1024.0) 0)))
	  (setq arki/auto-save--cur-file-size file-size-mega-bytes))
	(arki/auto-save--check-file-size)
	)
    t
    ))

(defun arki/auto-save-command ()
  "Save the current buffer if needed."
  ;; Use (buffer-file-name) instead of buffer-file-name-variable to
  ;; avoid error when the file is new created.
  ;; The ERROR: Wrong type argument: number-or-marker-p, nil
  (let ((file-name (buffer-file-name)))
    (when (and file-name
	       ;; And not file name not excluded
               (arki/auto-save--include-p file-name)
	       ;; And (if is remote: should allow remote save)
               (if (file-remote-p file-name) arki/auto-save-remote-files t)
	       ;; And is not readonly
	       (not buffer-read-only)
	       ;; And modified
               (buffer-modified-p (current-buffer))
	       ;; And writable
               (file-writable-p file-name)
	       ;; Check buffer status: And Yassnippet is not active And  Company is not active
	       (arki/auto-save--check-buffer-status)
	       ;; And file is not too large
	       (arki/auto-save--check-file-size)
	       )
      (if arki/auto-save-silent
	  (with-temp-message (format "Saving %S" file-name) (save-buffer))
	(save-buffer))
      )
    )
  )

(defvar arki/auto-save--idle-timer nil)

(defun arki/auto-save-command-advice (&rest _args)
  "A simple wrapper around `arki/auto-save-command' that's advice-friendly."
  (arki/auto-save-command))

(defun arki/auto-save--advice-trigger-commands ()
  "Apply arki/auto-save advice to the commands listed in `arki/auto-save-triggers'."
  (mapc (lambda (command)
          (advice-add command :before #'arki/auto-save-command-advice))
        arki/auto-save-triggers))

(defun arki/auto-save--advice-trigger-commands-cancel ()
  "Remove arki/auto-save advice from to the commands listed in `arki/auto-save-triggers'."
  (mapc (lambda (command) (advice-remove command #'arki/auto-save-command-advice))
        arki/auto-save-triggers))

(defun arki/auto-save--idle-timer-init ()
  "Initialize arki/auto-save idle timer if `arki/auto-save-auto-save-when-idle' is true."
  (setq arki/auto-save--idle-timer
        (when arki/auto-save-auto-save-when-idle
          (run-with-idle-timer arki/auto-save-idle-duration t #'arki/auto-save-command))))

(defun arki/auto-save--idle-timer-cancel ()
  "Stop arki/auto-save idle timer if `arki/auto-save--idle-timer' is set."
  (when arki/auto-save--idle-timer
    (cancel-timer arki/auto-save--idle-timer)))

(defun arki/auto-save-stop ()
  "Cleanup arki/auto-save's advices and hooks."
  (arki/auto-save--advice-trigger-commands-cancel)
  (arki/auto-save--idle-timer-cancel)
  (dolist (hook arki/auto-save-hook-triggers)
    (remove-hook hook #'arki/auto-save-command)))

(defun arki/auto-save-init ()
  "Setup arki/auto-save's advices and hooks."

  ;; Reset auto-save state
  (arki/auto-save-stop)
  ;; Add command advice
  (arki/auto-save--advice-trigger-commands)
  ;; Add timer
  (arki/auto-save--idle-timer-init)
  ;; Add hook
  (dolist (hook arki/auto-save-hook-triggers)
    (add-hook hook #'arki/auto-save-command)))

;; ;;;###autoload
;; (define-minor-mode arki/auto-save-mode
;;   "A minor mode that saves your Emacs buffers when they lose focus."
;;   :lighter " arki/auto-save"
;;   :keymap arki/auto-save-mode-map
;;   :group 'arki/auto-save
;;   :global nil
;;   (cond
;;    (arki/auto-save-mode (arki/auto-save-init))
;;    (t (arki/auto-save-stop))))
(add-hook 'emacs-startup-hook 'arki/auto-save-init)

(provide 'init-auto-save)
