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

(defcustom arki/auto-save-idle-duration 1
  "The number of seconds Emacs has to be idle, before auto-saving the current buffer.
See `arki/auto-save-auto-save-when-idle'."
  :group 'arki/auto-save
  :type 'integer)

(defcustom arki/auto-save-remote-files nil
  "Save remote files when t, ignore them otherwise."
  :group 'arki/auto-save
  :type 'boolean)

(defcustom arki/auto-save-exclude nil
  "A list of regexps for buffer-file-name excluded from arki/auto-save.
When a buffer-file-name matches any of the regexps it is ignored."
  :group 'arki/auto-save
  :type '(repeat (choice regexp)))

(defun arki/auto-save-include-p (filename)
  "Return non-nil if FILENAME doesn't match any of the `arki/auto-save-exclude'."
  (let ((checks arki/auto-save-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
                          (if (stringp (car checks))
                              (string-match (car checks) filename))))
            checks (cdr checks)))
    keepit))


(defun arki/auto-save-command ()
  "Save the current buffer if needed."
  (when (and buffer-file-name
	     ;; And modified
             (buffer-modified-p (current-buffer))
	     ;; And writable
             (file-writable-p buffer-file-name)
	     ;; And Yassnippet is not active
             (or (not (boundp 'yas--active-snippets))
		 (not yas--active-snippets))
             ;; And  Company is not active
             (or (not (boundp 'company-candidates))
                 (not company-candidates))
	     ;; And (if is remote: allow remote save)
             (if (file-remote-p buffer-file-name) arki/auto-save-remote-files t)
	     ;; And not file name not excluded
             (arki/auto-save-include-p buffer-file-name))
    (if arki/auto-save-silent
	(with-temp-message (format "Saving %S" buffer-file-name) (save-buffer))
      (save-buffer))
    ))

(defvar arki/auto-save-idle-timer)

(defun arki/auto-save-command-advice (&rest _args)
  "A simple wrapper around `arki/auto-save-command' that's advice-friendly."
  (arki/auto-save-command))

(defun arki/auto-save-advice-trigger-commands ()
  "Apply arki/auto-save advice to the commands listed in `arki/auto-save-triggers'."
  (mapc (lambda (command)
          (advice-add command :before #'arki/auto-save-command-advice))
        arki/auto-save-triggers))

(defun arki/auto-save-remove-advice-from-trigger-commands ()
  "Remove arki/auto-save advice from to the commands listed in `arki/auto-save-triggers'."
  (mapc (lambda (command)
          (advice-remove command #'arki/auto-save-command-advice))
        arki/auto-save-triggers))

(defun arki/auto-save-initialize-idle-timer ()
  "Initialize arki/auto-save idle timer if `arki/auto-save-auto-save-when-idle' is true."
  (setq arki/auto-save-idle-timer
        (when arki/auto-save-auto-save-when-idle
          (run-with-idle-timer arki/auto-save-idle-duration t #'arki/auto-save-command))))

(defun arki/auto-save-stop-idle-timer ()
  "Stop arki/auto-save idle timer if `arki/auto-save-idle-timer' is set."
  (when arki/auto-save-idle-timer
    (cancel-timer arki/auto-save-idle-timer)))

(defun arki/auto-save-initialize ()
  "Setup arki/auto-save's advices and hooks."
  (arki/auto-save-advice-trigger-commands)
  (arki/auto-save-initialize-idle-timer)
  (dolist (hook arki/auto-save-hook-triggers)
    (add-hook hook #'arki/auto-save-command)))

(defun arki/auto-save-stop ()
  "Cleanup arki/auto-save's advices and hooks."
  (arki/auto-save-remove-advice-from-trigger-commands)
  (arki/auto-save-stop-idle-timer)
  (dolist (hook arki/auto-save-hook-triggers)
    (remove-hook hook #'arki/auto-save-command)))

;; ;;;###autoload
;; (define-minor-mode arki/auto-save-mode
;;   "A minor mode that saves your Emacs buffers when they lose focus."
;;   :lighter " arki/auto-save"
;;   :keymap arki/auto-save-mode-map
;;   :group 'arki/auto-save
;;   :global nil
;;   (cond
;;    (arki/auto-save-mode (arki/auto-save-initialize))
;;    (t (arki/auto-save-stop))))
(add-hook 'emacs-startup-hook 'arki/auto-save-initialize)

(provide 'init-auto-save)
