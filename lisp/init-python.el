(defgroup arki/python nil
  "Python config."
  :group 'arki/config)


(defcustom arki/lsp-pyls-server-command nil
  "Python language server location.

May be here: {CONDA_HOME}/Scripts/pyls {PYTHON_ENV_HOME}/bin/pyls
"
  :group 'arki/python)


;; Config lsp.
;; https://github.com/emacs-lsp/lsp-mode
(setq arki/lsp--ready nil)
(when (require-pack 'lsp-mode)
  (setq lsp-keymap-prefix "C-c l")
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

  ;; https://github.com/emacs-lsp/dap-mode
  (require-pack 'dap-mode)

  ;; https://github.com/emacs-lsp/lsp-ui
  (require-pack 'lsp-ui)

  (setq arki/lsp--ready t)
  )


;; Config python env

;; https://github.com/necaris/conda.el
(when (require-pack 'conda)
  (defun arki/python--prepare-conda-env ()
    (let* ((conda-path (executable-find "conda")))
      (if conda-path
	  (if (and (string-prefix-p conda-anaconda-home conda-path) (string-match-p "conda" conda-anaconda-home))
	      ;; Activate conda env
	      (progn (conda-env-activate conda-env-name-for-buffer)
		     (if (string-prefix-p conda-env-current-path  (executable-find "python")) t
		       (message "Broken conda env: [%s] Python executable not found here!" conda-env-current-path)
		       nil))
	    ;; Config conda home
	    (let* ((prompt-message (format "Conda home [%s] is not valid, while conda is [%s]. Now select conda home: " conda-anaconda-home conda-path))
		   (conda-home (ido-read-directory-name prompt-message (arki/directory-parent conda-path 2))))
	      (customize-save-variable 'conda-anaconda-home conda-home)
	      (arki/python--prepare-conda-env))
	    )
	(message "Warning: Conda executable is not available. Stop configing conda related things.")
	nil
	)))
  
  (add-hook 'python-mode-hook (lambda ()
				(when (and arki/lsp--ready (arki/python--prepare-conda-env))
				  ;; Do these logic only if conda is available, it's meaningless otherwise.
				  (conda-env-initialize-interactive-shells)
				  (conda-env-initialize-eshell)
				  ;; https://github.com/emacs-lsp/lsp-pyright
				  (when (require-pack 'lsp-pyright)
				    (setq lsp-pyright-python-executable-cmd "python")
				    (lsp-deferred)
				    )
				  ))))
