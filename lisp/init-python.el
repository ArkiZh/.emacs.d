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
(setq arki/lsp--ready? nil)
;; Set key prefix should come before require pack.
(setq lsp-keymap-prefix "C-c l")
(when (require-pack 'lsp-mode)
  (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
  ;; https://github.com/emacs-lsp/dap-mode
  (require-pack 'dap-mode)

  ;; https://github.com/emacs-lsp/lsp-ui
  (require-pack 'lsp-ui)

  ;; https://github.com/emacs-lsp/lsp-treemacs
  (require-pack 'lsp-treemacs)
  
  (setq arki/lsp--ready? t)
  )


;; Config python env

;; https://github.com/necaris/conda.el
(setq arki/python--conda-home-ready? nil)
(when (require-pack 'conda)

  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)

  (defun arki/python--prepare-conda-env ()
    "Set conda home. And switch to base env if conda-project-env-path is not provided."    
    (let* ((conda-path (executable-find "conda")))
      (if conda-path
	  (if (and (string-prefix-p conda-anaconda-home conda-path) (string-match-p "conda" conda-anaconda-home))
	      ;; Here: conda command resides in conda-anaconda-home, and conda-anaconda-home contains "codna"
	      ;; Activate conda env
	      (progn (setq arki/python--conda-home-ready? t)
		     ;; `conda-project-env-path` can be set by a buffer-local or project-local variable (e.g. a `.dir-locals.el` that defines `conda-project-env-path`),
		     ;; or inferred from an `environment.yml` or similar at the project level."
		     (if (bound-and-true-p conda-project-env-path)
			 ;; If already activated, no need to re-activate.
			 (if (equal conda-env-current-path (conda-env-name-to-dir conda-project-env-path))
			     (message "Conda env has already been [%s]" conda-env-current-name)
			   (message "Got conda env info, activate it now: [%s]" conda-project-env-path)
			   (conda-env-activate conda-project-env-path))
		       ;; Cannot get env info, select an env.
		       (conda-env-activate))
		     ;; Verify python executable is in current conda env.
		     (if (string-prefix-p conda-env-current-path  (executable-find "python")) t
		       (message "Current conda env broken: [%s] Python executable not found here!" conda-env-current-path)
		       nil))
	    ;; Here: conda home is not ok, config it now:
	    (let* ((prompt-message (format "Conda home [%s] is not valid, while conda is [%s]. Now select conda home: " conda-anaconda-home conda-path))
		   (conda-home (ido-read-directory-name prompt-message (arki/directory-parent conda-path 2))))
	      (customize-save-variable 'conda-anaconda-home conda-home)
	      (customize-save-variable 'conda-env-home-directory conda-home)
	      (arki/python--prepare-conda-env))
	    )
	(message "Warning: Conda executable is not available. Stop setting conda related things.")
	nil
	)))

  (defun arki/python--adjust-conda-env-and-lsp-mode ()
    "The logic:

1. If conda env is not activated as desired: Activate the desired one, and link to lsp server if needed.
As desired means: 
Local variable conda-project-env-path is consistent with conda-env-current-path,
or, conda-project-env-path is not set.

2. If conda env changed: And lsp is not running: Just start it.
                         And lsp is running: restart lsp.
"
    (when (equal major-mode 'python-mode)

      (if (or (not (bound-and-true-p conda-project-env-path)) ; Local env not set
	      ;; Local env set the same with current env.
	      (and (bound-and-true-p conda-project-env-path)
		   (equal conda-env-current-path (conda-env-name-to-dir conda-project-env-path))))
	  ;; Conda env is activated and lsp mode it not on: turn on lsp mode.
	  (when (and (bound-and-true-p conda-env-current-path)
		     (not (bound-and-true-p lsp-mode)))
	    (lsp))

	;; Local env is set and not the same with current activated env.
	(conda-env-activate conda-project-env-path)
	(when (string-match-p "Connected to" (format "%S" (lsp)))
	  (call-interactively 'lsp-workspace-restart)))))

  (defun arki/python--adjust-conda-env-and-lsp-mode-advice (&rest args)
    (arki/python--adjust-conda-env-and-lsp-mode))

  (setq arki/python--lsp-ready? nil)
  (defun arki/python--prepare-lsp ()
    "Prepare python language server. Can only be executed successfully once."
    (when (and (not arki/python--lsp-ready?) ; Can only be executed successfully once.
	       arki/lsp--ready?		     ; Can only be invoked when lsp is ready.
	       (arki/python--prepare-conda-env)) ; Can only be invoked when conda env is ready.
      ;; https://github.com/emacs-lsp/lsp-pyright
      (when (require-pack 'lsp-pyright)
	;; (setq lsp-pyright-python-executable-cmd (expand-file-name "bin/python" (conda-env-name-to-dir "base")))
	(setq lsp-pyright-python-executable-cmd "python")
	(advice-add 'switch-to-buffer :after #'arki/python--adjust-conda-env-and-lsp-mode-advice)
	(setq arki/python--lsp-ready? t))))


  (defun arki/python-lsp-config ()
    "Init python lsp config."
    (interactive)
    (arki/python--prepare-lsp)
    (arki/python--adjust-conda-env-and-lsp-mode))

  
  (arki/define-key "p" 'arki/python-lsp-config 'arki/prefix-keymap)
  )


(provide 'init-python)
