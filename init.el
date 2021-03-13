;;; init.el start
;; Some config structure learnt from https://github.com/purcell/emacs.d

;; ----------------------------------------------------------------------------------------------------
;; WARM UP
;; ----------------------------------------------------------------------------------------------------
;; Check emacs version
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(let ((tested-ver "27.1"))
  (when (version< emacs-version tested-ver)
    (message "This config has been tested under version %s. But yours is %s. Upgrade if possible." tested-ver emacs-version)))

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

;; Adjust garbage collection thresholds during startup, and thereafter
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Setup mirror from https://elpa.emacs-china.org/
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
			 ("melpa" . "https://elpa.emacs-china.org/melpa/")))


;;(pp package-enable-at-startup) ;; t
;;(pp load-path) ;; For now, equal to subdirs of package-directory-list
;; See: the last 6 paragraphs of https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html#Package-Installation
;; your init file should call the function package-initialize. It is up to you to ensure that relevant user options, such as package-load-list (see below), are set up prior to the package-initialize call. This will automatically set package-enable-at-startup to nil, to avoid loading the packages again after processing the init file. 
(package-initialize)
;;(pp load-path) ;; At this time, add subdirs of  package-user-dir to package-directory-list
;;(pp package-load-list) ;; (all)
;;(pp package-enable-at-startup) ;; nil



;; SETUP MY CUSTOM PACKAGES

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-functions)

;; --------------------------------------------------SHELL ENVIRONMENT--------------------------------------------------
;; let emacs could find the executable
(when (memq window-system '(mac ns))
  (when (require-pack 'exec-path-from-shell)
    (exec-path-from-shell-initialize))
  )


;; Import new GNU ELPA keys (if any) into package.el’s keyring.
(require-pack 'gnu-elpa-keyring-update)



(require 'init-better-defaults)
(require 'init-ease-editor)
(require 'init-dired)
(require 'init-ui)
(require 'init-dashboard)
(require 'init-screensaver)
(require 'init-which-key)
(require 'init-auto-complete)
(require 'init-elisp)
(require 'init-org)
(require 'init-project-tools)
(require 'init-filetypes-support)
(require 'init-input-method)
(require 'init-fonts)


;;----------------------------------------------------------------------------
;; Load variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(message "My init file ends here!")
