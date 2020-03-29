;;; init.el start
;; Some config structure learnt from https://github.com/purcell/emacs.d

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))


;;(require 'init-benchmarking)		;Measure startup time

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold (* 20 1024 1024))))


;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;(pp package-load-list) ;;nil
;;(pp package-enable-at-startup) ;; t
;;(pp load-path) ;; For now, equal to subdirs of package-directory-list
;; See: the last 6 paragraphs of https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html#Package-Installation
;; your init file should call the function package-initialize. It is up to you to ensure that relevant user options, such as package-load-list (see below), are set up prior to the package-initialize call. This will automatically set package-enable-at-startup to nil, to avoid loading the packages again after processing the init file. 
(package-initialize)
;;(pp load-path) ;; At this time, add subdirs of  package-user-dir to package-directory-list
;;(pp package-load-list) ;; (all)
;;(pp package-enable-at-startup) ;; nil

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-custom-functions)
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-key-bindings)


;;----------------------------------------------------------------------------
;; Load variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))
