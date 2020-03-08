;;(pp package-load-list) ;;nil
;;(pp package-enable-at-startup) ;; t
;;(pp load-path) ;; For now, equal to subdirs of package-directory-list
;; See: the last 6 paragraphs of https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html#Package-Installation
;; your init file should call the function package-initialize. It is up to you to ensure that relevant user options, such as package-load-list (see below), are set up prior to the package-initialize call. This will automatically set package-enable-at-startup to nil, to avoid loading the packages again after processing the init file. 
(package-initialize)
;;(pp load-path) ;; At this time, add subdirs of  package-user-dir to package-directory-list
;;(pp package-load-list) ;; (all)
;;(pp package-enable-at-startup) ;; nil

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-custom-functions)
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-key-bindings)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load-file custom-file)
