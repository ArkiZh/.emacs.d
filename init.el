(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-custom-functions)
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-key-bindings)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load-file custom-file)
