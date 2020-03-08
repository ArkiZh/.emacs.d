(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))


;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar arki/packages '(
			;; basic
			use-package
			
			;; --- Themes ---
			monokai-theme
			solarized-theme

			;; --- Better editor ---
			move-text
			company
			;;smex ;; use swiper instead
			swiper
			counsel
			smartparens
			hungry-delete
			popwin
			
			;; --- shell util ---
			exec-path-from-shell

			;; --- lsp-mode ---
			lsp-mode
			lsp-ui
			flycheck
			company-lsp
			lsp-treemacs
			helm-lsp
			lsp-ivy
			;;dap-mode ;; Don't know how to use
			realgud
			conda

			;; --- anaconda-mode ---
			;;anaconda-mode
			;;company-anaconda
			;;neotree
			
			;; --- Major Mode ---
			;;js2-mode
			;; --- Minor Mode ---
			;;nodejs-repl
			) "Default packages")


(setq package-selected-packages arki/packages)


;;Check whether the packages have been installed.
(defun arki/packages-installed-p (packs)
  (loop for pkg in packs
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (arki/packages-installed-p arki/packages)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg arki/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; let emacs could find the executable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; ===============插件配置===============

;; 开启全局 Company 补全
(global-company-mode 1)

;; 启用monokai、solarized-dark主题
(load-theme 'solarized-dark t)

;; 配置move-text
(move-text-default-bindings)

;; 启用hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)


;; 配置smex，这部分功能用swiper代替了，所以注释掉了
;;(require 'smex) ; Not needed if you use package.el
;;(smex-initialize) ; Can be omitted. This might cause a (minimal) delay, when Smex is auto-initialized on its first run.
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands) ;runs Smex, limited to commands that are relevant to the active major mode. Try it with Dired or Magit.

;; 配置swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)

;; 配置smartparens
(require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode) ;; 在emacs-lisp-mode模式时加载smartparens
(smartparens-global-mode t) ;; 所有模式都加载smartparens

;; 配置popwin
(require 'popwin)
(popwin-mode 1)


;; 配置lsp-mode
(add-hook 'python-mode-hook (lambda ()
			      (setq lsp-keymap-prefix "C-c l")  ;;Should be set before require
			      (require 'lsp-mode)
			      ;; Install pyls by: pip install 'python-language-server[all]' -i https://pypi.tuna.tsinghua.edu.cn/simple/
			      (setq lsp-pyls-server-command "C:/DevSoft/anaconda/anaconda3_5.3.0/Scripts/pyls") ;; Specify you pyls command path, before or after require
			      ))
(add-hook 'python-mode-hook #'lsp-deferred)
;; 配置realgud，python的debug插件
(add-hook 'python-mode-hook (lambda () (load-library "realgud")))
;; 配置conda环境
(add-hook 'python-mode-hook (lambda ()
			      (custom-set-variables '(conda-anaconda-home "C:/DevSoft/anaconda/anaconda3_5.3.0"))  ;;Should be set before require, if not the default(~/.anaconda3)
			      (require 'conda)
			      (conda-env-initialize-interactive-shells)  ;;interactive shell support
			      (conda-env-initialize-eshell)  ;;eshell support
			      ;;(conda-env-autoactivate-mode t)  ;;try and detect the correct conda environment for a buffer,automatically
			      ))


;; 配置anaconda-mode
;;(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;(eval-after-load "company"
;;  '(add-to-list 'company-backends 'company-anaconda))
;;(add-hook 'python-mode-hook 'anaconda-mode)


(provide 'init-packages)
