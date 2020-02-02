(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))


;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

 ;; cl - Common Lisp Extension
(require 'cl)

 ;; Add Packages
(defvar arki/packages '(
		;; --- Themes ---
		monokai-theme
		;; --- Auto-completion ---
		company
		;;smex ;; use swiper instead
		swiper
		counsel
		smartparens
		;; --- Better Editor ---
		hungry-delete
		;; --- interactive ---
		popwin
		;; --- shell util ---
		exec-path-from-shell
		
		;; --- Major Mode ---
		;;js2-mode
		;; --- Minor Mode ---
		;;nodejs-repl
		) "Default packages")


(setq package-selected-packages arki/packages)


(defun arki/packages-installed-p ()
     (loop for pkg in arki/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

(unless (arki/packages-installed-p)
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

;; 启用monokai主题
(load-theme 'monokai t)

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

(provide 'init-packages)
