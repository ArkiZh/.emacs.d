 (when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
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
		;;smex
		swiper
		counsel
		smartparens
		;; --- Better Editor ---
		hungry-delete
		;; --- interactive ---
		popwin
		;;smartparens
		;; --- Major Mode ---
		;;js2-mode
		;; --- Minor Mode ---
		;;nodejs-repl
		;;exec-path-from-shell
		;; solarized-theme
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   (quote
    ("a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; 设置默认字体为汉字字符集 找到字符类型的方式： M+x describe-font RET RET
(set-frame-font "-outline-新宋体-normal-normal-normal-mono-13-*-*-*-c-*-iso8859-1")

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 125)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 禁止 Emacs 自动生成备份文件
(setq make-backup-files nil)

;; 禁止自动保存
(setq auto-save-default nil)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 全屏显示
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 显示行号
(global-linum-mode 1)

;; 高亮显示当前行
(global-hl-line-mode t)

;; 选中一段文字之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)

;; 在emacs-lisp模式下括号高亮匹配
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 设置常用快捷键
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; 自动加载修改过的文件
(global-auto-revert-mode t)

;; 关闭文件滑动控件
;; (scroll-bar-mode -1)

;; 更改光标的样式（不能生效，解决方案见第二集）
;; (setq cursor-type 'bar)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)


;; 记录最近打开过的文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;; 这个快捷键绑定可以用之后的插件 counsel 代替
(global-set-key (kbd "C-x C-r") 'recentf-open-files)


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


;; 配置counsel
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)


;; 配置smartparens
(require 'smartparens-config)
;;(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)


;; 配置popwin
(require 'popwin)
(popwin-mode 1)
