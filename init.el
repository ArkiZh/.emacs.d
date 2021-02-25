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

;; Import new GNU ELPA keys (if any) into package.el’s keyring.
(require-pack 'gnu-elpa-keyring-update)

;; --------------------------------------------------UI--------------------------------------------------
;; Config theme
;; Favorite themes: monokai-theme solarized-theme gruvbox-theme

;; https://github.com/hlissner/emacs-doom-themes/blob/master/doom-themes.el
(when (require-pack 'doom-themes)
  ;; (load-theme 'doom-dark+)
  (load-theme 'doom-molokai t)
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; https://github.com/domtronn/all-the-icons.el/
  ;; 需要安装字体文件：M-x all-the-icons-install-fonts
  (require-pack 'all-the-icons)
  )

;; https://github.com/seagle0128/doom-modeline
(when (require-pack 'doom-modeline)
  (add-hook 'after-init-hook #'doom-modeline-mode))

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode 'toggle)
;; 关闭菜单栏
(menu-bar-mode 'toggle)

;; 全屏显示
;; (setq initial-frame-alist (quote ((fullscreen . maximized))))
(toggle-frame-maximized)
;; (toggle-frame-fullscreen)

;; 高亮显示当前行
(global-hl-line-mode t)

;; 关闭文件滑动控件
(scroll-bar-mode 'toggle)

;; 显示行号
(if (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

;; 在最下面显示光标在行中的位置
(column-number-mode 1)

;; --------------------------------------------------NAVIGATION--------------------------------------------------
;; Config editor navigation

;; Move current line or region with M-up or M-down.
(when (require-pack 'move-text)
  ;; 配置move-text. Use default bindings for move-text-up and move-text-down (M-up / M-down).
  ;; (move-text-default-bindings)
  (arki/define-key "M-<down>" 'move-text-down)
  (arki/define-key "M-<up>"   'move-text-up)
  )

(when (require-pack 'ace-window)
  ;; 配置ace-windown
  (arki/define-key "M-p" 'ace-window)
  )

(when (require-pack 'transpose-frame))


;; --------------------------------------------------SMART EDITOR--------------------------------------------------
;; Config auto complete
(when (require-pack 'company)
  ;; 开启全局 Company 补全
  (global-company-mode 1)
  )

(when (require-pack 'swiper)
  ;; 配置swiper
  (ivy-mode 1)
  ;; (setq ivy-use-virtual-buffers nil) ;I dont't need virtual buffer for now, so commented out.
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (arki/define-key "C-s" 'swiper)
  ;; minibuffer 里面的 M-i 绑定 与输入法的冲突了，解绑掉
  (arki/define-key "M-i" nil 'ivy-minibuffer-map)
  )

(when (require-pack 'counsel)
  ;; 配置counsel
  (arki/define-key "M-x" 'counsel-M-x)
  (arki/define-key "C-x C-f" 'counsel-find-file)
  (arki/define-key "C-h f" 'counsel-describe-function)
  (arki/define-key "C-h v" 'counsel-describe-variable)
  (arki/define-key "C-c p f" 'counsel-git) ;;查找当前所在git仓库管理的文件
  )


;; 在emacs-lisp模式下括号高亮匹配
(when (require-pack 'smartparens)
  ;; 配置smartparens
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode) ;在emacs-lisp-mode模式时加载smartparens
  ;; (smartparens-global-mode t) ;所有模式都加载smartparens
  )
;; 将函数扩展为光标不在括号上也能显示两侧的括号
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))
;; 激活show-paren-mode
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(when (require-pack 'hungry-delete)
  ;; 启用hungry-delete
  (global-hungry-delete-mode)
  )

;; 执行完命令后，直接使用C-g关闭辅助性的buffer
(when (require-pack 'popwin)
  ;; 配置popwin
  (popwin-mode 1)
  )

;; https://github.com/gongo/json-reformat
(require-pack 'json-reformat)

;; --------------------------------------------------SHELL ENVIRONMENT--------------------------------------------------
;; let emacs could find the executable
(when (memq window-system '(mac ns))
  (when (require-pack 'exec-path-from-shell)
    (exec-path-from-shell-initialize))
  )

;; --------------------------------------------------CONFIG ORG MODE--------------------------------------------------
;; org-mode needs this
(when (require-pack 'htmlize))
(when (require-pack 'org-preview-html))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq org-src-fontily-natively t)
	    ;; Add markup to text that spans more than two consecutive lines
	    ;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode/13828#13828
	    ;; Default value of org-emphasis-regexp-components: ("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
	    (setcar (nthcdr 4 org-emphasis-regexp-components) 3)
	    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
	    )
	  )

;; (setq org-agenda-files '("~/org"))

;; (setq org-startup-indented t
;;       org-pretty-entities t
;;       ;; org-hide-emphasis-markers t  ;; show actually italicized text instead of /italicized text/
;;       org-fontify-whole-heading-line t
;;       org-fontify-done-headline t
;;       org-fontify-quote-and-verse-blocks t)

;; --------------------------------------------------PROJECT UTILS--------------------------------------------------
(when (require-pack 'magit)
  ;; 配置magit
  (arki/define-key "C-c u g" 'magit-status))

(when (require-pack 'treemacs)
  ;; 打开treemmacs导航
  (arki/define-key "M-0" 'treemacs))

(when (require-pack 'projectile)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; --------------------------------------------------MARKDOWN MODE CONFIG--------------------------------------------------
(when (require-pack 'markdown-mode)
  ;; 配置markdown-mode
  ;; 调用markdown-live-preview报错markdown failed with exit code 1解决方案：(参考自https://emacs-china.org/t/markdown/11399)
  ;; 安装pandoc，配置markdown-command指向pandoc。下载地址：https://pandoc.org/
  (add-hook 'markdown-mode-hook
	    (lambda () (setq markdown-command "pandoc")))
  )

;; --------------------------------------------------VIEW CSV--------------------------------------------------
(when (require-pack 'csv-mode))

;; --------------------------------------------------VIEW KEYMAP--------------------------------------------------
;; https://github.com/justbur/emacs-which-key
(when (require-pack 'which-key)
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is triggered.
  (setq which-key-idle-delay 3)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  ;; Supported types are minibuffer, side-window, frame, and custom.
  (setq which-key-popup-type 'side-window)

  ;; location of which-key window. valid values: top, bottom, left, right, or a list of any of the two.
  (setq which-key-side-window-location 'bottom)
  ;; max width of which-key window, when displayed at left or right.
  (setq which-key-side-window-max-width 60)
  ;; max height of which-key window, when displayed at top or bottom.
  (setq which-key-side-window-max-height 10)
  )


;; --------------------------------------------------BETTER DEFAULTS--------------------------------------------------
;; 设置utf-8为默认编码格式
(set-language-environment "UTF-8")

;; 取消滚动到底部的报警声
(setq ring-bell-function 'ignore)

;; 选中一段文字之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)

;; 禁止 Emacs 自动生成备份文件
(setq make-backup-files nil)

;; 禁止自动保存
(setq auto-save-default nil)

;; 自动加载修改过的文件
(global-auto-revert-mode t)

;; 记录最近打开过的文件
;; (require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 15)

;; 保存桌面布局
(desktop-save-mode 1)

;; 启用winner mode
(winner-mode 1)

;; 调整hippie-expand调用的自动补全功能顺序
(setq hippie-expand-try-functions-list '(
					 try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

;; Less typing when emacs asks yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; --------------------------------------------------DIRED CONFIG--------------------------------------------------
;; Dired config
(setq dired-recursive-deletes 'always)	;Always delete recursively
(setq dired-recursive-copies 'always)	;Always copy recursively

;; Reuse dired buffers. https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(put 'dired-find-alternate-file 'disabled nil)
;; https://stackoverflow.com/questions/2736087/eval-after-load-vs-mode-hook
;; https://stackoverflow.com/questions/21880139/what-is-with-eval-after-load-in-emacs-lisp/21880276
;; (add-hook 'dired-mode-hook
(with-eval-after-load 'dired
  (arki/define-key "RET" 'dired-find-alternate-file 'dired-mode-map)
  (arki/define-key "^"
		   (lambda () (interactive) (find-alternate-file ".."))
		   'dired-mode-map))

(require 'dired-x) ;; Enable <C-x C-j> to open current file's directory
(setq dired-dwim-target t) 		;当一个frame中存在多个window时，将下一个分屏自动设置成拷贝地址的目标


;; --------------------------------------------------KEY BINDING--------------------------------------------------
;; 设置常用快捷键
(arki/define-key "C-h C-f" 'find-function)
(arki/define-key "C-h C-f" 'find-function)
(arki/define-key "C-h C-v" 'find-variable)
(arki/define-key "C-h C-k" 'find-function-on-key)

;; 这个快捷键绑定可以用之后的插件 counsel 代替
(arki/define-key "C-c u o" 'recentf-open-files)

;;配置org-mode中的快捷键
(arki/define-key "C-c u a" 'org-agenda)

;; 使用hippie-expand增强自动补全
(arki/define-key "M-RET" 'hippie-expand)



;; --------------------------------------------------INPUT METHOD--------------------------------------------------
;; https://github.com/tumashu/pyim
(when (require-pack 'pyim)
  (require 'pyim)
  (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
  (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'ziranma-shuangpin)
  (setq pyim-page-length 9)
  ;; 设置显示候选词的框是什么形式的
  (if (memq window-system '(x win32 ns))
      (when (require-pack 'posframe)
	(setq pyim-page-tooltip 'posframe))
    (setq pyim-page-tooltip 'popup)
    )

  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
		  pyim-probe-auto-english))
  (arki/define-key "M-i" 'pyim-convert-code-at-point)
  )


;; --------------------------------------------------CONFIG FONTS--------------------------------------------------
(require 'init-fonts)


;;----------------------------------------------------------------------------
;; Load variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

