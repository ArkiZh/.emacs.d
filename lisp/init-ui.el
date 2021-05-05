;; Config theme
;; Favorite themes: monokai-theme solarized-theme gruvbox-theme

;; https://github.com/hlissner/emacs-doom-themes/blob/master/doom-themes.el
(when (require-pack 'doom-themes)
  ;; (load-theme 'doom-dark+)
  ;; (load-theme 'doom-molokai t)
  (load-theme 'doom-one-light t)
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  ;; https://github.com/domtronn/all-the-icons.el/
  ;; 需要安装字体文件：M-x all-the-icons-install-fonts
  (require-pack 'all-the-icons)
  )


(when (equal window-system 'x)
  ;; Set mouse-color
  (if (equal (frame-parameter nil 'background-mode) 'light)
      (set-frame-parameter nil 'mouse-color "black")
    (set-frame-parameter nil 'mouse-color "white")
      )
  )

;; (when (require-pack 'on-screen)
;;   (on-screen-global-mode +1))

;; Set cursor type to box or bar
(setq cursor-type 'box)

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

;; 显示行号 仅当编程模式时候
(add-hook 'prog-mode-hook
	  (lambda nil
	    (if (version<= "26.0.50" emacs-version )
		(display-line-numbers-mode)
	      (linum-mode 1)))
	  )

;; 在最下面显示光标在行中的位置
(column-number-mode 1)

(provide 'init-ui)
