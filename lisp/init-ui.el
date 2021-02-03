;; 设置默认字体为汉字字符集 找到字符类型的方式： M+x describe-font RET RET
(set-frame-font "-ADBO-Source Han Serif CN-normal-normal-normal-*-17-*-*-*-*-0-iso10646-1")

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 125)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
;; 关闭菜单栏
(menu-bar-mode 0)
;; 全屏显示
;; (setq initial-frame-alist (quote ((fullscreen . maximized))))
(toggle-frame-maximized)

;; 高亮显示当前行
(global-hl-line-mode t)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 更改光标的样式（不能生效，解决方案见第二集）
;; (setq cursor-type 'bar)

(provide 'init-ui)
