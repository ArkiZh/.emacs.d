;; 设置utf-8为默认编码格式
(set-language-environment "UTF-8")

;; 取消滚动到底部的报警声
(setq ring-bell-function 'ignore)

;; Enable truncate lines
(setq-default truncate-lines t)

;; 禁止 Emacs 自动生成备份文件
(setq make-backup-files nil)

;; 禁止默认的自动保存
(setq auto-save-default nil)

;; 保存桌面布局
(desktop-save-mode -1)

;; 自动加载修改过的文件
(global-auto-revert-mode t)

;; 记录最近打开过的文件
;; (require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 300)

;; Comment this keybinding. Use dashboard instead.
;; (arki/define-key "o" 'recentf-open-files 'arki/prefix-keymap)

;; Bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" arki/cache-dir))
(setq bookmark-save-flag 1)

;; Less typing when emacs asks yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable function
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(provide 'init-better-defaults)
