
;; 取消滚动到底部的报警声
(setq ring-bell-function 'ignore)

;; 显示行号
(global-linum-mode 1)

;; 选中一段文字之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)

;; 禁止 Emacs 自动生成备份文件
(setq make-backup-files nil)

;; 禁止自动保存
(setq auto-save-default nil)

;; 自动加载修改过的文件
(global-auto-revert-mode t)

;; 在emacs-lisp模式下括号高亮匹配
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 记录最近打开过的文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 15)

(provide 'init-better-defaults)
