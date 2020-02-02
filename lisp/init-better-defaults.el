
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

;; Dired config
(setq dired-recursive-deletes 'always) ;; Always delete recursively
(setq dired-recursive-copies 'always) ;; Always copy recursively
(put 'dired-find-alternate-file 'disabled nil) ;; Enable this danger function: reuse current buffer by pressing "a" in dired mode
(require 'dired-x) ;; Enable <C-x C-j> to open current file's directory
(setq dired-dwim-target t) ;; 当一个frame中存在多个window时，将下一个分屏自动设置成拷贝地址的目标

(provide 'init-better-defaults)
