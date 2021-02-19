;; 设置utf-8为默认编码格式
(set-language-environment "UTF-8")

;; 取消滚动到底部的报警声
(setq ring-bell-function 'ignore)

;; 显示行号
(if (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

;; 在最下面显示光标在行中的位置
(column-number-mode 1)

;; 选中一段文字之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)

;; 禁止 Emacs 自动生成备份文件
(setq make-backup-files nil)

;; 禁止自动保存
(setq auto-save-default nil)

;; 自动加载修改过的文件
(global-auto-revert-mode t)

;; 在emacs-lisp模式下括号高亮匹配
;; 先将函数扩展为光标不在括号上也能显示两侧的括号
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))
;; 激活show-paren-mode
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 记录最近打开过的文件
(require 'recentf)
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

;; Dired config
(setq dired-recursive-deletes 'always) ;; Always delete recursively
(setq dired-recursive-copies 'always) ;; Always copy recursively
(put 'dired-find-alternate-file 'disabled nil) ;; Enable this danger function: reuse current buffer by pressing "a" in dired mode
(require 'dired-x) ;; Enable <C-x C-j> to open current file's directory
(setq dired-dwim-target t) ;; 当一个frame中存在多个window时，将下一个分屏自动设置成拷贝地址的目标

(provide 'init-better-defaults)
