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

;; Find elisp source code.
(arki/define-key "C-h C-f" 'find-function)
(arki/define-key "C-h C-f" 'find-function)
(arki/define-key "C-h C-v" 'find-variable)
(arki/define-key "C-h C-k" 'find-function-on-key)


(provide 'init-elisp)
