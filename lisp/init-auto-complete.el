;; Input completion -- Completion of your input in the minibuffer. (See also Minibuffer Completion in the manual.)
(ido-mode t)
(setq ido-enable-flex-matching t)


;; https://github.com/joaotavora/yasnippet
(when (require-pack 'yasnippet)
  (yas-global-mode 1)
  ;; https://github.com/AndreaCrotti/yasnippet-snippets
  (require-pack 'yasnippet-snippets)
  )


;; Config company

;; https://www.youtube.com/watch?v=zSPraaX2524
;; https://github.com/tonyaldon/emacs.d/blob/master/settings/settings/setup-completion.el

;; https://www.youtube.com/watch?v=oyockkWcHp0
;; https://github.com/jerryhsieh/Emacs-config/blob/company/init.el
(when (require-pack 'company)
  ;; 开启全局 Company 补全
  (global-company-mode 1)

  (setq company-idle-delay 0.1)
  (make-variable-buffer-local 'company-idle-delay)
  
  (setq company-selection-wrap-around t)
  (setq company-tooltip-limit 9)
  (arki/define-key ">" 'company-filter-candidates 'company-active-map)

  (setq company-minimum-prefix-length 1)
  (make-variable-buffer-local 'company-minimum-prefix-length)

  ;; company-yasnippet must work with yasnippet and yasnippet-snippets
  (setq company-backends
        '((company-files company-keywords company-capf company-yasnippet)
          (company-abbrev company-dabbrev)))
  ;; (make-variable-buffer-local 'company-backends)
  
  ;; (setq company-transformers '(company-sort-by-backend-importance))

  (defun company-emacs-lisp-mode()
    "Set up `company-mode'  for `emacs-lisp-model'."
    (set (make-local-variable 'company-backends)
	 '((company-yasnippet
	    company-elisp
	    company-dabbrev-code
	    company-files))))
  (add-hook 'emacs-lisp-mode-hook 'company-emacs-lisp-mode)

  (defun company-text-mode ()
    "Set up `company-mode' for `text-mode'."
    (set (make-local-variable 'company-backends)
	 '((company-files) company-dabbrev))
    (setq company-minimum-prefix-length 3))
  (add-hook 'text-mode-hook 'company-text-mode)

  (defun company-shell-mode ()
    "Set up `company-mode' for `shell-mode'"
    (message "SHELL mode company")
    (set (make-local-variable 'company-backends)
	 '((company-files)))
	 (setq company-minimum-prefix-length 1)
	 (setq company-idle-delay 0.1))
  (add-hook 'shell-mode-hook 'company-shell-mode)
  )


;; ;; 调整hippie-expand调用的自动补全功能顺序
;; (setq hippie-expand-try-functions-list '(
;; 					 try-expand-dabbrev
;; 					 try-expand-dabbrev-all-buffers
;; 					 try-expand-dabbrev-from-kill
;; 					 try-complete-file-name-partially
;; 					 try-complete-file-name
;; 					 try-expand-all-abbrevs
;; 					 try-expand-list
;; 					 try-expand-line
;; 					 try-complete-lisp-symbol-partially
;; 					 try-complete-lisp-symbol))
;; ;; 使用hippie-expand增强自动补全
;; (arki/define-key "M-RET" 'hippie-expand)


;; (when (require-pack 'swiper)
;;   ;; 配置swiper
;;   (ivy-mode 1)
;;   ;; (setq ivy-use-virtual-buffers nil) ;I dont't need virtual buffer for now, so commented out.
;;   (setq enable-recursive-minibuffers t)
;;   ;; enable this if you want `swiper' to use it
;;   ;; (setq search-default-mode #'char-fold-to-regexp)
;;   (arki/define-key "C-s" 'swiper)
;;   ;; minibuffer 里面的 M-i 绑定 与输入法的冲突了，解绑掉
;;   (arki/define-key "M-i" nil 'ivy-minibuffer-map)
;;   )

(when (require-pack 'counsel)
  ;; 配置counsel
  (arki/define-key "M-x" 'counsel-M-x)
  ;; (arki/define-key "C-x C-f" 'counsel-find-file)
  ;; (arki/define-key "C-h f" 'counsel-describe-function)
  ;; (arki/define-key "C-h v" 'counsel-describe-variable)
  ;; (arki/define-key "C-c p f" 'counsel-git) ;;查找当前所在git仓库管理的文件
  )


(provide 'init-auto-complete)
