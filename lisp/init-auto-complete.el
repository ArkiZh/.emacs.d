;; Input completion -- Completion of your input in the minibuffer. (See also Minibuffer Completion in the manual.)
(ido-mode t)
(setq ido-enable-flex-matching t)


;; ;; Config auto complete
;; (when (require-pack 'company)
;;   ;; 开启全局 Company 补全
;;   (global-company-mode 1)
;;   )
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

;; (when (require-pack 'counsel)
;;   ;; 配置counsel
;;   (arki/define-key "M-x" 'counsel-M-x)
;;   ;; (arki/define-key "C-x C-f" 'counsel-find-file)
;;   (arki/define-key "C-h f" 'counsel-describe-function)
;;   (arki/define-key "C-h v" 'counsel-describe-variable)
;;   ;; (arki/define-key "C-c p f" 'counsel-git) ;;查找当前所在git仓库管理的文件
;;   )


(provide 'init-auto-complete)
