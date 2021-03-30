;; CSV
(require-pack 'csv-mode)

;; Markdown
;; https://github.com/jrblevin/markdown-mode
(when (require-pack 'markdown-mode)
  ;; 配置markdown-mode
  ;; 调用markdown-live-preview报错markdown failed with exit code 1解决方案：(参考自https://emacs-china.org/t/markdown/11399)
  ;; 安装pandoc，配置markdown-command指向pandoc。下载地址：https://pandoc.org/
  (add-hook 'markdown-mode-hook
	    (lambda () (setq markdown-command "pandoc")
	      ;; M-p 与跳转 ace window 冲突了，禁用掉
	      (arki/define-key "M-p" nil 'markdown-mode-map))
	    )
  ;; https://github.com/bmag/imenu-list
  (when (require-pack 'imenu-list)
    (add-hook 'markdown-mode-hook
	      (lambda ()
		(arki/define-key "C-'" 'imenu-list-smart-toggle 'markdown-mode-map)
		))
    (setq imenu-list-position 'left)
    (setq imenu-list-size 36)
    (setq imenu-list-focus-after-activation nil)
    (setq imenu-list-auto-resize nil)
    )
  )


;; Json
;; https://github.com/gongo/json-reformat
(require-pack 'json-reformat)


(provide 'init-filetypes-support)
