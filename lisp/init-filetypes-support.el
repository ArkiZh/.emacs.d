;; CSV
(require-pack 'csv-mode)

;; Markdown
(when (require-pack 'markdown-mode)
  ;; 配置markdown-mode
  ;; 调用markdown-live-preview报错markdown failed with exit code 1解决方案：(参考自https://emacs-china.org/t/markdown/11399)
  ;; 安装pandoc，配置markdown-command指向pandoc。下载地址：https://pandoc.org/
  (add-hook 'markdown-mode-hook
	    (lambda () (setq markdown-command "pandoc")))
  )

;; Json
;; https://github.com/gongo/json-reformat
(require-pack 'json-reformat)


(provide 'init-filetypes-support)
