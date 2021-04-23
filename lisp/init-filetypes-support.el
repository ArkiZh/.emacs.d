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
  )


;; Json
;; https://github.com/gongo/json-reformat
(require-pack 'json-reformat)

;; Yaml
;;
(require-pack 'yaml)

;; Support epub ebooks
(when (require-pack 'nov)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

;; Support large files
;; https://github.com/m00natic/vlfi
(when (require-pack 'vlf)
  (custom-set-variables '(large-file-warning-threshold 20971520))
  (require 'vlf-setup))

(provide 'init-filetypes-support)
