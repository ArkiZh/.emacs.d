(require 'org)

;; org-mode needs this
(require-pack 'htmlize)
(require-pack 'org-preview-html)

;; https://github.com/integral-dw/org-superstar-mode
(when (require-pack 'org-superstar)
  (add-hook 'org-mode-hook 'org-superstar-mode)
  (with-eval-after-load "org-superstar"
    (setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")))
  )

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-content 3)
	    
	    ;; Config view options
	    (setq line-spacing 0.1)
	    (setq org-src-fontily-natively t)
	    (org-indent-mode 1)
	    
	    ;; Config markup
	    (setq org-hide-emphasis-markers t) ;显示斜体、加粗之类的字体
	    (setq org-pretty-entities t)       ;显示上坡线指出的希腊字母、箭头图像之类的字符
	    (setq org-pretty-entities-include-sub-superscripts t) ;显示下角标、上角标
	    ;; When setting this variable to {}, ‘a_b’ is not interpreted as a subscript, but ‘a_{b}’ is
	    (setq org-use-sub-superscripts '{})			  ;限定_或者^后面跟着{}时候才使用上角标、下角标
	    ;; Add markup to text that spans more than two consecutive lines
	    ;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode/13828#13828
	    ;; Default value of org-emphasis-regexp-components: ("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
	    (setcar (nthcdr 4 org-emphasis-regexp-components) 3)
	    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

	    ;; Config export options
	    (setq org-export-with-sub-superscripts '{})		  ;设置导出时候，限定_或者^后面跟着{}时候才渲染上角标、下角标
	    (setq org-export-with-emphasis t)			  ;设置导出时候，渲染斜体、加粗之类的字体
	    ))

(when (display-graphic-p)
  ;; https://github.com/abo-abo/org-download
  (require-pack 'org-download)
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable))

;; (setq org-agenda-files '("~/org"))


(provide 'init-org)
