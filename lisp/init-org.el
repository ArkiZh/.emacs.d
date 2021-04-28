(require 'org)

;; org-mode needs this
(require-pack 'htmlize)
(require-pack 'org-preview-html)

;; https://github.com/integral-dw/org-superstar-mode
(when (require-pack 'org-superstar)
  (add-hook 'org-mode-hook 'org-superstar-mode)
  (with-eval-after-load "org-superstar"
    (setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq org-src-fontily-natively t)
	    ;; Add markup to text that spans more than two consecutive lines
	    ;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode/13828#13828
	    ;; Default value of org-emphasis-regexp-components: ("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
	    (setcar (nthcdr 4 org-emphasis-regexp-components) 3)
	    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
	    )
	  )
(when (display-graphic-p)
  ;; https://github.com/abo-abo/org-download
  (require-pack 'org-download)
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable))

;; Add markup to text that spans more than two consecutive lines
;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode/13828#13828
;; Default value of org-emphasis-regexp-components: ("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
(setcar (nthcdr 4 org-emphasis-regexp-components) 3)
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; (arki/define-key "C-c u a" 'org-agenda)

;; (setq org-agenda-files '("~/org"))

;; (setq org-startup-indented t
;;       org-pretty-entities t
;;       ;; org-hide-emphasis-markers t  ;; show actually italicized text instead of /italicized text/
;;       org-fontify-whole-heading-line t
;;       org-fontify-done-headline t
;;       org-fontify-quote-and-verse-blocks t)

;; (setq org-agenda-files '("~/org"))

;; (setq org-startup-indented t
;;       org-pretty-entities t
;;       ;; org-hide-emphasis-markers t  ;; show actually italicized text instead of /italicized text/
;;       org-fontify-whole-heading-line t
;;       org-fontify-done-headline t
;;       org-fontify-quote-and-verse-blocks t)


(provide 'init-org)
