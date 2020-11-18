(require 'org)

(setq org-src-fontily-natively t)

;; (setq org-agenda-files '("~/org"))

;; (setq org-startup-indented t
;;       org-pretty-entities t
;;       ;; org-hide-emphasis-markers t  ;; show actually italicized text instead of /italicized text/
;;       org-fontify-whole-heading-line t
;;       org-fontify-done-headline t
;;       org-fontify-quote-and-verse-blocks t)


;; Add markup to text that spans more than two consecutive lines
;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode/13828#13828
;; Default value of org-emphasis-regexp-components: ("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
(setcar (nthcdr 4 org-emphasis-regexp-components) 3)
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(provide 'init-org)
