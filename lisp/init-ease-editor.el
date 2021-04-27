;; --------------------------------------------------NAVIGATION--------------------------------------------------
;; Config editor navigation

;; ;; https://github.com/winterTTr/ace-jump-mode
;; ;; https://www.emacswiki.org/emacs/AceJump
;; (when (require-pack 'ace-jump-mode)
;;   (arki/define-key "C-c SPC" 'ace-jump-mode)
;;   (eval-after-load "ace-jump-mode"
;;     '(ace-jump-mode-enable-mark-sync))
;;   (arki/define-key "C-x SPC" 'ace-jump-mode-pop-mark))

;; https://github.com/abo-abo/avy
(when (require-pack 'avy)
  (arki/define-key "C-s" 'avy-goto-char-timer)
  (setq avy-timeout-seconds 0.35)
  (eval-after-load "isearch"
    '(arki/define-key "C-'" 'avy-isearch 'isearch-mode-map))
  ;; (global-set-key (kbd "C-:") 'avy-goto-char)
  ;; (global-set-key (kbd "C-'") 'avy-goto-char-2)
  ;; (global-set-key (kbd "M-g f") 'avy-goto-line)
  ;; (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  ;; (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  )

;; Move current line or region with M-up or M-down.
(when (require-pack 'move-text)
  ;; 配置move-text. Use default bindings for move-text-up and move-text-down (M-up / M-down).
  ;; (move-text-default-bindings)
  (arki/define-key "M-<down>" 'move-text-down)
  (arki/define-key "M-<up>"   'move-text-up)
  )

;; https://github.com/magnars/expand-region.el
(when (require-pack 'expand-region)
  (arki/define-key "C-=" 'er/expand-region)
  (arki/define-key "C-+" 'er/contract-region))


;; --------------------------------------------------EDIT TEXT--------------------------------------------------
;; 选中一段文字之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)


(when (require-pack 'hungry-delete)
  ;; 启用hungry-delete
  (global-hungry-delete-mode)
  )

(put 'upcase-region 'disabled nil)

;; browse-kill-ring
;; https://github.com/browse-kill-ring/browse-kill-ring
(when (require-pack 'browse-kill-ring)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-inserted-item t)
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-show-preview nil))

;; bbyac
;; https://github.com/baohaojun/bbyac
(when (require-pack 'browse-kill-ring)
  (when (require-pack 'bbyac)
    ;; Type a little bit and press M-g <return> to complete a word or M-s <return> to complete an arbitrary string.
    (bbyac-global-mode 1)))

;; --------------------------------------------------SHOW OUTLINE--------------------------------------------------

;; https://github.com/bmag/imenu-list
(when (require-pack 'imenu-list)
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (arki/define-key "C-'" 'imenu-list-smart-toggle 'markdown-mode-map)
	      ))
  (add-hook 'org-mode-hook
	    (lambda ()
	      ;; (arki/define-key "C-'" nil 'org-mode-map)
	      (arki/define-key "C-'" 'imenu-list-smart-toggle 'org-mode-map)
	      ))
  (setq imenu-list-position 'left)
  (setq imenu-list-size 36)
  (setq imenu-list-focus-after-activation nil)
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-after-jump-hook nil)
  (add-hook 'imenu-list-after-jump-hook (lambda () (recenter-top-bottom 0)))
  )


(provide 'init-ease-editor)
