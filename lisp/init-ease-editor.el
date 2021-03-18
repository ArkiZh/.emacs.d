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
  (setq avy-timeout-seconds 1)
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

;; --------------------------------------------------WINDOW MANAGER--------------------------------------------------
(when (require-pack 'ace-window)
  ;; 配置ace-windown
  (arki/define-key "M-p" 'ace-window)
  )

(require-pack 'transpose-frame)

;; 启用winner mode
(winner-mode 1)


;; 执行完命令后，直接使用C-g关闭辅助性的buffer
(when (require-pack 'popwin)
  ;; 配置popwin
  (popwin-mode 1)
  )


;; --------------------------------------------------EDIT TEXT--------------------------------------------------
;; 选中一段文字之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)


(when (require-pack 'hungry-delete)
  ;; 启用hungry-delete
  (global-hungry-delete-mode)
  )


(provide 'init-ease-editor)
