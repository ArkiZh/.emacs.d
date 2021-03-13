;; --------------------------------------------------NAVIGATION--------------------------------------------------
;; Config editor navigation

;; Move current line or region with M-up or M-down.
(when (require-pack 'move-text)
  ;; 配置move-text. Use default bindings for move-text-up and move-text-down (M-up / M-down).
  ;; (move-text-default-bindings)
  (arki/define-key "M-<down>" 'move-text-down)
  (arki/define-key "M-<up>"   'move-text-up)
  )

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
