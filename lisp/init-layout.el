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


;; https://github.com/nex3/perspective-el#buffer-switchers
(when (require-pack 'perspective)
  (persp-mode)
  (when (require 'bs)
    (arki/define-key "C-x C-b" (lambda (arg) (interactive "P")
				 (if (fboundp 'persp-bs-show)
                                     (persp-bs-show arg)
                                   (bs-show "all")))))
  (setq persp-state-default-file (expand-file-name "perspective.save" arki/cache-dir))
  (add-hook 'kill-emacs-hook (lambda () (interactive "P")
			       ;; Kill gpg buffers before save persp, to avoid password input when restart emacs.
			       (kill-matching-buffers ".*\\.gpg$" nil nil)
			       (persp-state-save persp-state-default-file)))
  (add-hook 'emacs-startup-hook (lambda () (interactive "P") (persp-state-load persp-state-default-file)))
  )

;; https://github.com/bmag/emacs-purpose
;; Config M-x customize-group purpose
;; (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;; (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;; (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;; (setq purpose-use-default-configuration t) ; not really necessary, default is t
;; (purpose-compile-user-configuration) ; activates your changes
;; (when (require-pack 'window-purpose)
  ;; (purpose-mode))


(provide 'init-layout)
