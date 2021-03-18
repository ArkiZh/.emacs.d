;; https://github.com/justbur/emacs-which-key
(when (require-pack 'which-key)
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is triggered.
  (setq which-key-idle-delay 0.6)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  ;; Supported types are minibuffer, side-window, frame, and custom.
  (setq which-key-popup-type 'side-window)

  ;; location of which-key window. valid values: top, bottom, left, right, or a list of any of the two.
  (setq which-key-side-window-location 'bottom)
  ;; max width of which-key window, when displayed at left or right.
  (setq which-key-side-window-max-width 60)
  ;; max height of which-key window, when displayed at top or bottom.
  (setq which-key-side-window-max-height 10)
  )

(provide 'init-which-key)
