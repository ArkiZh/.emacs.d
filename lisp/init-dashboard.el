
;; https://github.com/emacs-dashboard/emacs-dashboard
(when (require-pack 'dashboard)
  ;; (dashboard-setup-startup-hook)
  (arki/define-key "o" 'dashboard-refresh-buffer 'arki/prefix-keymap)
  (setq dashboard-startup-banner nil) ;'official
  (when dashboard-startup-banner
    (setq dashboard-banner-logo-title "Nice day, isn't it?"))
  (setq dashboard-center-content t)
  (setq dashboard-items '((bookmarks . 25)
			  (projects . 25)
			  (registers . 25)
			  (recents . 50)
			  ))
  (setq dashboard-show-shortcuts t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-footer nil)
  )

(provide 'init-dashboard)
