
;; https://github.com/emacs-dashboard/emacs-dashboard
(when (require-pack 'dashboard)
  ;; (dashboard-setup-startup-hook)
  (arki/define-key "C-c u d" 'dashboard-refresh-buffer)
  (setq dashboard-startup-banner nil) ;'official
  (when dashboard-startup-banner
    (setq dashboard-banner-logo-title "Nice day, isn't it?"))
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 25)
			  (projects . 25)
			  (bookmarks . 25)
			  ))
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-footer nil)
  )

(provide 'init-dashboard)
