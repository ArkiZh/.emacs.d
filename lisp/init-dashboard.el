
;; https://github.com/emacs-dashboard/emacs-dashboard
(when (require-pack 'dashboard)
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-banner-logo-title "Nice day, isn't it?")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 15)
			  (projects . 5)
			  (bookmarks . 5)
			  ))
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-footer nil)
  )

(provide 'init-dashboard)
