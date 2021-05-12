;; --------------------------------------------------PROJECT UTILS--------------------------------------------------
(when (require-pack 'magit)
  ;; 配置magit
  (arki/define-key "C-c u g" 'magit-status)
  (defun arki/push-current-branch-to-all-remotes ()
    (interactive)
    (let* ((cur-branch (magit-get-current-branch))
	   ;; Avoid pushing to different remote branches.
	   ;; Or ensure pushing to the remote branch with the same name, create it if not exist.
	   ;; (remote-names (magit-list-remote-branch-names))
	   (remote-names (--map (concat it "/" cur-branch) (magit-list-remotes)))
	   (success-push '())
	   (failed-push '()))
      (dolist (remote-name remote-names)
	(condition-case err (progn
			      (message "Pushing %S to %S" cur-branch remote-name)
			      (magit-git-push cur-branch remote-name nil)
			      (setq success-push (append success-push (list remote-name)))
			      )
	  (error
	   (message "Failed to push %S to %S, due to ERROR: %S" cur-branch remote-name err)
	   (setq failed-push (append failed-push (list remote-name))))
	  ))
      (message "Push %s invoked. Success: %s Failed: %s. Wait magit to finish it." cur-branch success-push failed-push)
      t
      ))
  (arki/define-key "H" 'arki/push-current-branch-to-all-remotes 'magit-status-mode-map)
  )


;; https://github.com/Alexander-Miller/treemacs
(when (require-pack 'treemacs)
  ;; Let treemacs become invisible to commands like ‘other-window’ or ‘evil-window-left’.
  (setq treemacs-is-never-other-window t)
  ;; 打开treemmacs导航
  (arki/define-key "C-<f2>" 'treemacs-select-window)
  (arki/define-key "<f2>" 'treemacs)
  )

(when (require-pack 'projectile)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(when (require-packs 'treemacs 'projectile)
  (require-pack 'treemacs-projectile))

(provide 'init-project-tools)
