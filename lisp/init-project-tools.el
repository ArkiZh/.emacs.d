;; --------------------------------------------------PROJECT UTILS--------------------------------------------------
(when (require-pack 'magit)
  ;; 配置magit
  (arki/define-key "C-c u g" 'magit-status))

;; https://github.com/Alexander-Miller/treemacs
(when (require-pack 'treemacs)
  ;; Let treemacs become invisible to commands like ‘other-window’ or ‘evil-window-left’.
  (setq treemacs-is-never-other-window t)
  ;; 打开treemmacs导航
  (arki/define-key "C-<f2>" 'treemacs)
  (arki/define-key "<f2>" 'treemacs-select-window)
  )

(when (require-pack 'projectile)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(when (require-packs 'treemacs 'projectile)
  (require-pack 'treemacs-projectile))

(provide 'init-project-tools)
