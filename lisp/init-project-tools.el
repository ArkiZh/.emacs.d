;; --------------------------------------------------PROJECT UTILS--------------------------------------------------
(when (require-pack 'magit)
  ;; 配置magit
  (arki/define-key "C-c u g" 'magit-status))

(when (require-pack 'treemacs)
  ;; 打开treemmacs导航
  (arki/define-key "M-0" 'treemacs))

(when (require-pack 'projectile)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(provide 'init-project-tools)
