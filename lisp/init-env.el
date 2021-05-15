
;; let emacs could find the executable
(when (memq window-system '(mac ns))
  (when (require-pack 'exec-path-from-shell)
    (exec-path-from-shell-initialize))
  )


(provide 'init-env)
