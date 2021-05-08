;; https://stackoverflow.com/questions/2736087/eval-after-load-vs-mode-hook
;; https://stackoverflow.com/questions/21880139/what-is-with-eval-after-load-in-emacs-lisp/21880276
;; (add-hook 'dired-mode-hook

(with-eval-after-load 'dired

  ;; https://github.com/purcell/diredfl
  ;; This is adapted from the extra font lock rules provided by Drew Adams' `dired+' package
  (when (require-pack 'diredfl)
    (add-hook 'dired-mode-hook 'diredfl-mode))
  
  ;; Dired config
  (setq dired-recursive-deletes 'always)	;Always delete recursively
  (setq dired-recursive-copies 'always)	;Always copy recursively
  ;; Reuse dired buffers. https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
  (put 'dired-find-alternate-file 'disabled nil)
  (arki/define-key "RET" 'dired-find-alternate-file 'dired-mode-map)
  (arki/define-key "^"
		   (lambda () (interactive) (find-alternate-file ".."))
		   'dired-mode-map)
  ;; https://emacs.stackexchange.com/questions/33548/how-show-size-in-kb-in-dired-mode
  (setq dired-listing-switches "-alh")
  ;; https://emacs.stackexchange.com/questions/36317/dired-first-show-list-of-folders
  ;; 加这个选项在wsl2 中没有反应：--group-directories-first
  ;; 在windows 环境可以用这个： (setq ls-lisp-dirs-first t)

  ;; (require 'dired-x) ;; Enable <C-x C-j> to open current file's directory
  (setq dired-dwim-target t) 		;当一个frame中存在多个window时，将下一个分屏自动设置成拷贝地址的目标
  
  (when (memq system-type '(gnu gnu/linux gnu/kfreebsd cygwin))
    ;; https://gitlab.com/xuhdev/dired-quick-sort#dired-quick-sort
    (when (require-pack 'dired-quick-sort)
      (dired-quick-sort-setup)
      (arki/define-key "S" 'hydra-dired-quick-sort/body 'dired-mode-map)
      )
    )
  )

(provide 'init-dired)
