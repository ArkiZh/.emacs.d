
;; https://github.com.cnpmjs.org/xuchunyang/youdao-dictionary.el
(when (require-pack 'youdao-dictionary)

  (defun youdao-dictionary-search-from-region-or-input (arg)
    "With C-u, search from input. Else: search at point or region."
    (interactive "p")
    (cond ((= arg 4) (call-interactively 'youdao-dictionary-search-from-input))
	  (t (call-interactively 'youdao-dictionary-search-at-point))))

  (arki/define-key "d" 'youdao-dictionary-search-from-region-or-input 'arki/prefix-keymap)
  (setq youdao-dictionary-search-history-file (expand-file-name ".youdao-history" arki/cache-dir))

  ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
  (when (require-pack 'popwin) (push "*Youdao Dictionary*" popwin:special-display-config))
  
  )


(provide 'init-dictionary)
