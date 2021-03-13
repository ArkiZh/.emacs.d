;; https://github.com/tumashu/pyim
(when (require-pack 'pyim)
  (require 'pyim)
  ;; (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
  ;; (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置  
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'ziranma-shuangpin)
  (setq pyim-page-length 9)
  ;; 设置显示候选词的框是什么形式的
  (if (memq window-system '(x win32 ns))
      (when (require-pack 'posframe)
	(setq pyim-page-tooltip 'posframe))
    (setq pyim-page-tooltip 'popup)
    )

  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
		  pyim-probe-auto-english))
  (arki/define-key "M-i" 'pyim-convert-code-at-point)
  )

(provide 'init-input-method)
