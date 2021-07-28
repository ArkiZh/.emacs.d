(defgroup arki/input-method nil
  "Input method config, only for read."
  :group 'arki/config)


(defcustom arki/input-method-ciku (expand-file-name "ciku" arki/cache-dir) "Ciku directory for pyim." :group 'arki/input-method)

(setq arki/ziranma-scheme '(arki/ziranma-shuangpin
			    :document "自然码双拼方案。"
			    :class shuangpin
			    :first-chars "abcdefghijklmnopqrstuvwxyz"
			    :rest-chars "abcdefghijklmnopqrstuvwxyz"
			    :prefer-trigger-chars nil
			    :keymaps
			    (("a" "a" "a")
			     ("b" "b" "ou")
			     ("c" "c" "iao")
			     ("d" "d" "uang" "iang")
			     ("e" "e" "e")
			     ("f" "f" "en")
			     ("g" "g" "eng")
			     ("h" "h" "ang")
			     ("i" "ch" "i")
			     ("j" "j" "an")
			     ("k" "k" "ao")
			     ("l" "l" "ai")
			     ("m" "m" "ian")
			     ("n" "n" "in")
			     ("o" "o" "uo" "o")
			     ("p" "p" "un")
			     ("q" "q" "iu")
			     ("r" "r" "uan" "er")
			     ("s" "s" "iong" "ong")
			     ("t" "t" "ue" "ve")
			     ("u" "sh" "u")
			     ("v" "zh" "v" "ui")
			     ("w" "w" "ia" "ua")
			     ("x" "x" "ie")
			     ("y" "y" "uai" "ing")
			     ("z" "z" "ei")
			     ("aa" "a")
			     ("an" "an")
			     ("ai" "ai")
			     ("ao" "ao")
			     ("ah" "ang")
			     ("ee" "e")
			     ("ei" "ei")
			     ("en" "en")
			     ("er" "er")
			     ("eg" "eng")
			     ("oo" "o")
			     ("ou" "ou"))))

;; https://github.com/tumashu/pyim
(when (require-pack 'pyim)
  ;; (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
  ;; (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
  (setq default-input-method "pyim")
  (pyim-scheme-add arki/ziranma-scheme)
  (setq pyim-enable-shortcode nil)
  (setq pyim-default-scheme 'arki/ziranma-shuangpin)
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
  ;; https://github.com/tumashu/pyim/issues/397
  ;; pyim-convert-code-at-point 已废弃
  (arki/define-key "M-i" 'pyim-convert-string-at-point)

  (defun arki/input-method--get-ciku-files ()
    "Get pyim ciku files from directory `arki/input-method-ciku'`"
    (interactive)
    (when (file-exists-p arki/input-method-ciku)
      (directory-files-recursively arki/input-method-ciku ".pyim$")))

  (dolist (cur-file (arki/input-method--get-ciku-files))
    (add-to-list 'pyim-dicts (list :name (file-name-base cur-file) :file cur-file)))

  (setq pyim-fuzzy-pinyin-alist nil)
  )

(provide 'init-input-method)
