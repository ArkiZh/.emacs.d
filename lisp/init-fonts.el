;; 设置默认字体为汉字字符集 找到字符类型的方式： M+x describe-font RET RET
;; (set-frame-font "-ADBO-Source Han Serif CN-normal-normal-normal-*-17-*-*-*-*-0-iso10646-1")
;; (set-frame-font "Source Han Serif CN")

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; (set-face-attribute 'default nil :height 125)

;; 借鉴自：
;; https://gist.github.com/Superbil/7113937
;; base on https://gist.github.com/coldnew/7398835

(defgroup arki/font nil
  "Font config, only for read."
  :group 'arki/config)

(defcustom arki/font-english nil
  "The font name of English."
  :group 'arki/font)

(defcustom arki/font-chinese nil
  "The font name for Chinese."
  :group 'arki/font)

(defcustom arki/font-size-pair nil
  "Default font size pair for (english . chinese)"
  :group 'arki/font)

(defcustom arki/font-size-pair-list nil
  "This list is used to store matching (englis . chinese) font-size."
  :group 'arki/font)

(defcustom arki/font-english "DejaVu Sans Mono"
  "English font"
  :group 'arki/font)

(defcustom arki/font-english-size 20
  "English font size"
  :group 'arki/font)

(defcustom arki/font-chinese "Source Han Serif CN"
  "Chinese font"
  :group 'arki/font)

(defcustom arki/font-chinese-size 16
  "Chinese font size"
  :group 'arki/font)

(defcustom arki/font-chinese-extra "Source Han Serif CN"
  "Chinese extra font"
  :group 'arki/font)

(defcustom arki/font-chinese-extra-size 16
  "Chinese extra font size"
  :group 'arki/font)


(defcustom arki/font-symbol "FontAwesome"
  "Symbol font"
  :group 'arki/font)

(defcustom arki/font-symbol-size 16
  "Symbol font size"
  :group 'arki/font)


(defun font-exist-p (fontname)
  "test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

(defun arki/set-font-english (font-name font-size)
  "Set English font"
  (if (not (font-exist-p font-name))
      (warn "Font for English doesn't exist, please install it: %s" font-name)
    ;; (set-face-attribute 'default nil :font (font-spec :family font-name :size font-size))
    (set-frame-font (font-spec :family font-name :size font-size) t nil)
    (setq arki/font-english font-name)
    (setq arki/font-english-size font-size)
    )
  )

(defun arki/set-font-chinese (font-name font-size)
  "Set Chinese font"
  (if (not (font-exist-p font-name))
      (warn "Font for Chinese doesn't exist, please install it: %s" font-name)
    ;; 设置中文字体，注意，不要使用 'unicode charset,否则上面的英文字体设置将会失效。
    (dolist (charset '(kana han cjk-misc bopomofo gb18030))
      (set-fontset-font "fontset-default" charset (font-spec :family font-name :size font-size)))
    (setq arki/font-chinese font-name)
    (setq arki/font-chinese-size font-size)
    )
  )

(defun arki/set-font-chinese-extra (font-name font-size)
  "Set Chinese extra font

设置 fallback 字体，用于显示不常用的字符"
  (if (not (font-exist-p font-name))
      (warn "Font for Chinese extra doesn't exist, please install it: %s" font-name)
    (set-fontset-font "fontset-default" nil (font-spec :family font-name :size font-size) nil 'prepend)
    (setq arki/font-chinese-extra font-name)
    (setq arki/font-chinese-extra-size font-size)
    )
  )


(defun arki/set-font-symbol (font-name font-size)
  "Set symbol font"
  (if (not (font-exist-p font-name))
      (warn "Font for symbol doesn't exist, please install it: %s" font-name)
    (set-fontset-font "fontset-default" 'symbol (font-spec :family font-name :size font-size))
    (setq arki/font-symbol font-name)
    (setq arki/font-symbol-size font-size)
    )
  )


(defun arki/font-step (step-size)
  )


(defun set-font (font-english font-chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."
  (if (font-exist-p font-english)
      ;; set-frame-font: When called from Lisp, FONT should be a font
      ;; name (a string), a font object, font entity, or font spec.
      (set-frame-font (format "%s:pixelsize=%d" font-english (car size-pair)) t)
    (warn "Font for English doesn't exist, please install it: %s" font-english))

  (if (font-exist-p font-chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family font-chinese :size (cdr size-pair))))
    (warn "Font for Chinese doesn't exist, please install it: %s" font-chinese))
  )


;; (defun arki/font-step-chinese (step)
;;   "Increase or decrease Chinese font"
;;   (let ((new-size (+ step arki/font-chinese-size)))
;;     (if (not (font-exist-p arki/font-chinese))
;; 	(warn "Font for Chinese doesn't exist, please install it: %s" arki/font-chinese)
;;       (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;         (set-fontset-font (frame-parameter nil 'font) charset
;;                           (font-spec :family arki/font-chinese :size new-size)))
;;       (setq arki/font-chinese-size new-size)
;;       ))
;;   )

;; 测试一下
;; abcdef
;; (arki/font-step-chinese 0.5)

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps arki/font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq arki/font-size-pair
          (or (cadr (member arki/font-size-pair scale-steps))
              arki/font-size-pair))
    (when arki/font-size-pair
      (message "emacs font size set to %.1f" (car arki/font-size-pair))
      (set-font arki/font-english arki/font-chinese arki/font-size-pair))))

(defun increase-emacs-font-size ()
  "Decrease emacs's font-size acording arki/font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Increase emacs's font-size acording arki/font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(setq list-faces-sample-text
      (concat
       "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz\n"
       "11223344556677889900       壹貳參肆伍陸柒捌玖零"))

;; Only useful when has window server
(when window-system
  ;; setup change size font, base on arki/font-size-pair-list
  (arki/define-key "C-M-=" 'increase-emacs-font-size)
  (arki/define-key "C-M--" 'decrease-emacs-font-size)

  ;; setup default  English font and Chinese font
  (setq arki/font-english "DejaVu Sans Mono") ;Source Code Pro
  (setq arki/font-chinese "Source Han Serif CN")
  (setq arki/font-size-pair '(16 . 20))
  (setq arki/font-size-pair-list '(( 5 .  6)
				   (10 . 12)
                                   (13 . 16)
				   (15 . 18)
				   (16 . 20)
				   (17 . 20)
                                   (19 . 22)
				   (20 . 24)
				   (21 . 26)
                                   (24 . 28)
				   (26 . 32)
				   (28 . 34)
                                   (30 . 36)))
  ;; Setup font size based on arki/font-size-pair
  (set-font arki/font-english arki/font-chinese arki/font-size-pair)
  (set-font arki/font-english arki/font-chinese '(15 . 13))
  )

;; 有空时候看看这个用于org-mode 表格对齐的：https://github.com/chen-chao/zh-align.el

;; (when (require-pack 'cnfonts)
;;   ;; 配置cnfonts https://github.com/tumashu/cnfonts
;;   ;; 让 cnfonts 随着 Emacs 自动生效。
;;   (cnfonts-enable)
;;   (setq cnfonts-use-face-font-rescale t)
;;   (setq cnfonts-keep-frame-size nil))

;; +----------------------------------------------------+
;; | [*9.0-18*] [ 20-24 ] [ 26-28 ] [ -30- ] [ -32- ]   |
;; | 中英文等宽对齐设置：按加号或减号按钮直至此表格对齐 |
;; | abcdefjhijklmnoprqstuvwxwyABCDEFJHIJkLMNOPQRSTUVXW |
;; | 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄄𠄅𠄆𠄇𠄇𠄆 |
;; | 英文字号   中文对齐设置    EXT-B 对齐设置    测试  |
;; +----------------------------------------------------+

(provide 'init-fonts)
