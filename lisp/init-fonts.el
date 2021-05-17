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
  "English font"
  :group 'arki/font)

(defcustom arki/font-english-size 16
  "English font size"
  :group 'arki/font)

(defcustom arki/font-chinese nil
  "Chinese font"
  :group 'arki/font)

(defcustom arki/font-chinese-size 16
  "Chinese font size"
  :group 'arki/font)

(defcustom arki/font-chinese-extra nil
  "Chinese extra font"
  :group 'arki/font)

(defcustom arki/font-chinese-extra-size 16
  "Chinese extra font size"
  :group 'arki/font)

(defcustom arki/font-symbol nil
  "Symbol font"
  :group 'arki/font)

(defcustom arki/font-symbol-size 16
  "Symbol font size"
  :group 'arki/font)

(defcustom arki/font-size-min 5
  "Min font size"
  :group 'arki/font)

(defcustom arki/font-size-max 50
  "Max font size"
  :group 'arki/font)

(defcustom arki/font-size-step 1
  "Step size for font adjust"
  :group 'arki/font)



(defun font-exist-p (fontname)
  "test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))


(defun arki/set-font-english (font-name font-size)
  "Set English font"
  (when font-name
    (if (not (font-exist-p font-name))
	(warn "Font for English doesn't exist, please install it: %s" font-name)
      (if (or (< font-size arki/font-size-min) (> font-size arki/font-size-max))
	  (message "Font size [%S] is not in the range of: [%S, %S]" font-size arki/font-size-min arki/font-size-max)
	;; (set-face-attribute 'default nil :font (font-spec :family font-name :size font-size))
	(set-frame-font (font-spec :family font-name :size font-size) t nil)      
	(setq arki/font-english font-name)
	(setq arki/font-english-size font-size)
	(message "Set English font to: %S Size: %S" font-name font-size)
	))))


(defun arki/set-font-chinese (font-name font-size)
  "Set Chinese font"
  (when font-name
    (if (not (font-exist-p font-name))
	(warn "Font for Chinese doesn't exist, please install it: %s" font-name)
      (if (or (< font-size arki/font-size-min) (> font-size arki/font-size-max))
	  (message "Font size [%S] is not in the range of: [%S, %S]" font-size arki/font-size-min arki/font-size-max)
	;; 设置中文字体，注意，不要使用 'unicode charset,否则上面的英文字体设置将会失效。
	(dolist (charset '(kana han cjk-misc bopomofo gb18030))
	  (set-fontset-font "fontset-default" charset (font-spec :family font-name :size font-size)))
	(setq arki/font-chinese font-name)
	(setq arki/font-chinese-size font-size)
	(message "Set Chinese font to: %S Size: %S" font-name font-size)
	))))

(defun arki/set-font-chinese-extra (font-name font-size)
  "Set Chinese extra font

设置 fallback 字体，用于显示不常用的字符"
  (when font-name
    (if (not (font-exist-p font-name))
	(warn "Font for Chinese extra doesn't exist, please install it: %s" font-name)
      (if (or (< font-size arki/font-size-min) (> font-size arki/font-size-max))
	  (message "Font size [%S] is not in the range of: [%S, %S]" font-size arki/font-size-min arki/font-size-max)
	(set-fontset-font "fontset-default" nil (font-spec :family font-name :size font-size) nil 'prepend)
	(setq arki/font-chinese-extra font-name)
	(setq arki/font-chinese-extra-size font-size)
	(message "Set Chinese extra font to: %S Size: %S" font-name font-size)
	))))


(defun arki/set-font-symbol (font-name font-size)
  "Set symbol font"
  (when font-name
    (if (not (font-exist-p font-name))
	(warn "Font for symbol doesn't exist, please install it: %s" font-name)
      (if (or (< font-size arki/font-size-min) (> font-size arki/font-size-max))
	  (message "Font size [%S] is not in the range of: [%S, %S]" font-size arki/font-size-min arki/font-size-max)
	(set-fontset-font "fontset-default" 'symbol (font-spec :family font-name :size font-size))
	(setq arki/font-symbol font-name)
	(setq arki/font-symbol-size font-size)
	(message "Set symbol font to: %S Size: %S" font-name font-size)      
	))))


(defun arki/font-step (step-size font-type)
  "Set font size by step.

font-type: 1 for English font. 2 Chinese. 3 Chinese-extra. 4 symbol"
  
  (cond
   ((equal font-type 1) (arki/set-font-english arki/font-english (+ arki/font-english-size step-size)))
   ((equal font-type 2) (arki/set-font-chinese arki/font-chinese (+ arki/font-chinese-size step-size)))
   ((equal font-type 3) (arki/set-font-chinese-extra arki/font-chinese-extra (+ arki/font-chinese-extra-size step-size)))
   ((equal font-type 4) (arki/set-font-symbol arki/font-symbol (+ arki/font-symbol-size step-size)))
   (t (warn "Unsupported font-type: %s" font-type))
   )
  )


(defun arki/font-family-suggest (&optional font-type)
  "Suggest font families available in minibuffer.

font-type: 1 for English font. 2 Chinese. 3 Chinese-extra. 4 symbol"
  (interactive)
  (let* ((font-type-name (cond
			  ((equal font-type 1) " for English")
			  ((equal font-type 2) " for Chinese")
			  ((equal font-type 3) " for Chinese extra")
			  ((equal font-type 4) " for symbol")
			  (t "")
			  ))
	 (prev-font-name (cond
			  ((equal font-type 1) arki/font-english)
			  ((equal font-type 2) arki/font-chinese)
			  ((equal font-type 3) arki/font-chinese-extra)
			  ((equal font-type 4) arki/font-symbol)
			  (t "")
			  ))
	 (font-selected (ido-completing-read
			 (format "Select font%s: " font-type-name)
			 (delete-dups (font-family-list))
			 nil nil prev-font-name
			 )))
    (if (equal font-selected "nil")
	nil
      font-selected)
    )
  )


(defun arki/font-family-select (font-type)
  "Set font family.

font-type: 1 for English font. 2 Chinese. 3 Chinese-extra. 4 symbol"
  (if (not (memq font-type '(1 2 3 4)))
      (warn "Unsupported font-type: %s" font-type)
    (let ((suggested-font (arki/font-family-suggest font-type)))
      (cond
       ((equal font-type 1) (arki/set-font-english suggested-font arki/font-english-size))
       ((equal font-type 2) (arki/set-font-chinese suggested-font arki/font-chinese-size))
       ((equal font-type 3) (arki/set-font-chinese-extra suggested-font arki/font-chinese-extra-size))
       ((equal font-type 4) (arki/set-font-symbol suggested-font arki/font-symbol-size))
       )
      )))


(defun arki/set-font ()
  "Set all the font families and sizes"
  (arki/set-font-english arki/font-english arki/font-english-size)
  (arki/set-font-chinese arki/font-chinese arki/font-chinese-size)
  (arki/set-font-chinese-extra arki/font-chinese-extra arki/font-chinese-extra-size)
  (arki/set-font-symbol arki/font-symbol arki/font-symbol-size)
  )


(defun arki/save-font ()
  (message "Fonts config saving...")
  (customize-save-variable 'arki/font-english arki/font-english)
  (customize-save-variable 'arki/font-english-size arki/font-english-size)
  (customize-save-variable 'arki/font-chinese arki/font-chinese)
  (customize-save-variable 'arki/font-chinese-size arki/font-chinese-size)
  (customize-save-variable 'arki/font-chinese-extra arki/font-chinese-extra)
  (customize-save-variable 'arki/font-chinese-extra-size arki/font-chinese-extra-size)
  (customize-save-variable 'arki/font-symbol arki/font-symbol)
  (customize-save-variable 'arki/font-symbol-size arki/font-symbol-size)
  (message "Fonts config saved." )
  )

(defun arki/font-adjust ()
  (interactive)
  (message "Set font: ENG(1 q+ a-) ZH(2 w+ s-) EXT(3 e+ d-) Symbol(4 r+ f-). Save(0)")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (vector (list ?1))
       (lambda () (interactive) (arki/font-family-select 1) (arki/font-adjust)))
     (define-key map (vector (list ?q))
       (lambda () (interactive) (arki/font-step arki/font-size-step 1) (arki/font-adjust)))
     (define-key map (vector (list ?a))
       (lambda () (interactive) (arki/font-step (- arki/font-size-step) 1) (arki/font-adjust)))

     (define-key map (vector (list ?2))
       (lambda () (interactive) (arki/font-family-select 2) (arki/font-adjust)))
     (define-key map (vector (list ?w))
       (lambda () (interactive) (arki/font-step arki/font-size-step 2) (arki/font-adjust)))
     (define-key map (vector (list ?s))
       (lambda () (interactive) (arki/font-step (- arki/font-size-step) 2) (arki/font-adjust)))

     (define-key map (vector (list ?3))
       (lambda () (interactive) (arki/font-family-select 3) (arki/font-adjust)))
     (define-key map (vector (list ?e))
       (lambda () (interactive) (arki/font-step arki/font-size-step 3) (arki/font-adjust)))
     (define-key map (vector (list ?d))
       (lambda () (interactive) (arki/font-step (- arki/font-size-step) 3) (arki/font-adjust)))

     (define-key map (vector (list ?4))
       (lambda () (interactive) (arki/font-family-select 4) (arki/font-adjust)))
     (define-key map (vector (list ?r))
       (lambda () (interactive) (arki/font-step arki/font-size-step 4) (arki/font-adjust)))
     (define-key map (vector (list ?f))
       (lambda () (interactive) (arki/font-step (- arki/font-size-step) 4) (arki/font-adjust)))

     (define-key map (vector (list ?0))
       (lambda () (interactive) (arki/save-font) (arki/font-adjust)))     

     map))
  )


(when (display-graphic-p)
  (arki/define-key "f" 'arki/font-adjust 'arki/prefix-keymap)
  (add-hook 'after-init-hook (lambda () (interactive) (arki/set-font)))
  )



;; +----------------------------------------------------+
;; | [*9.0-18*] [ 20-24 ] [ 26-28 ] [ -30- ] [ -32- ]   |
;; | 中英文等宽对齐设置：按加号或减号按钮直至此表格对齐 |
;; | abcdefjhijklmnoprqstuvwxwyABCDEFJHIJkLMNOPQRSTUVXW |
;; | 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄄𠄅𠄆𠄇𠄇𠄆 |
;; | 英文字号   中文对齐设置    EXT-B 对齐设置    测试  |
;; +----------------------------------------------------+

;; 有空时候看看这个用于org-mode 表格对齐的：https://github.com/chen-chao/zh-align.el

;; (when (require-pack 'cnfonts)
;;   ;; 配置cnfonts https://github.com/tumashu/cnfonts
;;   ;; 让 cnfonts 随着 Emacs 自动生效。
;;   (cnfonts-enable)
;;   (setq cnfonts-use-face-font-rescale t)
;;   (setq cnfonts-keep-frame-size nil))


(provide 'init-fonts)
