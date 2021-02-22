;; 设置默认字体为汉字字符集 找到字符类型的方式： M+x describe-font RET RET
;; (set-frame-font "-ADBO-Source Han Serif CN-normal-normal-normal-*-17-*-*-*-*-0-iso10646-1")
;; (set-frame-font "Source Han Serif CN")

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; (set-face-attribute 'default nil :height 125)


;;; base on https://gist.github.com/coldnew/7398835
(defvar emacs-font-english nil
  "The font name of English.")

(defvar emacs-font-chinese nil
  "The font name for Chinese.")

(defvar emacs-font-size-pair nil
  "Default font size pair for (english . chinese)")

(defvar emacs-font-size-pair-list nil
  "This list is used to store matching (englis . chinese) font-size.")

(defun font-exist-p (fontname)
  "test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

(defun set-font (font-english font-chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."
  (if (font-exist-p font-english)
      (set-frame-font (format "%s:pixelsize=%d" font-english (car size-pair)) t)
    (warn "Font for English doesn't exist, please install it: %s" font-english))

  (if (font-exist-p font-chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family font-chinese :size (cdr size-pair))))
    (warn "Font for Chinese doesn't exist, please install it: %s" font-chinese))
  )

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-font-english emacs-font-chinese emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(setq list-faces-sample-text
      (concat
       "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz\n"
       "11223344556677889900       壹貳參肆伍陸柒捌玖零"))

(when window-system
  ;; setup change size font, base on emacs-font-size-pair-list
  (arki/define-key "C-M-=" 'increase-emacs-font-size)
  (arki/define-key "C-M--" 'decrease-emacs-font-size)

  ;; setup default  English font and Chinese font
  (setq emacs-font-english "DejaVu Sans Mono") ;Source Code Pro
  (setq emacs-font-chinese "Source Han Serif CN")
  (setq emacs-font-size-pair '(17 . 18))
  (setq emacs-font-size-pair-list '(( 5 .  6) (10 . 12)
                                    (13 . 16) (15 . 18) (17 . 20)
                                    (19 . 22) (20 . 24) (21 . 26)
                                    (24 . 28) (26 . 32) (28 . 34)
                                    (30 . 36) (34 . 40) (36 . 44)))
  )
;; Setup font size based on emacs-font-size-pair
(set-font emacs-font-english emacs-font-chinese emacs-font-size-pair)

;; (when (require-pack 'cnfonts)
;;   ;; 配置cnfonts https://github.com/tumashu/cnfonts
;;   ;; 让 cnfonts 随着 Emacs 自动生效。
;;   (cnfonts-enable)
;;   (setq cnfonts-use-face-font-rescale t)
;;   (setq cnfonts-keep-frame-size nil))


(provide 'init-fonts)
