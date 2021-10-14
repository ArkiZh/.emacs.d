;;; init.el start
;; Some config structure learnt from https://github.com/purcell/emacs.d

;; ----------------------------------------------------------------------------------------------------
;; WARM UP
;; ----------------------------------------------------------------------------------------------------
;; Check emacs version
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(let ((tested-ver "27.1"))
  (when (version< emacs-version tested-ver)
    (message "This config has been tested under version %s. But yours is %s. Upgrade if possible." tested-ver emacs-version)))

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

;; Adjust garbage collection thresholds during startup, and thereafter
;; In bytes
(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Setup mirror from https://elpa.emacs-china.org/
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
			 ("melpa" . "https://elpa.emacs-china.org/melpa/")))


;;(pp package-enable-at-startup) ;; t
;;(pp load-path) ;; For now, equal to subdirs of package-directory-list
;; See: the last 6 paragraphs of https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html#Package-Installation
;; your init file should call the function package-initialize. It is up to you to ensure that relevant user options, such as package-load-list (see below), are set up prior to the package-initialize call. This will automatically set package-enable-at-startup to nil, to avoid loading the packages again after processing the init file. 
(package-initialize)
;;(pp load-path) ;; At this time, add subdirs of  package-user-dir to package-directory-list
;;(pp package-load-list) ;; (all)
;;(pp package-enable-at-startup) ;; nil



;; SETUP MY CUSTOM PACKAGES

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-functions)

;; Import new GNU ELPA keys (if any) into package.el’s keyring.
(require-pack 'gnu-elpa-keyring-update)


;; 设置utf-8为默认编码格式
(set-language-environment "UTF-8")

;; 取消滚动到底部的报警声
(setq ring-bell-function 'ignore)

;; Enable truncate lines
(setq-default truncate-lines t)

;; 禁止 Emacs 自动生成备份文件
(setq make-backup-files nil)

;; 禁止默认的自动保存
(setq auto-save-default nil)

;; 保存桌面布局
(desktop-save-mode -1)

;; 自动加载修改过的文件
(global-auto-revert-mode t)

;; 记录最近打开过的文件
;; (require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 300)

;; Comment this keybinding. Use dashboard instead.
;; (arki/define-key "o" 'recentf-open-files 'arki/prefix-keymap)

;; Bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" arki/cache-dir))
(setq bookmark-save-flag 1)

;; Less typing when emacs asks yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable function
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; 选中一段文字之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)

(put 'upcase-region 'disabled nil)



;; https://stackoverflow.com/questions/2736087/eval-after-load-vs-mode-hook
;; https://stackoverflow.com/questions/21880139/what-is-with-eval-after-load-in-emacs-lisp/21880276
;; (add-hook 'dired-mode-hook

(with-eval-after-load 'dired  
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
  )

;; Set mouse-color
(when (equal window-system 'x)
  (if (equal (frame-parameter nil 'background-mode) 'light)
      (set-frame-parameter nil 'mouse-color "black")
    (set-frame-parameter nil 'mouse-color "white")
    ))

;; (when (require-pack 'on-screen)
;;   (on-screen-global-mode +1))

;; Set cursor type to box or bar
(setq-default cursor-type 'bar)  	;Doesn't take effect by: (setq cursor-type 'bar)


;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode 'toggle)
;; 关闭菜单栏
(menu-bar-mode 'toggle)

;; 全屏显示
;; (setq initial-frame-alist (quote ((fullscreen . maximized))))
(toggle-frame-maximized)
;; (toggle-frame-fullscreen)

;; 关闭文件滑动控件
(scroll-bar-mode 'toggle)

;; 显示行号 仅当编程模式时候
(add-hook 'prog-mode-hook
	  (lambda nil
	    (if (version<= "26.0.50" emacs-version )
		(display-line-numbers-mode)
	      (linum-mode 1)))
	  )

;; 在最下面显示光标在行中的位置
(column-number-mode 1)


;; Input completion -- Completion of your input in the minibuffer. (See also Minibuffer Completion in the manual.)
(ido-mode t)
(setq ido-enable-flex-matching t)


;; 激活show-paren-mode
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; Find elisp source code.
(arki/define-key "C-h C-f" 'find-function)
(arki/define-key "C-h C-f" 'find-function)
(arki/define-key "C-h C-v" 'find-variable)
(arki/define-key "C-h C-k" 'find-function-on-key)



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
  (message "
 +---------------------------------------------------------------+
 | 使用方法 | 按根据提示对中英文字体进行加减，使表格线对齐       |
 | 英文字   | 0123456789 abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLM|
 | 扩展字   | 𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄉𠄀𠄁𠄂𠄃𠄄𠄅𠄆𠄇𠄈𠄄𠄅𠄆𠄇𠄇𠄆 |
 +---------------------------------------------------------------+

Set font: ENG(1 q+ a-) ZH(2 w+ s-) EXT(3 e+ d-) Symbol(4 r+ f-). Save(0)")
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


;; 使用新的自动保存
;; 参考自：
;; https://github.com/bbatsov/super-save/blob/2a905b8bdfc93bee16e2d62a61c6211bbe009331/super-save.el
;; https://www.emacswiki.org/emacs/auto-save.el
(defgroup arki/auto-save nil
  "Smart-saving of buffers."
  :group 'arki/config)

(defvar arki/auto-save-mode-map (make-sparse-keymap)
  "arki/auto-save mode's keymap.")

(defcustom arki/auto-save-triggers '(
				     switch-to-buffer next-buffer previous-buffer
				     other-window windmove-up windmove-down windmove-left windmove-right
				     revert-buffer revert-buffer-with-coding-system
				     ace-window) ; TODO Need to check (fboundp 'ace-window)
  "A list of commands which would trigger `arki/auto-save-command'."
  :group 'arki/auto-save
  :type '(repeat symbol))

(defcustom arki/auto-save-silent t
  "Save buffer without modify minibuffer message."
  :group 'arki/auto-save
  :type 'boolean)

(defcustom arki/auto-save-hook-triggers
  '(mouse-leave-buffer-hook focus-out-hook)
  "A list of hooks which would trigger `arki/auto-save-command'."
  :group 'arki/auto-save
  :type '(repeat symbol))

(defcustom arki/auto-save-auto-save-when-idle t
  "Save current buffer automatically when Emacs is idle."
  :group 'arki/auto-save
  :type 'boolean)

(defcustom arki/auto-save-idle-duration 5
  "The number of seconds Emacs has to be idle, before auto-saving the current buffer.
See `arki/auto-save-auto-save-when-idle'."
  :group 'arki/auto-save
  :type 'integer)

(defcustom arki/auto-save-remote-files nil
  "Save remote files when t, ignore them otherwise."
  :group 'arki/auto-save
  :type 'boolean)

(defcustom arki/auto-save-max-filesize 10
  "File size in MB.

If set, when the file is larger than VAL, don't do auto-save for performance issues.
If VAL is nil, ignore the file size impact."
  :group 'arki/auto-save
  :type 'number)

(defcustom arki/auto-save-exclude '(".emacs.d/elpa")
  "A list of regexps for buffer-file-name excluded from arki/auto-save.
When a buffer-file-name matches any of the regexps it is ignored."
  :group 'arki/auto-save
  :type '(repeat (choice regexp)))

(defun arki/auto-save--include-p (filename)
  "Return non-nil if FILENAME doesn't match any of the `arki/auto-save-exclude'."
  (let ((checks arki/auto-save-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (ignore-errors
                          (if (stringp (car checks))
                              (string-match (car checks) filename))))
            checks (cdr checks)))
    keepit))



(defun arki/auto-save--check-buffer-status ()
  "Check whether the buffer is ready for save"
  (and 
   ;; And Yassnippet is not active
   (or (not (boundp 'yas--active-snippets))
       (not yas--active-snippets))
   ;; And  Company is not active
   (or (not (boundp 'company-candidates))
       (not company-candidates))
   )
  )

;; file size check
(defvar arki/auto-save--cur-file-size nil "Size of current file in MB")
(make-variable-buffer-local 'arki/auto-save--cur-file-size)

(defun arki/auto-save--check-file-size ()
  "Check file size. According to `arki/auto-save-max-filesize'"
  (if arki/auto-save-max-filesize
      (if arki/auto-save--cur-file-size
	  (<= arki/auto-save--cur-file-size arki/auto-save-max-filesize)
	(let* ((file-size-bytes (file-attribute-size (file-attributes (buffer-file-name))))
	       ;; When the file is newly created, file--attributes is nil, so let file-size be 0 now.
	       (file-size-mega-bytes (if file-size-bytes (/ file-size-bytes 1024.0 1024.0) 0)))
	  (setq arki/auto-save--cur-file-size file-size-mega-bytes))
	(arki/auto-save--check-file-size)
	)
    t
    ))

(defun arki/auto-save-command ()
  "Save the current buffer if needed."
  ;; Use (buffer-file-name) instead of buffer-file-name-variable to
  ;; avoid error when the file is new created.
  ;; The ERROR: Wrong type argument: number-or-marker-p, nil
  (let ((file-name (buffer-file-name)))
    (when (and file-name
	       ;; And not file name not excluded
               (arki/auto-save--include-p file-name)
	       ;; And (if is remote: should allow remote save)
               (if (file-remote-p file-name) arki/auto-save-remote-files t)
	       ;; And is not readonly
	       (not buffer-read-only)
	       ;; And modified
               (buffer-modified-p (current-buffer))
	       ;; And writable
               (file-writable-p file-name)
	       ;; Check buffer status: And Yassnippet is not active And  Company is not active
	       (arki/auto-save--check-buffer-status)
	       ;; And file is not too large
	       (arki/auto-save--check-file-size)
	       )
      (if arki/auto-save-silent
	  (with-temp-message (format "Saving %S" file-name) (save-buffer))
	(save-buffer))
      )
    )
  )

(defvar arki/auto-save--idle-timer nil)

(defun arki/auto-save-command-advice (&rest _args)
  "A simple wrapper around `arki/auto-save-command' that's advice-friendly."
  (arki/auto-save-command))

(defun arki/auto-save--advice-trigger-commands ()
  "Apply arki/auto-save advice to the commands listed in `arki/auto-save-triggers'."
  (mapc (lambda (command)
          (advice-add command :before #'arki/auto-save-command-advice))
        arki/auto-save-triggers))

(defun arki/auto-save--advice-trigger-commands-cancel ()
  "Remove arki/auto-save advice from to the commands listed in `arki/auto-save-triggers'."
  (mapc (lambda (command) (advice-remove command #'arki/auto-save-command-advice))
        arki/auto-save-triggers))

(defun arki/auto-save--idle-timer-init ()
  "Initialize arki/auto-save idle timer if `arki/auto-save-auto-save-when-idle' is true."
  (setq arki/auto-save--idle-timer
        (when arki/auto-save-auto-save-when-idle
          (run-with-idle-timer arki/auto-save-idle-duration t #'arki/auto-save-command))))

(defun arki/auto-save--idle-timer-cancel ()
  "Stop arki/auto-save idle timer if `arki/auto-save--idle-timer' is set."
  (when arki/auto-save--idle-timer
    (cancel-timer arki/auto-save--idle-timer)))

(defun arki/auto-save-stop ()
  "Cleanup arki/auto-save's advices and hooks."
  (arki/auto-save--advice-trigger-commands-cancel)
  (arki/auto-save--idle-timer-cancel)
  (dolist (hook arki/auto-save-hook-triggers)
    (remove-hook hook #'arki/auto-save-command)))

(defun arki/auto-save-init ()
  "Setup arki/auto-save's advices and hooks."

  ;; Reset auto-save state
  (arki/auto-save-stop)
  ;; Add command advice
  (arki/auto-save--advice-trigger-commands)
  ;; Add timer
  (arki/auto-save--idle-timer-init)
  ;; Add hook
  (dolist (hook arki/auto-save-hook-triggers)
    (add-hook hook #'arki/auto-save-command)))

;; ;;;###autoload
;; (define-minor-mode arki/auto-save-mode
;;   "A minor mode that saves your Emacs buffers when they lose focus."
;;   :lighter " arki/auto-save"
;;   :keymap arki/auto-save-mode-map
;;   :group 'arki/auto-save
;;   :global nil
;;   (cond
;;    (arki/auto-save-mode (arki/auto-save-init))
;;    (t (arki/auto-save-stop))))


(add-hook 'emacs-startup-hook 'arki/auto-save-init)


;; Modify the default ibuffer-formats (toggle with `)
;; mark modified read-only locked name size mode process filename
(setq ibuffer-formats
      '((mark "(" modified vc-status-mini "|" read-only locked ") "
              (size-h 10 -1 :right)
              "   "
	      (name 15 15 :left :elide)
              "   "
              (mode 15 15 :left :elide)
              "   "
              vc-relative-file)
        (mark "(" modified vc-status-mini "|" read-only locked ") "
              (size-h 10 -1 :right)
              "   "
	      (name 15 -1 :left)
              "   "
              (mode 15 -1 :left)
              "   "
              (vc-status 15 15 :left)
              "   "
              vc-relative-file)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

;; 启用winner mode
(winner-mode 1)



;;----------------------------------------------------------------------------
;; Load variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(message "My init file ends here!")
