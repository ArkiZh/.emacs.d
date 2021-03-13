;;----------------------------------------------------------------------------
;; Install package
;;----------------------------------------------------------------------------
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(defgroup arki/config nil
  "Manager for my own confgs.")


(defvar arki/package-contents-refreshed nil
  "Indicate whether the package contents has been refreshed.

If nil: invoke `package-refresh-contents' during `require-package', then set to t, no matter whether the refresh is successful.
If non-nil: don't invoke `package-refresh-contents' during `require-package.")

(defvar arki/package-installed-info '(("INSTALLED_BEFORE") ("INSTALLED_NOW") ("FAILED"))
  "Record the installation info of required packages.")


(defun arki/alist-push-value (input_alist key value)
  "Set the given ALIST.
Operation is done in-place.

If the key exists: push value to the fist position of that key's content.
If not exists: append the k-v pair to the end of this alist.

Return: the input_alist."
  (let ((cur_kv (assoc key input_alist)))
    (if cur_kv (setcdr cur_kv (cons value (cdr cur_kv)))
      (message "Unknown key: %s. Now add it with value: %s." key value)
      ;; (setcdr input_alist (copy-alist input_alist))
      ;; (setcar input_alist (list key value))
      (setcdr (last input_alist) (list (list key value)))
      )
    )
  input_alist)


(defun refresh-pack-contents ()
  "Refresh package database. And refresh only once."
  (unless arki/package-contents-refreshed
    (message "Refreshing package database...")
    (condition-case err (
			 progn
			 (message "Refreshing package database...")
			 (package-refresh-contents)
			 (message "Refreshing package database finished!")
			 )
      (error
       (message "Failed to refresh package database! ERROR: %S" err))
      )
    (setq arki/package-contents-refreshed t)
    ))


(defun require-pack (pack)
  "Install the given PACKAGE.
And execute require PACKAGE if newly installed, to avoid requiring it manually.

If installed successfully, return t, else return nil"
  (if (package-installed-p pack) (progn (arki/alist-push-value arki/package-installed-info "INSTALLED_BEFORE" pack) t)
    (refresh-pack-contents)
    (message "Package: %s installing..." pack)
    (condition-case err (progn
			  (package-install pack)
			  (arki/alist-push-value arki/package-installed-info "INSTALLED_NOW" pack)
			  (message "Package: %s installed!" pack)
			  (require pack)
			  (message "Package: %s loaded!" pack)
			  t)
      (error
       (arki/alist-push-value arki/package-installed-info "FAILED" pack)
       (message "Package `%S' failed to install! ERROR: %S" pack err)
       nil)
      )
    ))

;; https://emacs-china.org/t/require-package-maybe-require-package/8496
;; (defun require-package (package &optional min-version)
;;   "Install PACKAGE if it's not installed.
;; Optionally require MIN-VERSION."
;;   (or (package-installed-p package min-version)
;;       (if (assoc package package-archive-contents)
;;           (package-install package)
;;         (progn
;;           (package-refresh-contents)
;;           (package-install package)))))

;; (defun maybe-require-package (package &optional min-version)
;;   "Try to install PACKAGE, and return non-nil if successful or PACKAGE exists.
;; In the event of failure, return nil and print a warning message.
;; Optionally require MIN-VERSION."
;;   (condition-case err
;;       (require-package package min-version)
;;     (error
;;      (message "Failed to install optional package `%s': %S" package err)
;;      nil)))

;; (cl-defun require-package (package &key min-version ignore-error)
;;   ...)
;; (require-package foo-package) ;; 必须安装的包，却少则无法正常使用
;; (require-package bar-package :ignore-error t) ;; 可有可无的包，不安装也不影响使用

;; (unless (arki/packages-installed-p arki/packages)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg arki/packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))


;;----------------------------------------------------------------------------
;; Set key binding
;;----------------------------------------------------------------------------
(defvar arki/key-bindings '((global))
  "Record customize key-bindings.")

(defun arki/define-key (key command &optional keymap)
  "Set key binding, and record the binding.
If keymap is not provided, will use global-set-key.
If keymap is provided, you should add quote before it."
  (interactive)
  (if keymap
      (progn
	(define-key (eval keymap) (kbd key) command)
	(arki/alist-push-value arki/key-bindings keymap (list key command))
	)
    (global-set-key (kbd key) command)
    (arki/alist-push-value arki/key-bindings 'global (list key command))
    )
  )

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun arki/delete-this-file-and-buffer ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(arki/define-key "C-c u k" 'arki/delete-this-file-and-buffer)

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun arki/rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(arki/define-key "C-c u r" 'arki/rename-this-file-and-buffer)

;;----------------------------------------------------------------------------
;; 快速打开配置文件
;;----------------------------------------------------------------------------
(defun arki/open-init-file()
  "Open init file.

The file is named init.el under `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; (arki/define-key "<f2>" 'arki/open-init-file)

;;----------------------------------------------------------------------------
;; 调整buffer中的缩进
;;----------------------------------------------------------------------------
(defun arki/indent-region-or-buffer()
  "Indent the region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Region indented."))
      (progn
	(indent-region (point-min) (point-max))
	(message "Buffer indented.")))))

(arki/define-key "C-M-\\" 'arki/indent-region-or-buffer)

;;----------------------------------------------------------------------------
;; 选中当前行
;;----------------------------------------------------------------------------
(defun arki/select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(arki/define-key "C-c u l" 'arki/select-current-line)

;;----------------------------------------------------------------------------
;; 将多行的段落合并成一行
;;----------------------------------------------------------------------------
;; https://stackoverflow.com/questions/6707758/inverse-of-m-q-an-unfill-paragraph-function
(defun arki/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; 将多行的段落合并成一行
(arki/define-key "M-Q" 'arki/unfill-paragraph)

(provide 'init-functions)
