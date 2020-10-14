;;----------------------------------------------------------------------------
;; Install package
;;----------------------------------------------------------------------------
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(defgroup arki/config nil
  "Manager for my own confgs.")

;; (defcustom arki/packages nil
;;   "A list of packages to install."
;;   :type 'list
;;   :group 'arki/config)


;; (defun arki/packages-installed-p (pkgs)
;;   "Check whether the packages have been installed."
;;   (cl-loop for pkg in pkgs
;; 	   when (not (package-installed-p pkg)) do (return nil)
;; 	   finally (return t)))


(defvar arki/package-contents-refreshed nil
  "Indicate whether the package contents has been refreshed.

If nil: invoke `package-refresh-contents' during `require-package', then set to t, no matter whether the refresh is successful.
If non-nil: don't invoke `package-refresh-contents' during `require-package.")

(defvar arki/package-installed-info )

(defun require-package (package)
  "Install given PACKAGE"

  (unless (package-installed-p package)
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
      )
    (message "Installing package: %s ..." package)
    (condition-case err (progn
			  (package-install package)			  
			  (message "Installing package: %s ...Finished!" package))
      (error
       (message "Failed to install package `%S'! ERROR: %S" package err))
      ))

  )

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
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
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


;;----------------------------------------------------------------------------
;; 快速打开配置文件
;;----------------------------------------------------------------------------
(defun open-init-file()
  "Open init file.

The file is named init.el under `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))


;;----------------------------------------------------------------------------
;; 调整buffer中的缩进
;;----------------------------------------------------------------------------
(defun indent-buffer()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  "Indent the region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Region indented."))
      (progn
	(indent-buffer)
	(message "Buffer indented.")))))

;;----------------------------------------------------------------------------
;; 选中当前行
;;----------------------------------------------------------------------------
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

;;----------------------------------------------------------------------------
;; 将多行的段落合并成一行 TODO: 目前不支持选区的合并
;;----------------------------------------------------------------------------
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(provide 'init-custom-functions)
