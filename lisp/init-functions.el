;;----------------------------------------------------------------------------
;; Install package
;;----------------------------------------------------------------------------
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")))


(defgroup arki/config nil
  "Manager for my own confgs.")


(defcustom arki/cache-dir (expand-file-name "arki.cache" user-emacs-directory) "Cache for useful files" :group 'arki/config)
(make-directory arki/cache-dir t)


(defcustom arki/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
  "Directory for site lisp. If changed, should add the new value to `load-path' manually."
  :group 'arki/config)
(make-directory arki/site-lisp-dir t)
(add-to-list 'load-path arki/site-lisp-dir)


(defvar arki/package-contents-refreshed nil
  "Indicate whether the package contents has been refreshed.

If nil: invoke `package-refresh-contents' during `require-package', then set to t, no matter whether the refresh is successful.
If non-nil: don't invoke `package-refresh-contents' during `require-package.")

(defvar arki/package-installed-info '(("INSTALLED_BEFORE") ("INSTALLED_NOW") ("FAILED"))
  "Record the installation info of required packages.")


(defun arki/alist-push-value (input_alist key value)
  "Set the given ALIST.
Operation is done in-place.

If the key exists: push value to the first position of that key's content.
If not exists: append the k-v pair to the end of this alist.

Return: the input_alist."
  (let ((cur_kv (assoc key input_alist)))
    (if cur_kv (setcdr cur_kv (cons value (cdr cur_kv)))
      ;; (message "Unknown key: %s. Now add it with value: %s." key value)
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
  (if (package-installed-p pack) (progn
				   (arki/alist-push-value arki/package-installed-info "INSTALLED_BEFORE" pack)
				   (require pack)
				   t)
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

(defun require-packs (pack &rest packs)
  "Insall the given packs.

If all installed successfully, return t, else return nil"
  (if (not packs)
      (require-pack pack)
    (push pack packs)
    (let ((all-installed t))
      (dolist (cur-pack packs)
	(setq all-installed (and all-installed (require-pack cur-pack))))
      all-installed))
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
;; Set key binding
;;----------------------------------------------------------------------------
(defvar arki/key-bindings '((global))
  "Record customize key-bindings.")

(define-prefix-command 'arki/prefix-keymap)
(global-set-key (kbd "M-SPC") 'arki/prefix-keymap)

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

(arki/define-key "k" 'arki/delete-this-file-and-buffer 'arki/prefix-keymap)

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

(arki/define-key "r" 'arki/rename-this-file-and-buffer 'arki/prefix-keymap)

;;----------------------------------------------------------------------------
;; 快速打开配置文件
;;----------------------------------------------------------------------------
(defun arki/open-init-file()
  "Open init file.

The file is named init.el under `user-emacs-directory'."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun arki/open-alias()
  "Open alias files.

If the alias is defined, open it.
Else, define it now, then open it."
  (interactive)
  ;; TODO
  )


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
  "Select current line"
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line)
  )

(defun arki/copy-current-line ()
  "Copy current line"
  (interactive)
  (save-mark-and-excursion ;; else
    (arki/select-current-line)
    (kill-ring-save (region-beginning) (region-end))
    (pop-mark)
    )
  )
(defun arki/kill-current-line ()
  "Copy current line"
  (interactive)
  (save-mark-and-excursion ;; else
    (arki/select-current-line)
    (kill-region (region-beginning) (region-end))
    (pop-mark)
    )
  )

(arki/define-key "a" 'arki/copy-current-line 'arki/prefix-keymap)
(arki/define-key "w" 'arki/kill-current-line 'arki/prefix-keymap)

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


;;----------------------------------------------------------------------------
;; Site lisp utils
;;----------------------------------------------------------------------------
;; Learnt from purcel's config.
;; https://github.com/purcell/emacs.d

;; It seems only need to add site-lisp root dir to load-path, so comment this function out.
;; (defun arki/add-subdirs-to-load-path (parent-dir)
;;   "Add every non-hidden subdir of PARENT-DIR to `load-path'."
;;   (let ((default-directory parent-dir))
;;     (dolist (cur-dir (directory-files (expand-file-name user-emacs-directory) t "^[^\\.]"))
;;       (when (file-directory-p cur-dir)
;; 	(setq load-path (append (list cur-dir) load-path))))))


;; Utilities for grabbing upstream libs
(defun arki/site-lisp-dir-for (name)
  (expand-file-name (symbol-name name) arki/site-lisp-dir))


(defun arki/site-lisp-library-el-path (name)
  "Locate el file path in site lisp directory.

First search file in `arki/site-lisp-dir', if not found here, give the path in subdir."
  (let* ((el-name (format "%s.el" name))
	 (first-file (expand-file-name el-name arki/site-lisp-dir)))
    (if (file-exists-p first-file)
	first-file
      (expand-file-name el-name (arki/site-lisp-dir-for name)))))


(defun arki/download-site-lisp-el-file (name url)
  (let ((dir (arki/site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (arki/site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))


(defun site-lisp-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/site-lisp/name/"
  (file-exists-p (arki/site-lisp-library-el-path name)))


(defun require-pack-local (name &optional ensure url)
  "Install package from url.

If it's already downloaded before, compile it if necessary, then require it.
If not found, download it if `ensure' is t, otherwise ignore this package."
  (if (site-lisp-library-loadable-p name)
      (let ((el-file (arki/site-lisp-library-el-path name)))
	(if (file-exists-p (byte-compile-dest-file el-file))
	    (load (byte-compile-dest-file el-file))
	  (message "Compile file: " el-file)
	  (byte-compile-file el-file t)))
    (if ensure
	(if url
	    (progn (message "Local package not found: %s, download now." name)
		   (byte-compile-file (arki/download-site-lisp-el-file name url)))
	  (warn "Local package not found: %s, and no download url is provided!" name)
	  t)
      (message "Local package is not found: %s, ignore it." name)
      nil)))


(provide 'init-functions)
