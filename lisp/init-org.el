(require 'org)

;; org-mode needs this
(require-pack 'htmlize)
(require-pack 'org-preview-html)

;; https://github.com/integral-dw/org-superstar-mode
(when (require-pack 'org-superstar)
  (add-hook 'org-mode-hook 'org-superstar-mode)
  (with-eval-after-load "org-superstar"
    (setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")))
  )

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-content 3)
	    
	    ;; Config view options
	    (setq line-spacing 0.1)
	    (setq org-src-fontily-natively t)
	    (org-indent-mode 1)
	    
	    ;; Config markup
	    (setq org-hide-emphasis-markers t) ;显示斜体、加粗之类的字体
	    (setq org-pretty-entities t)       ;显示上坡线指出的希腊字母、箭头图像之类的字符
	    (setq org-pretty-entities-include-sub-superscripts t) ;显示下角标、上角标
	    ;; When setting this variable to {}, ‘a_b’ is not interpreted as a subscript, but ‘a_{b}’ is
	    (setq org-use-sub-superscripts '{})			  ;限定_或者^后面跟着{}时候才使用上角标、下角标
	    ;; Add markup to text that spans more than two consecutive lines
	    ;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode/13828#13828
	    ;; Default value of org-emphasis-regexp-components: ("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "[:space:]" "." 1)
	    (setcar (nthcdr 4 org-emphasis-regexp-components) 3)
	    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

	    ;; Config export options
	    (setq org-export-with-sub-superscripts '{})		  ;设置导出时候，限定_或者^后面跟着{}时候才渲染上角标、下角标
	    (setq org-export-with-emphasis t)			  ;设置导出时候，渲染斜体、加粗之类的字体

	    ;; Config agenda related
	    (setq org-log-done 'time)
	    (setq org-log-done-with-time t)
	    
	    ))

;; Config org todos
(with-eval-after-load 'org
  ;; Set keywords. Need to invoke org-mode-restart after set.
  (setq org-todo-keywords
        '((sequence "TODO(1!)" "READY(2!)" "DOING(3!)" "STUCK(4@)" "PAUSE(4!)" "|" "DONE(5!)")
          (sequence "|" "CANCELED(0@/!)")))
  ;; Insert "CLOSED: [timestamp]" when entering DONE states.
  (setq org-log-done 'time)
  ;; Keep the line when remove todo keywords
  (setq org-closed-keep-when-no-todo t)
  ;; Set todo keywords faces
  (setq org-todo-keyword-faces
        '(("READY" . "green") ("STUCK" . "red") ("PAUSE" . (:underline t))
          ("CANCELLED" . (:foreground "gray" :weigth bold))))
  ;; Tracking todo state changes
  (setq org-log-into-drawer "LOGBOOK")
  ;; Set priorities to A-E
  (setq org-priority-highest 65 org-priority-default 67 org-priority-lowest 69)
  )


;; Config org agenda
(with-eval-after-load 'org
  (setq org-agenda-time-grid (quote ((daily today require-timed)
                                     (800
                                      1000
                                      1200
                                      1330
                                      1530
                                      1730
				      1930
				      2100)
                                     " ..."
                                     " ...................................."
                                     )))
  )


;; Config org-capture


(defgroup arki/org nil
  "Org mode config variables."
  :group 'arki/config)


(defcustom arki/org-capture-file-default "~/org/capture.org"
  "The default file location for `org-capture'"
  :group 'arki/org)

(defcustom arki/org-capture-file-thoughts "~/org/thoughts.org"
  "File location for `org-capture', usage: record thoughts."
  :group 'arki/org)


(defun init-org-capture ()
  "Init org capture config."

  (require 'org-capture)
  
  (setq org-default-notes-file arki/org-capture-file-default)
  
  (arki/define-key "a" 'org-agenda 'arki/prefix-keymap)
  (arki/define-key "c" 'org-capture 'arki/prefix-keymap)
  (add-to-list 'org-capture-templates
	       '("i" "Thoughts" entry (file+olp+datetree arki/org-capture-file-thoughts)
		 "* %U - %^{heading}\n  %?"))
  
  )


(add-hook 'after-init-hook (lambda () (interactive) (init-org-capture)))


;; Config org image insertion
(with-eval-after-load 'org

  ;; Config image view
  ;; (setq org-image-actual-width (/ (display-pixel-width) 6))
  (setq org-image-actual-width nil)
  ;; TODO How to display image link like: [[./img.png][image-describe]]
  (setq org-startup-with-inline-images t)
  
  (setq arki/org-insert-image--previous-dir nil)
  (defun arki/org-insert-image ()
    (interactive)
    (let* ((file-path (buffer-file-name)))
      (if file-path
          (let* ((cur-dir (file-name-directory file-path))
		 (dir-name (concat (file-name-nondirectory file-path) ".file"))
		 (dir-path (expand-file-name dir-name cur-dir))
		 ;; (origin-file (ido-read-file-name "Image location: " arki/org-insert-image--previous-dir))
		 (origin-file (let* ((input-path (ido-read-file-name "Image location: " arki/org-insert-image--previous-dir))
				     (win2linux-path (arki/wsl-path-win2linux input-path)))
				(when win2linux-path
				  (setq input-path win2linux-path))
				(if (file-directory-p input-path)
				    (ido-read-file-name (concat "Select image in " win2linux-path " :") input-path)
				  input-path)))
		 (img-name (file-name-nondirectory origin-file))
		 (new-name (concat (read-from-minibuffer "Rename image: " (file-name-base img-name)) (file-name-extension img-name t)))
		 (img-width (ido-completing-read "Image width in pixel: " (list "500" "origin" "300" "600") nil nil nil))
		 (target-file (expand-file-name new-name dir-path))
		 ;; (target-file-relative (concat (file-name-as-directory ".") (file-name-as-directory dir-name) new-name))
		 (target-file-relative (concat (file-name-as-directory dir-name) new-name))
		 )
	    ;; TODO Validate whether it's an image, using `image-file-name-regexp'.
	    ;; TODO Add support for download image from url.
	    (if (and (file-exists-p origin-file)
		     (not (file-directory-p origin-file)))
		(progn
		  (make-directory dir-path t)
		  (copy-file origin-file target-file 1)
		  (setq arki/org-insert-image--previous-dir (file-name-directory origin-file))
		  (if (equal img-width "origin")
		      (insert (format "[[file:%s]]" target-file-relative))
		    (insert (format "#+ATTR_ORG: :width %s\n[[file:%s]]" img-width target-file-relative)))
		  (org-display-inline-images t))
	      (message (concat "Failed! The input file not exists or is a directory: " origin-file))
	      )
	    )
	(message "This buffer hasn't been saved. Can't decide target image path."))
      )
    )

  (setq arki/org-insert-file--previous-dir nil)
  (defun arki/org-insert-file ()
    (interactive)
    (let* ((file-path (buffer-file-name)))
      (if file-path
          (let* ((cur-dir (file-name-directory file-path))
		 (dir-name (concat (file-name-nondirectory file-path) ".file"))
		 (dir-path (expand-file-name dir-name cur-dir))
		 (origin-file (let* ((input-path (ido-read-file-name "File location: " arki/org-insert-file--previous-dir))
				     (win2linux-path (arki/wsl-path-win2linux input-path)))
				(when win2linux-path
				  (setq input-path win2linux-path))
				(if (file-directory-p input-path)
				    (ido-read-file-name (concat "Select file in " win2linux-path " :") input-path)
				  input-path)))
		 (file-name (file-name-nondirectory origin-file))
		 (new-name (concat (read-from-minibuffer "Rename file: " (file-name-base file-name)) (file-name-extension file-name t)))
		 (target-file (expand-file-name new-name dir-path))
		 (target-file-relative (concat (file-name-as-directory dir-name) new-name)))
	    ;; TODO Add support for download file from url.
	    (if (and (file-exists-p origin-file)
		     (not (file-directory-p origin-file)))
		(progn
		  (make-directory dir-path t)
		  (copy-file origin-file target-file 1)
		  (setq arki/org-insert-file--previous-dir (file-name-directory origin-file))
		  (insert (format "[[file:%s][%s]]" target-file-relative new-name)))
	      (message (concat "Failed! The input file not exists or is a directory: " origin-file))))
	(message "This buffer hasn't been saved. Can't decide target image path."))))

  (defun arki/org-delete-file-link()
    "Delete the link at cursor, along with the associated file."
    (interactive)
    (let ((cur-file (buffer-file-name))
	  (cur-element (org-element-context)))
      (if (and (equal (car cur-element) 'link)
	       (file-exists-p cur-file))
	  (progn
	    (let* ((f-path (org-element-property :path cur-element))
		   (f-path-abs (expand-file-name f-path (file-name-directory cur-file)))
		   (begin (org-element-property :begin cur-element))
		   (end (org-element-property :end cur-element)))
	      (if (file-regular-p f-path-abs)
		  (when (yes-or-no-p (format "Delete file [%s] ?" f-path-abs))
		    (message "Deleting file: %s" f-path-abs)
		    (delete-file f-path-abs)
		    (message "File deleted: %s" f-path-abs)
		    (delete-region begin end))
		(message "Failed to delete file link: It's not a regular file: %s" f-path-abs))))
	(message "Failed to delete file link: Not a link here or this buffer file not exists."))))


  (defun arki/org-insert-file-link()
    (interactive)

    (message "Insert link: Image(i) File(f)     |     Delete file and link: (k)")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector (list ?i))
	 (lambda () (interactive) (arki/org-insert-image)))
       (define-key map (vector (list ?f))
	 (lambda () (interactive) (arki/org-insert-file)))
       (define-key map (vector (list ?k))
	 (lambda () (interactive) (arki/org-delete-file-link)))
       map)))

  (arki/define-key "C-c i" 'arki/org-insert-file-link 'org-mode-map)
  
  )



(when (display-graphic-p)
  ;; https://github.com/abo-abo/org-download
  (require-pack 'org-download)
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable))

;; (setq org-agenda-files '("~/org"))

;; Config for latex formular
(when (require-pack 'auctex t)
  (when (require-pack 'cdlatex)
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
  )


(require-pack 'org-ref)


;; Config org-roam
(when (require-pack 'org-roam)
  (org-roam-setup)
  (arki/define-key "q" 'org-roam-buffer-display-dedicated 'arki/prefix-keymap)
  (arki/define-key "C-c q" 'org-roam-node-insert 'org-mode-map)
  )

;; Config org-web-tools
(when (require-pack 'org-web-tools))

(provide 'init-org)
