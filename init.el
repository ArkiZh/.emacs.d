(defun arki/org-babel-load-file (file &optional compile)
  "Similar to `org-babel-load-file', but changes some logic.  Load Emacs
Lisp source code blocks in the Org FILE.  This function exports the
source code using `org-babel-tangle' and then loads the resulting file
using `load-file'.  With optional prefix argument COMPILE, the tangled
Emacs Lisp file is byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let* ((tangled-file (concat (file-name-sans-extension file) ".el"))
         (tangled-compiled-file (concat (file-name-sans-extension file) ".elc")))
    (require 'org)
    ;; Tangle only if the Org file is newer than the Elisp file.
    (when (file-newer-than-file-p file tangled-file)
      (org-babel-tangle-file file tangled-file "emacs-lisp")
      (message "Tangled: %s --> %s" file tangled-file))
    (if compile
        (if (file-newer-than-file-p tangled-file tangled-compiled-file)
            (progn
              (byte-compile-file tangled-file 'load)
              (message "Compiled and loaded: %s --> %s" tangled-file tangled-compiled-file))
          (load-file tangled-compiled-file)
          (message "Has been compiled before. Loaded now: %s" tangled-compiled-file))
      (load-file tangled-file)
      (message "Loaded: %s" tangled-file))))

;; Set arki/package-install-disabled to t, if don't want to install new packages.
(defvar arki/package-install-disabled nil "Disable package install if t.")

(defvar arki/package-require-disabled nil "Disable require-pack if t.")

(arki/org-babel-load-file (expand-file-name "readme.org" user-emacs-directory) t)
