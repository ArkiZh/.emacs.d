(defun arki/eaf-enable ()
  (interactive)
  
  ;; https://github.com/manateelazycat/emacs-application-framework
  ;; Step1: Download eaf repo.
  ;; git clone --depth=1 -b master https://github.com/manateelazycat/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/
  
  ;; Step2: Install system dependencies. Load eaf first then invoke this command.
  ;; M-x eaf-install-dependencies

  ;; Step3: Install el dependencies.
  ;; https://github.com/kiwanami/emacs-ctable
  (require-pack 'ctable)
  ;; https://github.com/kiwanami/emacs-deferred
  (require-pack 'deferred)
  ;; https://github.com/kiwanami/emacs-epc
  (require-pack 'epc)
  ;; https://github.com/magnars/s.el
  (require-pack 's)

  ;; Step4: Enable eaf
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)

  ;; Then all active EAF Browser buffers will be noted down before
  ;; Emacs is killed, and will be opened again when calling M-x
  ;; eaf-browser-restore-buffers in a future Emacs session.
  (setq eaf-browser-continue-where-left-off t)
  
  (setq eaf-browser-default-search-engine "duckduckgo")
  (eaf-setq eaf-browse-blank-page-url "https://duckduckgo.com")

  ;; There are certain variables will be shared across Emacs Lisp and Python.
  ;; You can easily configure then with the function eaf-setq.
  ;; Check the full list of configurable variables with C-h v eaf-var-list
  )
(arki/define-key "e" 'arki/eaf-enable 'arki/prefix-keymap)

(provide 'init-eaf)
