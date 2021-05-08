;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:
;; Learn from: https://github.com/purcell/emacs.d/blob/master/lisp/init-ibuffer.el

;;; Code:
(when (require-pack 'fullframe)
  (with-eval-after-load 'ibuffer
    (fullframe ibuffer ibuffer-quit)))

(when (require-pack 'ibuffer-vc)
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
  )

(setq-default ibuffer-show-empty-filter-groups nil)

(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

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


(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
