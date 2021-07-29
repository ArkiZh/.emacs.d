(defgroup arki/wsl nil
  "Input method config, only for read."
  :group 'arki/config)


(defcustom arki/wsl-p nil "Flag to indicate whether I'm in wsl." :group 'arki/wsl)



(defun arki/wsl-path-win2linux (win-path)
  "Transform windows path to linux path inside wsl."

  (when (and arki/wsl-p
	     (string-match "^[a-zA-Z]:[\\\\/]" win-path))
    (let* ((disk (downcase (substring win-path 0 1)))
	   (final-path (replace-regexp-in-string "^[a-zA-Z]:[\\\\/]*" "" win-path))
	   )
      (setq final-path (replace-regexp-in-string "\\\\+" "/" final-path))
      (setq final-path (concat "/mnt/" disk "/" final-path)))))


(defun arki/wsl-copy-file-win2linux (win-path target-linux-path)
  "Copy file from windows to specified linux path inside wsl."
  (let* ((wsl-path (arki/wsl-path-win2linux win-path)))
    (when wsl-path
      (copy-file wsl-path target-linux-path 1)
      t)))


(provide 'init-wsl)
