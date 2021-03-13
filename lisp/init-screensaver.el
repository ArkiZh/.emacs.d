(require 'zone)
(require 'color)
(require 'cl-lib)

(defgroup zone-timer nil
  "Zone out with timer."
  :group 'games)
;; Rainbow effect learnt from: https://github.com/kawabata/zone-rainbow
(defcustom zone-timer-rainbow-hue-factor 50 "Hue factor." :group 'zone-timer)
(defcustom zone-timer-rainbow-sat 1.0 "Saturation." :group 'zone-timer)
(defcustom zone-timer-rainbow-light 0.5 "Light." :group 'zone-timer)
(defcustom zone-timer-rainbow-background nil "If not nil, Background color." :group 'zone-timer)
(defcustom zone-timer-interval 10 "Interval (in seconds) to show the new time." :group 'zone-timer)
(defcustom zone-timer-rainbow-enable t "Whether to enable rainbow effect. t: enable. nil: disable." :group 'zone-timer)
(defcustom zone-timer-format-string "%Y-%m-%d %k:%M:%S" "Used to format time. See: `format-time-string'" :group 'zone-timer)
(defcustom zone-timer-start-column -1 "The start column number from where to print time. -1 means the middle of window." :group 'zone-timer)
(defun zone-pgm-timer ()
  "Show current time."
  (let* ((start-column (if (= zone-timer-start-column -1)
			   (- (/ (window-width) 2) (/ (length (format-time-string zone-timer-format-string)) 2))
			 zone-timer-start-column
			 )
		       )
	 (start-time (current-time))
	 (pre-time start-time)
	 (pre-second (nth 1 pre-time))
	 (k 0))

    (when (not (input-pending-p))
      (delete-region (point-min) (point-max))
      (insert (concat (make-string start-column (string-to-char " ")) (format-time-string zone-timer-format-string start-time)))
      (newline) (sit-for 0.1))
    ;; 渲染流动彩色
    (while (not (input-pending-p))
      ;; 更新时间
      (let* ((cur-time (current-time))
	     (cur-second (nth 1 cur-time)))
	(when (> cur-second pre-second)	; 需要更新秒
	  (delete-region (line-beginning-position) (line-end-position))
	  (insert (concat (make-string start-column (string-to-char " ")) (format-time-string zone-timer-format-string cur-time)))
	  (setq pre-second cur-second)
	  (sit-for 0.01)
	  (when (>= (- cur-second (nth 1 pre-time)) zone-timer-interval) ; 需要更新时间间隔
	    (if (< (line-number-at-pos) (- (window-height) 3)) ;判断是不是最后一行，是最后一行的话需要跳到开始
		(newline)
	      (delete-region (point-min) (point-max)))
	    (setq pre-time cur-time))
	  )

	;; 渲染单帧彩色
	(when (and zone-timer-rainbow-enable (window-system))
	  (cl-loop
	   for i from (window-start) to (1- (window-end)) do
	   (add-text-properties
	    i (1+ i)
	    `(face ((foreground-color
		     . ,(apply 'color-rgb-to-hex
                               (color-hsl-to-rgb
				(/ (* (% (+ i k) zone-timer-rainbow-hue-factor) 1.0)
				   zone-timer-rainbow-hue-factor)
				zone-timer-rainbow-sat zone-timer-rainbow-light)))
		    ,@(when zone-timer-rainbow-background
			`((background-color
			   . ,zone-timer-rainbow-background)))))))
	  (cl-incf k))
	(sit-for 0.1)))))


;;;###autoload
(defun zone-timer ()
  "Zone out with rainbow."
  (interactive)
  (let ((zone-programs [zone-pgm-timer])
	(zone-timer-interval 5))
    (zone)))


;; 启用zone
(zone-when-idle 60)
(setq zone-programs [zone-pgm-timer])
(setq zone-timer-interval 600)
(setq zone-timer-rainbow-enable t)

(provide 'init-screensaver)
