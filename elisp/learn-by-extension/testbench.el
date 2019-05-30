(special-form-p 'today)
(other-window 1)
(other-buffer)

(defun pre-buffer ()
  (interactive)
  (display-buffer-in-previous-window (other-buffer)))

(toggle-debug-on-error t)
