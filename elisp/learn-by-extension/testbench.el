(push "xxx" dynamic-library-alist)
(print dynamic-library-alist)

(special-form-p 'today)
(other-window 1)
(other-buffer)

(defun pre-buffer ()
  (interactive)
  (display-buffer-in-previous-window (other-buffer)))

(toggle-debug-on-error t)

((lambda (x) x . 3) 4)

(dolist (num '(1 2 3) 999) (print num))

(buffer-list)

(buffer-name)

(let ((mylist))
  (dolist (num '(1 2 3))
    (push num mylist))
  (print mylist))

(with-temp-message "" (progn
			(+ 1 3)
			(* 3 5)))

(with-temp-message "" (+ 1 2) (* 8 9))
(progn
  (+ 1 3)
  (* 3 5))

(unless (> 3 2)
  (print "hhh"))

(unless (> 1 2)
  (print "aaa"))
(unless (< 1 2)
  (print "bbb"))

(with-temp-message "" (basic-save-buffer))

(setq score 84)
(cond
 ((>= score 85) (print "A"))
 ((>= score 75) (print "B"))
 ((>= score 60) (print "C"))
 (t (print "D")))

(length "123")
(length '(1 2 3 4))
(length 6)

(message "hello, %s!" 'world)

(car '(1 2 3))

  (let ((lst '(1 2 3)) (sum 0))
    (while lst
      (setq num (car lst))
      (setq sum (+ sum num))
      (setq lst (cdr lst)))
    (print sum))

  (let ((lst '(1 2 3)))
    (while lst
      (setq num (car lst))
      (message "%d" num)
      (setq lst (cdr lst))))

(identity '(1 2 3))
(mapconcat 'identity '("1" "2" "3") ", ")

(defun hello (name greet)
  (message "hello %s! %s, current time is: %s" name greet (format-time-string "%H:%M:%S"))
  (sleep-for 3)
  (message ""))

(setq timer-obj (run-with-idle-timer 3 t 'hello "Tom" "how are you"))

(font-lock-flush)
