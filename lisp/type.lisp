;; The humble beginnings of a text editor

(defvar black 0)
(defvar white 1)

(defun insert-char (buffer char)
  (concatenate 'string buffer (princ-to-string char)))

(defun display (buffer)
  (set-cursor 0 0)
  (set-text-color black white)
  (fill-screen white)
  (with-gfx (out)
    (princ buffer out)
    (princ #\176 out))
  (refresh))

(defun edit (buffer)
  (let ((dirty t))
    (loop (if (not (get-key)) (return))) ; empty key buffer
    (fill-screen white)
    (loop
     (let ((q (get-key)))
       (cond
	 ((not q) (if dirty (progn (display buffer) (setq dirty nil))))
	 ((= q (char-code #\Escape)) (return buffer))
	 ((= q (char-code #\Backspace))
	  (setq buffer (subseq buffer 0 (1- (length buffer)))
		dirty t))
	 ((< q #xff)
	  (setq buffer (insert-char buffer (code-char q))
		dirty t)))))))

(defun type () (edit ""))

(defun gfx-repl ()
  (loop
   (with-gfx (gfx)
     (let ((typed (type)))
       (if (eq 0 (length typed)) (setq typed "nil"))
       (let ((evalled (eval (read-from-string typed))))
	 (princ #\Newline gfx) (princ #\Newline gfx)
	 (prin1 evalled gfx)
	 (refresh)
	 (loop (if (eq (get-key) 27) (return))))))))

(defun repeat-string (str n)
  (let ((res ""))
    (dotimes (i n res)
      (setq res (concatenate 'string res str)))))
