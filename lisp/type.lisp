;; The humble beginnings of a text editor

(defvar black 0)
(defvar white 1)

(defun insert-char (buffer char)
  (concatenate 'string buffer (princ-to-string char)))

(defun display (buffer)
  (set-cursor 10 10)
  (set-text-color black white)
  (fill-screen white)
  (with-gfx (out)
    (princ buffer out)
    (princ #\176 out))
  (refresh))

(defun edit (buffer)
  (let ((cursor-pos 0))
    (loop (if (not (get-key)) (return))) ; empty key buffer
    (fill-screen white)
    (display buffer)
    (loop
     (let ((q (get-key)))
       (cond
	 ((not q) nothing)
	 ((= q 27) (return buffer))
	 ((= q 8)
	  (setq buffer (subseq buffer 0 (1- (length buffer)))))
         ((< q #xff)
	  (setq buffer (insert-char buffer (code-char q)))))
       (display buffer)))))

(defun type () (edit ""))

(defun gfx-repl ()
  (loop
   (with-gfx (gfx)
     (let ((typed (type)))
       (if (eq 0 (length typed)) (setq typed "nil"))
       (let ((evalled (eval (read-from-string typed))))
	 (princ #\Newline gfx)
	 (prin1 evalled gfx)
	 (refresh)
	 (loop (if (eq (get-key) 27) (return))))))))
