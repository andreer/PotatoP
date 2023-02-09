;; The humble beginnings of a text editor

(defvar black 0)
(defvar white 1)

(defun insert-char (buffer char pos)
  (concatenate 'string
	       (subseq 0 cursor-pos)
	       (princ-to-string char)
	       (subseq cursor-pos (length buffer))))

(defun display (buffer cursor-pos)
  (set-cursor 0 0)
  (set-text-color black white)
  (fill-screen white)
  (with-gfx (out)
    (princ (subseq 0 cursor-pos))
    (princ #\176 out)
    (princ (subseq cursor-pos (length buffer)))
    (refresh)))

(defvar left #x11)
(defvar right #x10)
(defvar up #x1e)
(defvar down #x1f)

(defun edit (buffer)
  (let ((cursor-pos 0)
	(dirty t))
    (loop (if (not (get-key)) (return))) ; empty key buffer
    (fill-screen white)
    (loop
     (let ((q (get-key)))
       (cond
	 ((not q) (if dirty (progn (display buffer cursor-pos) (setq dirty nil))))
	 ((= q (char-code #\Escape)) (return buffer))
	 ((= q (char-code #\Backspace))
	  (setq buffer (subseq buffer 0 (1- (length buffer)))
		(decf cursor-pos)
		dirty t))
	 ((= q left) (progn (decf cursor-pos) (setq dirty t)))
	 ((= q right) (progn (incf cursor-pos) (setq dirty t)))
	 ((< q #xff)
	  (setq buffer (insert-char buffer (code-char q))
		(incf cursor-pos)
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
