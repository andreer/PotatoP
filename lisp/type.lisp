;; The humble beginnings of a text editor
;;
;; TODO:
;;   - move to start/end of line
;;   - move by word
;;   - highlight matching paren/quote

(defvar black 0)
(defvar white 1)

(defun insert-char (buffer char pos)
  (concatenate 'string
	       (subseq buffer 0 cursor-pos)
	       (princ-to-string char)
	       (subseq buffer cursor-pos)))


(defun delete-char (buffer pos)
  (concatenate 'string
	       (subseq buffer 0 pos)
	       (subseq buffer (1+ pos))))

(defun display (buffer cursor-pos)
  (set-cursor 0 0)
  (set-text-color black white)
  (fill-screen white)
  (with-gfx (out)
    (princ (subseq buffer 0 cursor-pos) out)
    (princ #\176 out)
    (princ (subseq buffer cursor-pos) out)
    (refresh)))

(defvar left #x11)
(defvar right #x10)
(defvar up #x1e)
(defvar down #x1f)

(defun prev-pos (buffer pos ch)
  (loop
   (decf pos)
   (if (< pos 0) (return nil))
   (if (eq ch (char buffer pos)) (return pos))))

(defun next-pos (buffer pos ch)
  (let ((l (length buffer)))
    (loop
     (incf pos)
     (if (>= pos l) (return nil))
     (if (eq ch (char buffer pos)) (return pos)))))

(defun typo (buffer)
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
	  (if (> cursor-pos 0)
	      (setq buffer (delete-char buffer (1- cursor-pos))
		    cursor-pos (1- cursor-pos)
		    dirty t)))
	 ((= q left) (setq cursor-pos (max 0 (1- cursor-pos))
			   dirty t))
	 ((= q right) (setq cursor-pos (min (length buffer) (1+ cursor-pos))
			    dirty t))
	 ((= q up) (setq cursor-pos (or (prev-pos buffer cursor-pos #\Newline) 0)
			 dirty t))
	 ((= q down) (setq cursor-pos (or (next-pos buffer cursor-pos #\Newline) (length buffer))
			   dirty t))
	 ((< q #xff)
	  (setq buffer (insert-char buffer (code-char q) cursor-pos)
		cursor-pos (1+ cursor-pos)
		dirty t)))))))

(defun type () (typo ""))

(defun repeat-string (str n)
  (let ((res ""))
    (dotimes (i n res)
      (setq res (concatenate 'string res str)))))

(defun gfx-repl ()
  (loop
   (with-gfx (gfx)
     (let ((typed (type)))
       (if (eq 0 (length typed)) (setq typed "nil"))
       (let ((evalled (eval (read-from-string typed))))
	 (princ #\Newline gfx) (princ #\Newline gfx)
	 (prin1 evalled gfx)
	 (terpri gfx) (terpri gfx)
	 (print "Press esc to continue" gfx)
	 (refresh)
	 (loop (if (eq (get-key) 27) (return))))))))
