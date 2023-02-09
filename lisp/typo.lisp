#|

The humble beginnings of a text editor and repl

 TODO:
   - scrolling
   - move by word
   - highlight matching paren/quote
   - optimizations

|#

(defvar black 0)
(defvar white 1)

(defun insert-char (buffer char pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (princ-to-string char)
               (subseq buffer pos)))

(defun delete-char (buffer pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (subseq buffer (1+ pos))))

(defun display (buffer pos)
  (let ((l (length buffer)))
    (set-cursor 0 0)
    (set-text-color black white)
    (fill-screen white)
    (with-gfx (out)
      (princ "Cells: " out) (princ (room) out) (princ ", Length: " out) (princ l out) (terpri out) (terpri out)
      (princ (subseq buffer 0 pos) out)
      (set-text-color white black)
      (if (>= pos l) (princ " " out)
	  (progn (let ((ch (char buffer pos)))
	    (if (eq ch #\Newline)
		(princ " " out))
	    (princ ch out))
		 (set-text-color black white)
		 (if (< pos (1- l))
		     (princ (subseq buffer (1+ pos)) out))))
      (set-text-color black white)
      (refresh))))

(defvar left #x11)
(defvar right #x10)
(defvar up #x1e)
(defvar down #x1f)

(defun match-pos (buffer pos)
  (case (char buffer pos)
    #\" (match-dquote buffer pos)
    t nil))


; qwe "rty" ui "opasdf" ghjk

(defun match-quote (buffer ch pos)
  (let ((quot nil)
	(prev nil)
	(l (length buffer))
	(p 0))
    (loop
     (if (>= pos l) (return nil))
     (if (eq #\" (char buffer p))
	 (case quot
	   '() (setq quot t
		     prev p)
	   't (cond ((= prev pos) (return pos)
		    (= p pos) return prev)
		   t (setq quot nil
			   prev p))))
     (incf p))))

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
  (let ((pos 0)
        (dirty t))
    (loop (if (not (get-key)) (return)))
    (fill-screen white)
    (loop
     (let ((q (get-key)))
       (cond
         ((not q) (if dirty (progn (display buffer pos) (setq dirty nil))))
         ((= q (char-code #\Escape)) (return buffer))
         ((= q (char-code #\Backspace))
          (if (> pos 0)
	      (setq buffer (delete-char buffer (1- pos))
                    pos (1- pos)
                    dirty t)))
         ((= q left) (setq pos (max 0 (1- pos))
                           dirty t))
         ((= q right) (setq pos (min (length buffer) (1+ pos))
                            dirty t))
         ((= q up) (setq pos (or (prev-pos buffer pos #\Newline) 0)
                         dirty t))
         ((= q down) (setq pos (or (next-pos buffer pos #\Newline) (length buffer))
                           dirty t))
         ((< q #xff)
          (setq buffer (insert-char buffer (code-char q) pos)
                pos (1+ pos)
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
       (if (string= "q" typed) (return))
       (error) ; clear any previous error
       (ignore-errors
	 (let ((evalled (eval (read-from-string typed))))
	   (princ #\Newline gfx) (princ #\Newline gfx)
	   (prin1 evalled gfx)
	   (terpri gfx) (terpri gfx)))
       (if (get-error)
	   (progn
	     (terpri gfx) (terpri gfx)
	     (princ "Error: " gfx)
	     (princ (get-error) gfx)
	     (terpri gfx)))
       (princ "..." gfx)
       (refresh)
       (loop (if (eq (get-key) 27) (return)))))))
