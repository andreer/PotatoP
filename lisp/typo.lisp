#|

The humble beginnings of a text editor and repl

TODO:
- scrolling (pageup/pagedown)
- move by word
- highlight matching paren/quote
- optimizations
  - rewrite from single string to list of lines

|#

(defvar black 0)
(defvar white 1)

(defun typo (buffer)
  (let ((pos 0)
        (dirty t))
    (clear-key-buffer)
    (fill-screen white)
    (loop
     (let ((q (get-key)))
       (cond
         ((not q) (when dirty (time (display buffer pos)) (setq dirty nil)))
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
         ((= q down) (setq pos (or (next-pos buffer pos #\Newline)
				   (length buffer))
                           dirty t))
         ((< q #xff)
          (setq buffer (insert-char buffer (code-char q) pos)
                pos (1+ pos)
                dirty t)))))))

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
      (format out "Cells: ~a, Length: ~a~%~%" (room) l)
      (let* ((vis (n-lines-around 28 pos buffer))
             (pos (- pos (car vis)))
             (buffer (cadr vis))
             (l (length buffer)))
        (princ (subseq buffer 0 pos) out)
        (set-text-color white black)
        (if (>= pos l)
            (princ " " out)
            (progn
              (let ((ch (char buffer pos)))
		(if (eq ch #\Newline) (princ " " out))
		(princ ch out))
              (set-text-color black white)
              (if (< pos (1- l)) (princ (subseq buffer (1+ pos)) out)))))
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

;; Jeez what a mess of a function
(defun n-lines-around (n pos buffer)
  (let ((start pos)
	(end pos)
	(lines 0)
	(n (1+ n))) ; like wft even is this
    (loop
     (let ((prev (prev-pos buffer start #\Newline)))
       (when (null prev) (setq start 0) (incf lines) (return))
       (if (< (* 3 lines) n)
	   (setq start prev
		 lines (1+ lines))
	   (progn (setq start prev)
		  (incf lines)
		  (return)))))
    (if (and (< pos (length buffer))
	     (eq (char buffer pos) #\Newline))
	(incf lines))
    (dotimes (i (- n lines))
      (setq end (or (next-pos buffer end #\Newline)
                    (length buffer))))
    (if (and (> (length buffer) start) (eq (char buffer start) #\Newline)) (incf start))
    (list start (subseq buffer (or start 0) end))))

(defun clear-key-buffer ()
  (loop (if (not (get-key)) (return))))

(defun type () (typo ""))
