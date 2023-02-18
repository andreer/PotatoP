; a simple text editor with a list-of-strings data structure

(defun typo2 (list-of-strings)
  (defvar buffer list-of-strings)
  (defvar dirty t)
  (defvar pos '(0 . 0))
  (defvar cur black)
  (defvar cur-last (millis))
  (clear-key-buffer)
  (fill-screen white)
  (loop
   (let ((q (get-key)))
     (cond
       ((not q)
	(cond (dirty
	       (t2-display buffer pos)
	       (setq dirty nil))
	      (t (when (< (+ cur-last 600) (millis))
		   (setf cur (mod (1+ cur) 2)
			 cur-last (millis)
			 dirty t)))))
       ((= q (char-code #\Escape)) (return buffer))
       (t (time (progn (t2-handle-key q) nothing)))))))

(defun type2 ()
  (typo2 '("")))

(defun t2-handle-key (q)
  (setq cur black
	cur-last (millis)
	dirty t)
  (cond
    ((= q (char-code #\Newline))
     (let* ((line (nth (car pos) buffer))
	    (one (subseq line 0 (cdr pos)))
	    (two (subseq line (cdr pos) (length line))))
       (setq buffer (t2-remove-line buffer (car pos))
	     buffer (t2-insert-line buffer (car pos) two)
	     buffer (t2-insert-line buffer (car pos) one)
	     pos (cons (car (t2-next-line pos)) 0))))
    ((= q (char-code #\Backspace))
     (t2-del-char-lol))
    ((= q left) (setq pos (t2-dec-pos pos buffer)))
    ((= q right) (setq pos (t2-inc-pos pos buffer)))
    ((= q up) (setq pos (t2-prev-line pos buffer)))
    ((= q down) (setq pos (t2-next-line pos)))
    ((= q (char-code #\z)) (setq pos (cons (1- (length buffer)) 0)))
    ((< q #xff)
     (setq buffer (t2-ins-char-lol buffer (code-char q) pos)
           pos (t2-inc-pos pos buffer)))))

(defun t2-equal (x y)
  (cond
   ((and (stringp x) (stringp y)) (string= x y))
   ((and (consp x) (consp y)) (and (t2-equal (car x) (car y)) (t2-equal (cdr x) (cdr y))))
   (t (eq x y))))

(defun t2-insert-line (buffer n line) ; probably too slow
  (append (subseql buffer 0 n) (list line) (subseql buffer n (length buffer))))

(defun t2-remove-line (buffer n) ; probably too slow
  (append (subseql buffer 0 n) (subseql buffer (1+ n) (length buffer))))

(defun t2-display (buffer pos)
  (let ((l (length buffer)))
    (set-cursor 0 0)
    (set-text-color black white)
    (set-text-wrap nil)
    (fill-screen white)
    (with-gfx (out)
      (gc)
      (format out "~a Pos: ~a / ~a~%~%" (room) pos l)
      (let* ((vis (t2-n-lines-around 28 pos buffer))
             (rel (cons (- (car pos) (car vis)) (cdr pos)))
             (vis-lines (cdr vis)))
	(let ((line-num 0))
	  (dolist (line vis-lines)
	    (set-cursor 1 (* 8 (+ line-num 2)))
	    (cond ((= line-num (car rel))
		   (princ (subseq line 0 (cdr rel)) out)
		   (set-text-color white black)
		   (if (= (cdr rel) (length line))
		       (princ " " out)
		       (princ (char line (cdr rel)) out))
		   (set-text-color black white)		   
		   (princ (subseq line (1+ (cdr rel))) out)
		   (terpri out))
		  (t (princ line out) (terpri out)))
	    (incf line-num)))
	(t2-draw-cursor (* 6 (cdr rel)) (* 8 (+ 2 (car rel))))
	(refresh)))))

(defun t2-draw-cursor (x y)
  (draw-line x (1- y) x (+ y 8) cur))

(defun t2-dec-pos (pos buffer)
  (cond ((= 0 (car pos))
	 (cons 0 (max 0 (1- (cdr pos)))))
	((= 0 (cdr pos))
	 (cons (1- (car pos))
	       (length (nth (1- (car pos)) buffer))))
	(t (cons (car pos) (1- (cdr pos))))))

(defun t2-inc-pos (pos buffer)
  (if (< (cdr pos) (length (nth (car pos) buffer)))
      (cons (car pos) (1+ (cdr pos)))
      (let* ((line (min (1- (length buffer)) (1+ (car pos))))
	     (idx (min (length (nth line buffer)) (cdr pos))))
	(cons line idx))))

(defun t2-next-line (pos)
  (let* ((line (min (1- (length buffer)) (1+ (car pos))))
	 (idx (min (cdr pos) (length (nth line buffer)))))
    (cons line idx)))

(defun t2-read-lines (filename)
  (with-sd-card (sd filename)
    (let ((res nil))
      (loop
	    (let ((ln (read-line sd)))
	      (if (not ln) (return)
		  (setq res (cons ln res)))))
      (reverse res))))

(defun t2-prev-line (pos buffer)
  (let* ((line (max 0 (1- (car pos))))
	 (idx (min (cdr pos) (length (nth line buffer)))))
    (cons line idx)))

(defun t2-ins-char-str (buffer char pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (princ-to-string char)
               (subseq buffer pos)))

(defun t2-del-char-str (buffer pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (subseq buffer (1+ pos))))

(defun t2-ins-char-lol (buffer char pos)
  (setf (nth (car pos) buffer)
	(t2-ins-char-str
	 (nth (car pos) buffer) char (cdr pos)))
  buffer)

(defun t2-del-char-lol ()
  (cond ((and (= 0 (cdr pos))
	      (/= 0 (car pos)))
	 (let* ((prev-line (nth (1- (car pos)) buffer))
		(this-line (nth (car pos) buffer))
		(new-line (concatenate 'string prev-line this-line))
		(new-pos (cons (1- (car pos)) (length prev-line))))
	   (setf buffer (t2-remove-line buffer (1- (car pos)))
		 buffer (t2-remove-line buffer (1- (car pos)))
		 buffer (t2-insert-line buffer (1- (car pos)) new-line)
		 pos new-pos))
	 buffer)
	((> (cdr pos) 0)
	 (setf (nth (car pos) buffer)
	       (t2-del-char-str
		(nth (car pos) buffer) (1- (cdr pos)))
	       pos (cons (car pos) (1- (cdr pos))))
	 buffer)))

(defun t2-prev-pos (buffer pos ch)
  (loop
   (decf pos)
   (if (< pos 0) (return nil))
   (if (eq ch (char buffer pos)) (return pos))))

(defun t2-next-pos (buffer pos ch)
  (let ((l (length buffer)))
    (loop
     (incf pos)
     (if (>= pos l) (return nil))
     (if (eq ch (char buffer pos)) (return pos)))))

;; Still super slow because it iterates m times
(defun t2-subseq* (lst n m)
  (let ((head lst)
	(res nil)
	(skip n)
	(take (- m n)))
    (loop
     (cond ((> skip 0)
	    (setq skip (1- skip)
		  head (cdr head)))
	   ((> take 0)
	    (setq take (1- take)
		  res (cons (car head) res)
		  head (cdr head)))
	   (t (return))))
    (reverse res)))

;; TODO: find why this one doesn't work
(defun t2-subseq-buggy (lst n m)
  (let ((res nil)
    (dotimes (i (- m n))
      (setq res (cons (nth (+ n i) lst) res))))))

(defun t2-subseq (lst n m)
  (let ((res nil)
	(i n))
    (loop
	  (if (< i m)
	      (setq res (cons (nth i lst) res)
		    i (1+ i))
	      (return)))
    (reverse res)))

(defun t2-n-lines-around (lines pos buffer)
  "Returns the (max) n lines from the buffer surrounding the position."
  (if (> lines (length buffer)) (cons 0 buffer)
      (let* ((first (max 0 (- (car pos) (floor (/ lines 2)))))
	     (last (min (+ first lines) (length buffer))))
	(cons first (subseql buffer first last)))))

(defun t2-prev-pos-str (buffer pos ch)
  (loop
   (decf pos)
   (if (< pos 0) (return nil))
   (if (eq ch (char buffer pos)) (return pos))))

(defun t2-next-pos-str (buffer pos ch)
  (let ((l (length buffer)))
    (loop
     (incf pos)
     (if (>= pos l) (return nil))
     (if (eq ch (char buffer pos)) (return pos)))))

(defun t2-prev-pos-lol (buffer pos ch) ; WIP
  (loop
	(let ((prev (prev-pos-str buffer (cdr pos ch))))
	  (cond (prev (return prev))
		(= 0 (car pos) (return nil))
		(t setq pos (decf (cons (car pos) 0)))))))

;(defun next-pos-lol (buffer pos ch)) ; TODO

(defun t2 ()
  "read in a medium sized file for perf testing"
  (typo2 (t2-read-lines "me.txt")))
