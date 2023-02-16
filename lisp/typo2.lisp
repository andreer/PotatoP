(defun typo2 (list-of-strings)
  (defvar buffer list-of-strings)
  (defvar dirty t)
  (defvar pos '(0 . 0))
  (clear-key-buffer)
  (fill-screen white)
  (loop
   (let ((q (get-key)))
     (cond
       ((not q) (when dirty (t2-display buffer pos) (setq dirty nil)))
       ((= q (char-code #\Escape)) (return buffer))
       (t (t2-handle-key q))))))

(defun type2 ()
  (typo2 '("")))

(defun t2-handle-key (q)
  (cond
    ((= q (char-code #\Backspace))
     (let ((before (t2-dec-pos pos buffer)))
       (if (not (t2-equal pos before))
	   (t2-del-char-lol buffer before))
       (setq pos before
	     dirty t)))
    ((= q left) (setq pos (t2-dec-pos pos buffer)
                      dirty t))
    ((= q right) (setq pos (t2-inc-pos pos buffer)
                       dirty t))
    ((= q up) (setq pos (t2-prev-line pos buffer)
                    dirty t))
    ((= q down) (setq pos (t2-next-line pos)
                      dirty t))
    ((< q #xff)
     (setq buffer (t2-ins-char-lol buffer (code-char q) pos)
           pos (t2-inc-pos pos buffer)
           dirty t))))

(defun t2-equal (x y)
  (cond
   ((and (stringp x) (stringp y)) (string= x y))
   ((and (consp x) (consp y)) (and (t2-equal (car x) (car y)) (t2-equal (cdr x) (cdr y))))
   (t (eq x y))))

(defun t2-display (buffer pos)
  (let ((l (length buffer)))
    (set-cursor 0 0)
    (set-text-color black white)
    (fill-screen white)
    (with-gfx (out)
      (format out "Cells: ~a, Lines: ~a, Pos: ~a~%~%" (room) l pos)
      (let* ((vis (t2-n-lines-around 20 pos buffer))
             (rel (cons (- (car pos) (car vis)) (cdr pos)))
             (vis-lines (cdr vis)))
	(let ((line-num 0))
	  (dolist (line vis-lines)
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
	(refresh)))))

(defun t2-dec-pos (pos buffer)
  (cond ((= 0 (car pos))
	 (cons 0 (max 0 (1- (cdr pos)))))
	((= 0 (cdr pos))
	 (cons (1- (car pos))
	       (1- (length (nth (1- (car pos)) buffer)))))
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

(defun t2-reduce (fn lst)
  (if (null (cdr lst)) (car lst)
      (funcall fn (car lst) (t2-reduce fn (cdr lst)))))


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

(defun t2-del-char-lol (buffer pos)
    (setf (nth (car pos) buffer)
	(t2-del-char-str
	 (nth (car pos) buffer) (cdr pos)))
  buffer)

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

(defun t2-subseq* (lst n m)
  "Returns the subsequence of lst from item n to item m-1."
  ;; from http://www.ulisp.com/show?3GSE 
  (cond
   ((> n 0) (t2-subseq* (cdr lst) (1- n) (1- m)))
   ((zerop m) nil)
   (t (cons (car lst) (t2-subseq* (cdr lst) 0 (1- m))))))

(defun t2-n-lines-around (lines pos buffer)
  "Returns the (max) n lines from the buffer surrounding the position."
  (if (> lines (length buffer)) (cons 0 buffer)
      (let* ((first (max 0 (- (car pos) (floor (/ lines 2)))))
	     (last (min (+ first lines) (length buffer))))
	(cons first (t2-subseq* buffer first last)))))

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
