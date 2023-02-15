
(defvar black 0)
(defvar white 1)

(defun typo (buffer)
  (defvar *typo-buffer* buffer)
  (defvar dirty t)
  (defvar *pos* '(0 . 0))
  (clear-key-buffer)
  (fill-screen white)
  (loop
   (let ((q (get-key)))
     (cond
       ((not q) (when dirty (typo-redisplay) (setq dirty nil)))
       ((= q (char-code #\Escape)) (return *typo-buffer*))
       (t (handle-key q))))))

(defun dec-pos (pos buffer)
  (cond ((= 0 (car pos))
	 (cons 0 (max 0 (1- (cdr pos)))))
	((= 0 (cdr pos))
	 (cons (1- (car pos))
	       (1- (length (nth (1- (car pos)) buffer)))))
	(t (cons (car pos) (1- (cdr pos))))))

(defun inc-pos (pos buffer)
  (if (< (cdr pos) (length (nth (car pos) buffer)))
      (cons (car pos) (1+ (cdr pos)))
      (cons (min (length buffer) (1+ (car pos))) 0)))

(defun ins-char-str (buffer char pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (princ-to-string char)
               (subseq buffer pos)))

(defun del-char-str (buffer pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (subseq buffer (1+ pos))))

(defun ins-char-lol (buffer char pos)
  (setf (nth (car pos) buffer)
	(insert-char-str
	 (nth (car pos) buffer) char (cdr pos)))
  buffer)

(defun del-char-lol (buffer pos)
    (setf (nth (car pos) buffer)
	(del-char-str
	 (nth (car pos) buffer) (cdr pos)))
  buffer)

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

(defun subseq* (lst n m)
  "Returns the subsequence of lst from item n to item m-1."
  ;; from http://www.ulisp.com/show?3GSE 
  (cond
   ((> n 0) (subseq* (cdr lst) (1- n) (1- m)))
   ((zerop m) nil)
   (t (cons (car lst) (subseq* (cdr lst) 0 (1- m))))))

(defun n-lines-around (lines pos buffer)
  "Returns the (max) n lines from the buffer surrounding the position."
  (if (> lines (length buffer)) buffer
      (let* ((first (max 0 (- (car pos) (floor (/ lines 2)))))
	     (last (min (+ first lines) (length buffer))))
	(subseq* buffer first last))))

(defun prev-pos-str (buffer pos ch)
  (loop
   (decf pos)
   (if (< pos 0) (return nil))
   (if (eq ch (char buffer pos)) (return pos))))

(defun next-pos-str (buffer pos ch)
  (let ((l (length buffer)))
    (loop
     (incf pos)
     (if (>= pos l) (return nil))
     (if (eq ch (char buffer pos)) (return pos)))))

(defun prev-pos-lol (buffer pos ch) ; WIP
  (loop
	(let ((prev (prev-pos-str buffer (cdr pos ch))))
	  (cond (prev (return prev))
		(= 0 (car pos) (return nil))
		(t setq pos (decf (cons (car pos) 0)))))))

(defun next-pos-lol (buffer pos ch)) ; TODO
