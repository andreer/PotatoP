;; FFT code from http://www.ulisp.com/show?27ES

(defun cx (x y) (cons x y))

(defun c+ (x y)
  (cx (+ (car x) (car y)) (+ (cdr x) (cdr y))))

(defun c- (x y)
  (cx (- (car x) (car y)) (- (cdr x) (cdr y))))

(defun c* (x y)
  (cx
   (- (* (car x) (car y)) (* (cdr x) (cdr y)))
   (+ (* (cdr x) (car y)) (* (car x) (cdr y)))))

(defun c/ (x y)
  (let ((den (+ (* (car y) (car y)) (* (cdr y) (cdr y)))))
    (cx
     (/ (+ (* (car x) (car y)) (* (cdr x) (cdr y))) den)
     (/ (- (* (cdr x) (car y)) (* (car x) (cdr y))) den))))

(defun cexp (x)
  (cx
   (* (exp (car x)) (cos (cdr x)))
   (* (exp (car x)) (sin (cdr x)))))

(defun cabs (x)
  (sqrt (+ (* (car x) (car x)) (* (cdr x) (cdr x)))))

(defvar pi 3.14159)

(defun evens (f)
  (if (null f) nil
      (cons (car f) (evens (cddr f)))))

(defun odds (f)
  (if (null f) nil
      (cons (cadr f) (odds (cddr f)))))

(defun fft (x)
  (if (= (length x) 1) x 
      (let*
          ((even (fft (evens x)))
           (odd (fft (odds x)))
           (k -1)
           (aux (mapcar 
		 (lambda (j) 
                   (c* (cexp (c/ (c* (cx 0 -2) (cx (* pi (incf k)) 0)) (cx (length x) 0))) j)) 
		 odd)))
	(append (mapcar c+ even aux) (mapcar c- even aux)))))

(defun fftr (r) (fft (mapcar (lambda (x) (cx x 0)) r)))

#| (pprint (fftr '(1 1 1 1 0 0 0 0))) |#

(defun bench-fft (times)
  (dotimes (i times)
  (fftr (let (z) (dotimes (x 16) (push 0 z)) (dotimes (x 16) (push 1 z)) z))))
