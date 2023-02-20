(defun tak (x y z)
  (if (not (< y x))
      z
    (tak
     (tak (1- x) y z)
     (tak (1- y) z x)
     (tak (1- z) x y))))

(defun fib (n)
  (if (< n 3) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun q (n)
  (if (<= n 2) 1
    (+
     (q (- n (q (- n 1))))
     (q (- n (q (- n 2)))))))

(defun q2 (x y)
  (if (or (< x 1) (< y 1)) 1
    (+ (q2 (- x (q2 (1- x) y)) y)
       (q2 x (- y (q2 x (1- y)))))))

(defun factor (n)
  (let ((d 2) (i 1))
    (loop
     (when (> (* d d) n) (return n))
     (when (zerop (mod n d)) (return d))
     (incf d i) (setq i 2))))

(defun factorize (n)
  (let ((f (factor n)))
    (if (= n f) (list n)
	(cons f (factorize (/ n f))))))

(defun crc32 (str)
  (let ((crc #xFFFFFFFF))
    (dotimes (k (length str))
      (let* ((c (char str k))
             (n (char-code c)))
        (dotimes (i 8)
          (setq crc 
                (if (oddp (logxor n crc))
                    (logxor
		     (logand (ash crc -1) #x7FFFFFFF)
		     #xEDB88320)
                    (logand (ash crc -1) #x7FFFFFFF)))
          (setq n (ash n -1)))))
    (logxor crc #xFFFFFFFF)))

