(defun edit (fun)
  (if (not (boundp fun))
      (error "There is no function ~a" fun)))

(defun repeat-string (str n)
  (let ((res ""))
    (dotimes (i n res)
      (setq res (concatenate 'string res str)))))

(defun my-time (fun)
  (let ((start (millis))
	(res (fun)))
    (format gfx "~%~a ms" (- (millis) start))
    res))

(defun fib (n)
  (if (< n 3) 1
      (+
       (fib (- n 1))
       (fib (- n 2)))))

(defun pow (a b)
  (if (= b 0) 1
      (* a (pow a (1- b)))))

(defun faces (n)
  (dotimes (i n)
    (draw-char (random 320) (random 240) (code-char (1+ (random 2))) black white (1+ (random 10))))
  (refresh))

