(defun col (n) (if (> n 1) 1 0))

(defun sierpinski (x0 y0 size gen)
  (when (plusp gen)
    (let ((s (ash size -1)) (n (1- gen)))
      (fill-triangle
       (+ x0 s) y0
       x0 (+ y0 s s)
       (+ x0 s s) (+ y0 s s)
       (col gen))
      (refresh)
      (sierpinski (+ x0 (ash s -1)) y0 s n)
      (sierpinski x0 (+ y0 s) s n)
      (sierpinski (+ x0 s) (+ y0 s) s n))))

(defun s-draw ()
  (set-rotation 0)
  (fill-screen 1)
  (sierpinski 40 0 240 5))
