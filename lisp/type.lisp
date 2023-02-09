;; The humble beginnings of a text editor

(defun type ()
  (let ((p 1))
    (fill-screen 1)
    (refresh)
    (loop
     (let ((q (get-key)))
       (if (and q (< q #xff))
	   (progn
	     (draw-char (* p 6) 10 (code-char q) 0)
	     (set 'p (+ p 1))
	     (refresh)))))))
