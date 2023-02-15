(defun edit (fun)
  (if (not (boundp fun))
      (error "There is no function ~a" fun)))
; not done

(defun repeat-string (str n)
  (let ((res ""))
    (dotimes (i n res)
      (setq res (concatenate 'string res str)))))

(defun my-time (fun)
  (let ((start (millis))
	(res (fun)))
    (format gfx "~%~a ms" (- (millis) start))
    res))

(defun faces (n)
  (dotimes (i n)
    (draw-char (random 320) (random 240) (code-char (1+ (random 2))) black white (1+ (random 10))))
  (refresh))

