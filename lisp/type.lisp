;; The humble beginnings of a text editor

(defvar black 1)
(defvar white 0)

(defun insert-char (buffer char)
  (concatenate 'string buffer (princ-to-string char)))

(defun display (buffer)
  (fill-screen white)
  (set-text-color black white)
  (with-gfx (out)
    (princ buffer out)
    (princ #\176 out)))

(defun type ()
  (let ((buffer "")
	(cursor-pos 1)
	(font-width 6)
	(font-height 8))
    (fill-screen white)
    (refresh)
    (loop
     (let ((q (get-key)))
       (if (and q (< q #xff))
	   (progn
	     (setq buffer (insert-char buffer (char-code q)))
	     (incf cursor-pos)
	     (display buffer)))))))


	   ;(progn
	   ;  (draw-char (* cursor-pos font-width) font-height (code-char q) black)
	   ;  (set 'cursor-pos (+ cursor-pos 1))
	   ;  (refresh)))))))
