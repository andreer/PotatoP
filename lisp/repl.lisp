(defun wait-for-key-press (gfx)
  (format gfx " Press any key to continue~%")
  (refresh)
  (t2-clear-key-buffer)
  (loop (let ((key (get-key)))
          (if (and key (not (logbitp 15 key)))
              (return)))))

(defun gfx-repl ()
  (loop
   (ignore-errors
     (with-gfx (gfx)
       (let ((inp (t2-join-lines (type2))))
         (if (= 1 (length inp)) (return))
         (error) ; clear any previous error
         (setq inp (concatenate 'string
                    "(progn "
                    inp
                    " )"))
         (ignore-errors
           (print (eval (read-from-string inp)) gfx))
         (fill-rect 0 200 320 40 white)
         (draw-rect 0 200 320 40 black)
         (set-cursor 0 205)
         (if (get-error)
             (format gfx " Error: ~a~%"
                     (get-error)))
         (wait-for-key-press gfx))))))

(defun load (filename)
 (eval
  (read-from-string
   (t2-join-lines
    (append
     (list "(progn ")
     (t2-read-lines filename)
     (list ")"))))))
