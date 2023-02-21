(defun wait-for-key-press (gfx)
  (format gfx "> Press any key to continue~%")
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
         (if (eq 0 (length inp)) (setq inp "()"))
         (if (string= "q" inp) (return))
         (error) ; clear any previous error
         (ignore-errors
           (terpri gfx)
           (print (eval (read-from-string inp)) gfx)
           (terpri gfx))
         (if (get-error)
             (format gfx "~%~%Error: ~a~%"
                     (get-error)))
         (refresh)
         (wait-for-key-press gfx))))))

(defun load (filename)
  (eval
   (read-from-string
    (t2-join-lines
     (append
      (list "(progn ")
      (t2-read-lines filename)
      (list ")"))))))
