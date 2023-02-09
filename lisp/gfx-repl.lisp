; requires typo

(defun wait-for-key-press (gfx)
  (format gfx "> Press any key to continue~%")
  (refresh)
  (clear-key-buffer)
  (loop (let ((key (get-key)))
	  (if (and key (not (logbitp 8 key))) (return)))))

(defun gfx-repl ()
  (loop
   (ignore-errors
     (with-gfx (gfx)
       (let ((typed (type)))
	 (if (eq 0 (length typed)) (setq typed "nil"))
	 (if (string= "q" typed) (return))
	 (error) ; clear any previous error
	 (ignore-errors
	   (format gfx "~%~%~a~%~%" (eval (read-from-string typed))))
	 (if (get-error)
	     (format gfx "~%~%Error: ~a~%" (get-error)))
	 (refresh)
	 (wait-for-key-press gfx))))))

(gfx-repl)