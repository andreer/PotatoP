(defun pp (q)
 (pprint q gfx))

(defun ed ()
 (setq me (typo me))
 (length me))

(defun run ()
 (eval (read-from-string me)))

(defun save ()
 (with-sd-card (sd "me.txt" 2) (print me sd))
 (length me))

(defun load ()
 (defvar me (with-sd-card (sd "me.txt") (read sd)))
 (length me))

(defun done ()
 'done)
