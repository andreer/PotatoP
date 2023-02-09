#|

The idea here is to create a parser that can validate a set of ulisp statements
before it is passed to the reader, in order to avoid invoking undefined behaviour
(and also, perhaps, to get better error messages).

|#

(defun parse-error (str)
  (error (concatenate 'string "Parse error: " str)))

(defun parse (str)
  (if (not (stringp str))
      (error "not a string!")
      (parse-from str 0)))

(defun parse-from (str pos)
  (let ((l (length str)))
    (if (= l pos) nil
	(let ((ch (char str pos)))
	  (if (eq ch #\")
	      (parse-string str (+1 pos))
	      "dunno")))))

(defun parse-string (str start)
  (let ((pos start))
    (loop
	  (if (>= pos (length str))
	      (error "end of file reached while parsing quoted string!")
	      (if (eq (char str pos) #\") (return (subseq str start pos)))))))
							  
(parse 'cake)
(parse "")
