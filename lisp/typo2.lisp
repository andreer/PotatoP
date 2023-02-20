;; a simple text editor with a
;; list-of-strings data structure

(defvar black 0)
(defvar white 1)

(defvar left #x11)
(defvar right #x10)
(defvar up #x1e)
(defvar down #x1f)

(defun t2-clear-key-buffer ()
  (loop (if (not (get-key)) (return))))

(defun typo2 (list-of-strings)
  (defvar buffer list-of-strings)
  (defvar dirty t)
  (defvar pos '(0 . 0))
  (defvar cur black)
  (defvar cur-last (millis))
  (t2-clear-key-buffer)
  (fill-screen white)
  (loop
   (let ((q (get-key)))
     (cond
       ((not q)
        (cond (dirty
               (t2-display buffer pos)
               (setq dirty nil))
              (t (when (< (+ cur-last 600) (millis))
                   (setf cur (mod (1+ cur) 2)
                         cur-last (millis)
                         dirty t)))))
       ((= q (char-code #\Escape)) (return buffer))
       (t (t2-handle-key q))))))

(defun type2 ()
  (typo2 (list "")))

(defun t2-handle-key (q)
  (setq cur black
        cur-last (millis)
        dirty t)
  (cond
    ((= q (char-code #\Newline))
     (let* ((line (nth (car pos) buffer))
            (one (subseq line 0 (cdr pos)))
            (two (subseq line (cdr pos)
                         (length line))))
       (setq buffer (t2-remove-line buffer (car pos))
             buffer (t2-insert-line buffer
                                    (car pos) two)
             buffer (t2-insert-line buffer
                                    (car pos) one)
             pos (cons (car (t2-next-line pos)) 0))))
    ((= q (char-code #\Backspace))
     (t2-del-char-los))
    ((= q left) (setq pos (t2-dec-pos pos buffer)))
    ((= q right) (setq pos (t2-inc-pos pos buffer)))
    ((= q up) (setq pos (t2-prev-line pos buffer)))
    ((= q down) (setq pos (t2-next-line pos)))
    ((= q (char-code #\Escape)) ; find what "end" is
     (setq pos (cons (1- (length buffer)) 0)))
    ((not (plusp q)) nothing) ;ignore keyup events
    ((/= 255 (logand 255 q)) ; modifier
     (setq buffer
           (t2-ins-char-los buffer
                            (code-char
                             (logand 255 q))
                            pos)
           pos
           (t2-inc-pos pos buffer)))))

(defun t2-insert-line (buffer n line)
  (append (t2-subseql buffer 0 n)
          (list line)
          (t2-subseql buffer n (length buffer))))

(defun t2-remove-line (buffer n)
  (append (t2-subseql buffer 0 n)
          (t2-subseql buffer (1+ n)
                      (length buffer))))

(defun t2-display (buffer pos)
  (let ((l (length buffer)))
    (set-cursor 0 0)
    (set-text-color black white)
    (set-text-wrap nil)
    (fill-screen white)
    (with-gfx (out)
      (let* ((vis (t2-n-lines-around 30 pos buffer))
             (rel (cons (- (car pos) (car vis)) (cdr pos)))
             (vis-lines (cdr vis)))
        (let ((line-num 0))
          (dolist (line vis-lines)
            (set-cursor 1 (* 8 line-num))
            (cond ((= line-num (car rel))
                   (princ (subseq line 0 (cdr rel)) out)
                   (set-text-color white black)
                   (if (= (cdr rel) (length line))
                       (princ " " out)
                       (princ (char line (cdr rel)) out))
                   (set-text-color black white)
                   (princ (subseq line (1+ (cdr rel))) out)
                   (terpri out))
                  (t (princ line out) (terpri out)))
            (incf line-num)))
        (t2-draw-cursor (* 6 (cdr rel)) (* 8 (car rel)))
        (refresh)))))

(defun t2-draw-cursor (x y)
  (draw-line x (1- y) x (+ y 8) cur))

(defun t2-dec-pos (pos buffer)
  (cond ((= 0 (car pos))
         (cons 0 (max 0 (1- (cdr pos)))))
        ((= 0 (cdr pos))
         (cons (1- (car pos))
               (length (nth (1- (car pos)) buffer))))
        (t (cons (car pos) (1- (cdr pos))))))

(defun t2-inc-pos (pos buffer)
  (if (< (cdr pos) (length (nth (car pos) buffer)))
      (cons (car pos) (1+ (cdr pos)))
      (let* ((line (min (1- (length buffer)) (1+ (car pos))))
             (idx (min (length (nth line buffer)) (cdr pos))))
        (cons line idx))))

(defun t2-next-line (pos)
  (let* ((line (min (1- (length buffer)) (1+ (car pos))))
         (idx (min (cdr pos) (length (nth line buffer)))))
    (cons line idx)))

(defun t2-prev-line (pos buffer)
  (let* ((line (max 0 (1- (car pos))))
         (idx (min (cdr pos) (length (nth line buffer)))))
    (cons line idx)))

(defun t2-ins-char-str (buffer char pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (princ-to-string char)
               (subseq buffer pos)))

(defun t2-del-char-str (buffer pos)
  (concatenate 'string
               (subseq buffer 0 pos)
               (subseq buffer (1+ pos))))

(defun t2-ins-char-los (buffer char pos)
  (setf (nth (car pos) buffer)
        (t2-ins-char-str
         (nth (car pos) buffer) char (cdr pos)))
  buffer)

(defun t2-del-char-los ()
  (cond ((and (= 0 (cdr pos))
              (/= 0 (car pos)))
         (let* ((prev-line (nth (1- (car pos)) buffer))
                (this-line (nth (car pos) buffer))
                (new-line (concatenate 'string prev-line this-line))
                (new-pos (cons (1- (car pos)) (length prev-line))))
           (setf buffer (t2-remove-line buffer (1- (car pos)))
                 buffer (t2-remove-line buffer (1- (car pos)))
                 buffer (t2-insert-line buffer (1- (car pos)) new-line)
                 pos new-pos))
         buffer)
        ((> (cdr pos) 0)
         (setf (nth (car pos) buffer)
               (t2-del-char-str
                (nth (car pos) buffer) (1- (cdr pos)))
               pos (cons (car pos) (1- (cdr pos))))
         buffer)))

(defun t2-prev-pos (buffer pos ch)
  (loop
   (decf pos)
   (if (< pos 0) (return nil))
   (if (eq ch (char buffer pos)) (return pos))))

(defun t2-next-pos (buffer pos ch)
  (let ((l (length buffer)))
    (loop
     (incf pos)
     (if (>= pos l) (return nil))
     (if (eq ch (char buffer pos)) (return pos)))))

(defun t2-n-lines-around (lines pos buffer)
  "Returns the (max) n lines from the buffer surrounding the position."
  (if (> lines (length buffer)) (cons 0 buffer)
      (let* ((first (max 0 (- (car pos) (floor (/ lines 2)))))
             (last (min (+ first lines) (length buffer))))
        (cons first (t2-subseql buffer first last)))))

(defun t2-subseql (list n m)
  (reverse (subseql list n m)))

(defun t2-prev-pos-str (buffer pos ch)
  (loop
   (decf pos)
   (if (< pos 0) (return nil))
   (if (eq ch (char buffer pos)) (return pos))))

(defun t2-next-pos-str (buffer pos ch)
  (let ((l (length buffer)))
    (loop
     (incf pos)
     (if (>= pos l) (return nil))
     (if (eq ch (char buffer pos)) (return pos)))))

(defun t2-prev-pos-los (buffer pos ch) ; WIP
  (loop
   (let ((prev (prev-pos-str buffer
                             (cdr pos ch))))
     (cond (prev (return prev))
           (= 0 (car pos) (return nil))
           (t setq pos
              (decf (cons (car pos) 0)))))))

;;(defun next-pos-los (buffer pos ch)) ; TODO

(defun t2-join-lines (los)
  (with-output-to-string (str)
    (dolist (line los)
      (princ line str)
      (terpri str))))

(defun t2-read-lines (filename)
  (with-sd-card (sd filename)
    (let ((res nil))
      (loop
       (let ((ln (read-line sd)))
         (if (not ln) (return)
             (setq res (cons ln res)))))
      (reverse res))))

(defun t2-write-lines (los filename)
  (with-sd-card (sd filename 2)
    (dolist (line los 'saved)
      (write-line line sd))))

(defun t2 (filename)
  (t2-write-lines
   (typo2 (t2-read-lines filename))
   filename))
