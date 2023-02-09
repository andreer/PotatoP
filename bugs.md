BUGS
----

Stack overflow causes ulisp to hang:
(defun fill () (fill) nil)

Various input strings can cause the reader to hang or crash:
"("
"\""

