(load-file "util.el")

(cl-loop for x on (mapcar #'string-to-number (slurp "in/1"))
         count (> (or (cadr x) 0) (car x)))
