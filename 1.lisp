(defun slurp (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          collect line)))

(defun slurp-numbers (file)
  (mapcar #'parse-integer (slurp file)))

(defun count-increasing (lst)
  (loop for x on lst count (> (or (cadr x) 0) (or (car x) 0))))

(defun part1 (file)
  (count-increasing (mapcar #'parse-integer (slurp file))))

(defun window (lst)
  (when (and lst (cdr lst) (cddr lst))
    (+ (car lst) (cadr lst) (caddr lst))))

(defun part2 (file)
  (count-increasing (maplist #'window (slurp-numbers file))))
