;; (cl-loop for x on (mapcar #'string-to-number (split-string (buffer-string) "\n"))
;;          count (> (or (cadr x) 0) (car x)))

(load "util.lisp")

(defun count-increasing (lst)
  (loop for x on lst count (> (or (cadr x) 0) (or (car x) 0))))

(defun part1 (file)
  (count-increasing (mapcar #'parse-integer (slurp file))))

(defun window (lst)
  (when (and lst (cdr lst) (cddr lst))
    (+ (car lst) (cadr lst) (caddr lst))))

(defun part2 (file)
  (count-increasing (maplist #'window (slurp-numbers file))))
