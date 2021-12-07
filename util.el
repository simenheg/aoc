(require 'seq)
(require 'cl-lib)

(defun slurp (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (string-trim (buffer-string)) "\n")))

(defun slurp-numbers (file)
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar #'string-to-number
            (split-string (string-trim (buffer-string)) ","))))

(defun transpose (matrix)
  (apply #'seq-mapn (lambda (&rest x) x) matrix))

(defun sum (list)
  (cl-reduce #'+ list))
