(require 'seq)
(require 'cl-lib)

(defun slurp (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (string-trim (buffer-string)) "\n")))

(defun transpose (matrix)
  (apply #'seq-mapn (lambda (&rest x) x) matrix))
