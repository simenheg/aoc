(defun slurp (file)
  (with-open-file (s file)
    (loop for line = (read-line s nil)
          while line
          collect line)))

(defun slurp-numbers (file)
  (mapcar #'parse-integer (slurp file)))
