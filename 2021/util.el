(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defun split-lines (s) (split-string s "\n"))

(defun slurp-raw (file)
  (with-temp-buffer
    (insert-file-contents file)
    (string-trim (buffer-string))))

(defun slurp (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-lines (string-trim (buffer-string)))))

(defun slurp-numbers (file)
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar #'string-to-number
            (split-string (string-trim (buffer-string)) ","))))

(defun slurp-matrix (file)
  (mapcar
   (lambda (line)
     (mapcar
      (lambda (char)
        (string-to-number (char-to-string char)))
      (string-to-list line)))
   (slurp file)))

(defun mref (matrix x y)
  (when (and (<= 0 x) (<= 0 y))
    (elt (elt matrix y) x)))

(defun transpose (matrix)
  (apply #'seq-mapn (lambda (&rest x) x) matrix))

(defun sum (list)
  (cl-reduce #'+ list))
