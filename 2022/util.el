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

(defun slurp--grid (file transfun)
  (mapcar
   (lambda (line) (mapcar transfun (string-to-list line)))
   (slurp file)))

(defun slurp-matrix (file)
  (slurp--grid file (lambda (c) (string-to-number (char-to-string c)))))

(defun slurp-char-grid (file)
  (slurp--grid file #'identity))

(defun mref (matrix x y)
  (when (and (<= 0 x) (<= 0 y))
    (elt (elt matrix y) x)))

(defun mset (matrix x y val)
  (when (and (<= 0 x) (<= 0 y))
    (setf (elt (elt matrix y) x) val)))

(defun transpose (matrix)
  (apply #'seq-mapn (lambda (&rest x) x) matrix))

(defun sum (list)
  (cl-reduce #'+ list))
