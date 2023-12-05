(load-file "util.el")

;; The engine schematic (your puzzle input) consists of a visual
;; representation of the engine. There are lots of numbers and symbols
;; you don't really understand, but apparently any number adjacent to
;; a symbol, even diagonally, is a "part number" and should be
;; included in your sum. (Periods (.) do not count as a symbol.)

;; Here is an example engine schematic:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; In this schematic, two numbers are not part numbers because they
;; are not adjacent to a symbol: 114 (top right) and 58 (middle
;; right). Every other number is adjacent to a symbol and so is a part
;; number; their sum is 4361.

;; Of course, the actual engine schematic is much larger. What is the
;; sum of all of the part numbers in the engine schematic?

(defun nump (x)
  (member x '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defun symp (x)
  (and (stringp x) (not (nump x)) (not (equal x "."))))

(defun sym-adjacent-p (m x y)
  (seq-some
   #'symp
   (list (mref m (+ x 1) y) (mref m (- x 1) y)
         (mref m x (+ y 1)) (mref m x (- y 1))
         (mref m (+ x 1) (+ y 1)) (mref m (- x 1) (- y 1))
         (mref m (+ x 1) (- y 1)) (mref m (- x 1) (+ y 1)))))

(defun part1 (file)
  (let ((m (slurp-string-grid file))
        (hist '())
        (coord-hist '())
        (sum 0))
    (dotimes (y (length m))
      (dotimes (x (length (car m)))
        (let ((cur (mref m x y)))
          (cond
           ((nump cur)
            (push cur hist)
            (push (list x y) coord-hist))
           (hist
            (when (seq-some
                   (lambda (coord)
                     (sym-adjacent-p m (x coord) (y coord)))
                   coord-hist)
              (cl-incf sum (string-to-number (apply #'concat (reverse hist)))))
            (setq hist '())
            (setq coord-hist '()))))))
    sum))

;; The missing part wasn't the only issue - one of the gears in the
;; engine is wrong. A gear is any * symbol that is adjacent to exactly
;; two part numbers. Its gear ratio is the result of multiplying those
;; two numbers together.

;; This time, you need to find the gear ratio of every gear and add
;; them all up so that the engineer can figure out which gear needs to
;; be replaced.

;; Consider the same engine schematic again:

;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..

;; In this schematic, there are two gears. The first is in the top
;; left; it has part numbers 467 and 35, so its gear ratio is
;; 16345. The second gear is in the lower right; its gear ratio is
;; 451490. (The * adjacent to 617 is not a gear because it is only
;; adjacent to one part number.) Adding up all of the gear ratios
;; produces 467835.

;; What is the sum of all of the gear ratios in your engine schematic?

(defun gearp (x) (equal x "*"))

(defun gear-adjacent-p (m x y)
  (seq-some
   #'gearp
   (list (mref m (+ x 1) y) (mref m (- x 1) y)
         (mref m x (+ y 1)) (mref m x (- y 1))
         (mref m (+ x 1) (+ y 1)) (mref m (- x 1) (- y 1))
         (mref m (+ x 1) (- y 1)) (mref m (- x 1) (+ y 1)))))

(defun adjacent-gears (m x y)
  (let ((res '()))
    (dolist (diffs '((1 0) (-1 0) (0 1) (0 -1)
                     (1 1) (-1 -1) (1 -1) (-1 1)))
      (let ((coords (list (+ x (x diffs)) (+ y (y diffs)))))
        (when (gearp (mref m (x coords) (y coords)))
          (push coords res))))
    res))

(defun part2 (file)
  (let ((m (slurp-string-grid file))
        (hist '())
        (coord-hist '())
        (gears (make-hash-table :test #'equal)))
    (dotimes (y (length m))
      (dotimes (x (length (car m)))
        (let ((cur (mref m x y)))
          (cond
           ((nump cur)
            (push cur hist)
            (push (list x y) coord-hist))
           (hist
            (let ((new-gears
                   (apply
                    #'append
                    (seq-uniq
                     (mapcar
                      (lambda (coords)
                        (adjacent-gears m (x coords) (y coords)))
                      coord-hist)))))
              (dolist (gear new-gears)
                (push (string-to-number (apply #'concat (reverse hist)))
                      (gethash gear gears))))
            (setq hist '())
            (setq coord-hist '()))))))
    (sum
     (map-apply (lambda (k v) (apply #'* v))
                (map-filter (lambda (k v) (= (length v) 2))
                            gears)))))
