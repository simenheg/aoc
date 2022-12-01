(load-file "util.el")

;; The transparent paper is marked with random dots and includes
;; instructions on how to fold it up (your puzzle input). For example:

;; 6,10
;; 0,14
;; 9,10
;; 0,3
;; 10,4
;; 4,11
;; 6,0
;; 6,12
;; 4,1
;; 0,13
;; 10,12
;; 3,4
;; 3,0
;; 8,4
;; 1,10
;; 2,14
;; 8,10
;; 9,0

;; fold along y=7
;; fold along x=5

;; The first section is a list of dots on the transparent paper. 0,0
;; represents the top-left coordinate. The first value, x, increases
;; to the right. The second value, y, increases downward. So, the
;; coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is
;; below 0,0. The coordinates in this example form the following
;; pattern, where # is a dot on the paper and . is an empty, unmarked
;; position:

;; ...#..#..#.
;; ....#......
;; ...........
;; #..........
;; ...#....#.#
;; ...........
;; ...........
;; ...........
;; ...........
;; ...........
;; .#....#.##.
;; ....#......
;; ......#...#
;; #..........
;; #.#........

;; Then, there is a list of fold instructions. Each instruction
;; indicates a line on the transparent paper and wants you to fold the
;; paper up (for horizontal y=... lines) or left (for vertical
;; x=... lines). In this example, the first fold instruction is fold
;; along y=7, which designates the line formed by all of the positions
;; where y is 7 (marked here with -):

;; ...#..#..#.
;; ....#......
;; ...........
;; #..........
;; ...#....#.#
;; ...........
;; ...........
;; -----------
;; ...........
;; ...........
;; .#....#.##.
;; ....#......
;; ......#...#
;; #..........
;; #.#........

;; Because this is a horizontal line, fold the bottom half up. Some of
;; the dots might end up overlapping after the fold is complete, but
;; dots will never appear exactly on a fold line. The result of doing
;; this fold looks like this:

;; #.##..#..#.
;; #...#......
;; ......#...#
;; #...#......
;; .#.#..#.###
;; ...........
;; ...........

;; Now, only 17 dots are visible.

;; Notice, for example, the two dots in the bottom left corner before
;; the transparent paper is folded; after the fold is complete, those
;; dots appear in the top left corner (at 0,0 and 0,1). Because the
;; paper is transparent, the dot just below them in the result (at
;; 0,3) remains visible, as it can be seen through the transparent
;; paper.

;; Also notice that some dots can end up overlapping; in this case,
;; the dots merge together and become a single dot.

;; The second fold instruction is fold along x=5, which indicates this
;; line:

;; #.##.|#..#.
;; #...#|.....
;; .....|#...#
;; #...#|.....
;; .#.#.|#.###
;; .....|.....
;; .....|.....

;; Because this is a vertical line, fold left:

;; #####
;; #...#
;; #...#
;; #...#
;; #####
;; .....
;; .....

;; The instructions made a square!

;; The transparent paper is pretty big, so for now, focus on just
;; completing the first fold. After the first fold in the example
;; above, 17 dots are visible - dots that end up overlapping after the
;; fold is completed count as a single dot.

;; How many dots are visible after completing just the first fold
;; instruction on your transparent paper?

(defun parse-part-1 (line)
  (mapcar #'string-to-number (split-string line ",")))

(defun parse-part-2 (line)
  (pcase-let ((`(,axis ,n) (split-string
                            (string-trim-left line "fold along ")
                            "=")))
    (list axis (string-to-number n))))

(defun parse (file)
  "Return (DOTS FOLDS)."
  (let ((in (split-string (slurp-raw file) "\n\n")))
    (pcase-let ((`(,part1 ,part2) (mapcar #'split-lines in)))
      (list
       (mapcar #'parse-part-1 part1)
       (mapcar #'parse-part-2 part2)))))

(defun xmax (dots) (+ 1 (seq-max (mapcar #'car dots))))
(defun ymax (dots) (+ 1 (seq-max (mapcar #'cadr dots))))

(defun make-paper (dots)
  (let ((paper
         (make-list* (ymax dots) (lambda () (make-list (xmax dots) 0)))))
    (pcase-dolist (`(,x, y) dots)
      (cl-incf (nth x (nth y paper))))
    paper))

  ;; a b c d e
  ;; f g h i j
  ;; k l m n o
  ;; p q r s t
  ;; u v w x y

  ;; fold = (0 2)

  ;; (0 0) = (0 4)
  ;; (1 0) = (1 4)
  ;; (2 0) = (2 4)
  ;; (3 0) = (3 4)
  ;; (4 0) = (4 4)

  ;; (0 1) = (0 3)
  ;; (1 1) = (1 3)
  ;; (2 1) = (2 3)
  ;; (3 1) = (3 3)
  ;; (4 1) = (4 3)

  ;; (0 2) --- (4 2)

(defun fold (paper axis n)
  "Fold PAPER along AXIS at line N."
  (when (equal axis "x")
    (setq paper (transpose paper)))
  (dotimes (y n)
    (dotimes (x (length (car paper)))
      (cl-incf (nth x (nth y paper))
               (nth x (nth (- (length paper) y 1) paper)))))
  (setq paper (seq-take paper n))
  (if (equal axis "x")
      (transpose paper)
    paper))

(defun part1 (file)
  (pcase-let ((`(,dots ,folds) (parse file)))
    (let ((paper (make-paper dots))
          (fold (car folds)))
      (seq-count
       #'cl-plusp
       (flatten-list (fold paper (car fold) (cadr fold)))))))

(defun print-paper (paper)
  (dolist (line paper)
    (dolist (d line)
      (princ (if (> d 0) "#" " ")))
    (princ "\n")))

(defun part2 (file)
  (pcase-let ((`(,dots ,folds) (parse file)))
    (let ((paper (make-paper dots)))
      (pcase-dolist (`(,axis ,n) folds)
        (setq paper (fold paper axis n)))
      (print-paper paper))))
