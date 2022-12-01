(load-file "util.el")

;; You come across a field of hydrothermal vents on the ocean floor!
;; These vents constantly produce large, opaque clouds, so it would be
;; best to avoid them if possible.

;; They tend to form in lines; the submarine helpfully produces a list
;; of nearby lines of vents (your puzzle input) for you to review. For
;; example:

;; 0,9 -> 5,9
;; 8,0 -> 0,8
;; 9,4 -> 3,4
;; 2,2 -> 2,1
;; 7,0 -> 7,4
;; 6,4 -> 2,0
;; 0,9 -> 2,9
;; 3,4 -> 1,4
;; 0,0 -> 8,8
;; 5,5 -> 8,2

;; Each line of vents is given as a line segment in the format x1,y1
;; -> x2,y2 where x1,y1 are the coordinates of one end the line
;; segment and x2,y2 are the coordinates of the other end. These line
;; segments include the points at both ends. In other words:

;;    An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
;;    An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

;; For now, only consider horizontal and vertical lines: lines where
;; either x1 = x2 or y1 = y2.

;; So, the horizontal and vertical lines from the above list would
;; produce the following diagram:

;; .......1..
;; ..1....1..
;; ..1....1..
;; .......1..
;; .112111211
;; ..........
;; ..........
;; ..........
;; ..........
;; 222111....

;; In this diagram, the top left corner is 0,0 and the bottom right
;; corner is 9,9. Each position is shown as the number of lines which
;; cover that point or . if no line covers that point. The top-left
;; pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row
;; is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

;; To avoid the most dangerous areas, you need to determine the number
;; of points where at least two lines overlap. In the above example,
;; this is anywhere in the diagram with a 2 or larger - a total of 5
;; points.

;; Consider only horizontal and vertical lines. At how many points do
;; at least two lines overlap?

(defun parse-point (point)
  (mapcar #'string-to-number (split-string point ",")))

(defun parse-line (line)
  (pcase-let ((`(,from ,_ ,to) (split-string line)))
    (list (parse-point from) (parse-point to))))

(defun parse (file)
  (mapcar #'parse-line (slurp file)))

(defun x-max (in)
  (seq-max
   (flatten-list
    (mapcar
     (lambda (line)
       (list (caar line) (caadr line)))
     in))))

(defun y-max (in)
  (seq-max
   (flatten-list
    (mapcar
     (lambda (line)
       (list (cadar line) (cadadr line)))
     in))))

(defun make-board (in)
  (let ((width (+ (x-max in) 1))
        (height (+ (y-max in) 1)))
    (make-list* height (lambda () (make-list width 0)))))

(defun horizontalp (x1 y1 x2 y2)
  (or (= x1 x2) (= y1 y2)))

(defun between (x1 y1 x2 y2)
  (cons (list x1 y1)
        (let ((next-x x1)
              (next-y y1))
          (cond ((< x1 x2) (cl-incf next-x))
                ((> x1 x2) (cl-decf next-x)))
          (cond ((< y1 y2) (cl-incf next-y))
                ((> y1 y2) (cl-decf next-y)))
          (unless (and (= x1 x2) (= y1 y2))
            (between next-x next-y x2 y2)))))

(defun fill-board (board in)
  (pcase-dolist (`((,x1 ,y1) (,x2 ,y2)) in)
    (when (horizontalp x1 y1 x2 y2)
      (pcase-dolist (`(,x ,y) (between x1 y1 x2 y2))
        (cl-incf (nth x (nth y board))))))
  board)

(defun fill-board-2 (board in)
  (pcase-dolist (`((,x1 ,y1) (,x2 ,y2)) in)
    (pcase-dolist (`(,x ,y) (between x1 y1 x2 y2))
      (cl-incf (nth x (nth y board)))))
  board)

(defun part1 (file)
  (let* ((in (parse file))
         (board (fill-board (make-board in) in)))
    (seq-count
     (lambda (x) (> x 1))
     (flatten-list board))))

(defun part2 (file)
  (let* ((in (parse file))
         (board (fill-board-2 (make-board in) in)))
    (seq-count
     (lambda (x) (> x 1))
     (flatten-list board))))
