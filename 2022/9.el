(load-file "util.el")

;; Consider a rope with a knot at each end; these knots mark the head
;; and the tail of the rope. If the head moves far enough away from
;; the tail, the tail is pulled toward the head.

;; Due to nebulous reasoning involving Planck lengths, you should be
;; able to model the positions of the knots on a two-dimensional
;; grid. Then, by following a hypothetical series of motions (your
;; puzzle input) for the head, you can determine how the tail will
;; move.

;; Due to the aforementioned Planck lengths, the rope must be quite
;; short; in fact, the head (H) and tail (T) must always be touching
;; (diagonally adjacent and even overlapping both count as touching):

;; ....
;; .TH.
;; ....

;; ....
;; .H..
;; ..T.
;; ....

;; ...
;; .H. (H covers T)
;; ...

;; If the head is ever two steps directly up, down, left, or right
;; from the tail, the tail must also move one step in that direction
;; so it remains close enough:

;; .....    .....    .....
;; .TH.. -> .T.H. -> ..TH.
;; .....    .....    .....

;; ...    ...    ...
;; .T.    .T.    ...
;; .H. -> ... -> .T.
;; ...    .H.    .H.
;; ...    ...    ...

;; Otherwise, if the head and tail aren't touching and aren't in the
;; same row or column, the tail always moves one step diagonally to
;; keep up:

;; .....    .....    .....
;; .....    ..H..    ..H..
;; ..H.. -> ..... -> ..T..
;; .T...    .T...    .....
;; .....    .....    .....

;; .....    .....    .....
;; .....    .....    .....
;; ..H.. -> ...H. -> ..TH.
;; .T...    .T...    .....
;; .....    .....    .....

;; You just need to work out where the tail goes as the head follows a
;; series of motions. Assume the head and the tail both start at the
;; same position, overlapping.

;; For example:

;; R 4
;; U 4
;; L 3
;; D 1
;; R 4
;; D 1
;; L 5
;; R 2

;; After simulating the rope, you can count up all of the positions
;; the tail visited at least once. In this diagram, s again marks the
;; starting position (which the tail also visited) and # marks other
;; positions the tail visited:

;; ..##..
;; ...##.
;; .####.
;; ....#.
;; s###..

;; So, there are 13 positions the tail visited at least once.

;; Simulate your complete hypothetical series of motions. How many
;; positions does the tail of the rope visit at least once?

(defun dir (dir)
  (pcase dir
    ("U" '(0 1)) ("R" '(1 0)) ("D" '(0 -1)) ("L" '(-1 0))))

(defun part1 (file)
  (let ((H '(0 0))
        (T '(0 0))
        (hist '((0 0))))
    (dolist (line (slurp file))
      (pcase-let ((`(,dir ,n) (split-string line)))
        (dotimes (_ (string-to-number n))
          (setq H (seq-mapn #'+ H (dir dir)))
          (cond
           ((= 2 (- (x H) (x T)))
            (setq T `(,(- (x H) 1) ,(y H))))
           ((= -2 (- (x H) (x T)))
            (setq T `(,(+ (x H) 1) ,(y H))))
           ((= 2 (- (y H) (y T)))
            (setq T `(,(x H) ,(- (y H) 1))))
           ((= -2 (- (y H) (y T)))
            (setq T `(,(x H) ,(+ (y H) 1)))))
          (cl-pushnew T hist :test #'equal))))
    (length hist)))

;; Rather than two knots, you now must simulate a rope consisting of
;; ten knots. One knot is still the head of the rope and moves
;; according to the series of motions. Each knot further down the rope
;; follows the knot in front of it using the same rules as before.

;; Using the same series of motions as the above example, but with the
;; knots marked H, 1, 2, ..., 9.

;; Now, you need to keep track of the positions the new tail, 9,
;; visits. In this example, the tail never moves, and so it only
;; visits 1 position. However, be careful: more types of motion are
;; possible than before, so you might want to visually compare your
;; simulated rope to the one above.

;; Simulate your complete series of motions on a larger rope with ten
;; knots. How many positions does the tail of the rope visit at least
;; once?

(defun move (H T)
  (let* ((xd (- (x H) (x T)))
         (yd (- (y H) (y T)))
         (x* (cond ((= xd 2) -1) ((= xd -2) 1) (t 0)))
         (y* (cond ((= yd 2) -1) ((= yd -2) 1) (t 0))))
    (if (and (= x* 0) (= y* 0))
        T
      `(,(+ (x H) x*) ,(+ (y H) y*)))))

(defun part2 (file)
  (let ((rope (make-list* 10 (lambda () (list 0 0))))
        (hist '((0 0))))
    (dolist (line (slurp file))
      (pcase-let ((`(,dir ,n) (split-string line)))
        (dotimes (_ (string-to-number n))
          (setf (car rope) (seq-mapn #'+ (car rope) (dir dir)))
          (dotimes (i (- (length rope) 1))
            (setf (elt rope (+ i 1))
                  (move (elt rope i) (elt rope (+ i 1)))))
          (cl-pushnew (car (last rope)) hist :test #'equal))))
    (length hist)))
