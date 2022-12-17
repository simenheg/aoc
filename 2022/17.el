(load-file "util.el")

;; Your handheld device has located an alternative exit from the cave
;; for you and the elephants. The ground is rumbling almost
;; continuously now, but the strange valves bought you some time. It's
;; definitely getting warmer in here, though.

;; The tunnels eventually open into a very tall, narrow
;; chamber. Large, oddly-shaped rocks are falling into the chamber
;; from above, presumably due to all the rumbling. If you can't work
;; out where the rocks will fall next, you might be crushed!

;; The five types of rocks have the following peculiar shapes, where #
;; is rock and . is empty space:

;; ####

;; .#.
;; ###
;; .#.

;; ..#
;; ..#
;; ###

;; #
;; #
;; #
;; #

;; ##
;; ##

;; The rocks fall in the order shown above: first the - shape, then
;; the + shape, and so on. Once the end of the list is reached, the
;; same order repeats: the - shape falls first, sixth, 11th, 16th,
;; etc.

;; The rocks don't spin, but they do get pushed around by jets of hot
;; gas coming out of the walls themselves. A quick scan reveals the
;; effect the jets of hot gas will have on the rocks as they fall
;; (your puzzle input).

;; For example, suppose this was the jet pattern in your cave:

;; >>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>

;; In jet patterns, < means a push to the left, while > means a push
;; to the right. The pattern above means that the jets will push a
;; falling rock right, then right, then right, then left, then left,
;; then right, and so on. If the end of the list is reached, it
;; repeats.

;; The tall, vertical chamber is exactly seven units wide. Each rock
;; appears so that its left edge is two units away from the left wall
;; and its bottom edge is three units above the highest rock in the
;; room (or the floor, if there isn't one).

;; After a rock appears, it alternates between being pushed by a jet
;; of hot gas one unit (in the direction indicated by the next symbol
;; in the jet pattern) and then falling one unit down. If any movement
;; would cause any part of the rock to move into the walls, floor, or
;; a stopped rock, the movement instead does not occur. If a downward
;; movement would have caused a falling rock to move into the floor or
;; an already-fallen rock, the falling rock stops where it is (having
;; landed on something) and a new rock immediately begins falling.

;; To prove to the elephants your simulation is accurate, they want to
;; know how tall the tower will get after 2022 rocks have stopped (but
;; before the 2023rd rock begins falling). In this example, the tower
;; of rocks will be 3068 units tall.

;; How many units tall will the tower of rocks be after 2022 rocks
;; have stopped falling?

(defun spawn-rock (pat y-max)
  (mapcar
   (pcase-lambda (`(,x ,y))
     `(,(+ 2 x) ,(+ y-max y 3)))
   pat))

(defun bumps (p board)
  (or (< (x p) 0) (>= (x p) 7) (< (y p) 0)
      (gethash p board)))

(defun push-rock (rock jet board)
  (let ((new (mapcar (lambda (p) (seq-mapn #'+ p jet)) rock)))
    (if (seq-some (lambda (p) (bumps p board)) new) rock new)))

(defun fall-rock (rock board)
  (let ((new (mapcar (lambda (p) (seq-mapn #'- p '(0 1))) rock)))
    (if (seq-some (lambda (p) (bumps p board)) new) rock new)))

(defun y-max (board)
  (+ 1 (apply #'max (or (mapcar #'y (hash-table-keys board)) '(-1)))))

(defun part1 (file)
  (let ((board (make-hash-table :test #'equal))
        (jets (make-circular
               (seq-map
                (lambda (c) (cl-case c (?> '(1 0)) (?< '(-1 0))))
                (slurp-raw file))))
        (rock-patterns
         (seq-copy
          '(((0 0) (1 0) (2 0) (3 0))       ; ̣–
            ((0 1) (1 0) (1 1) (1 2) (2 1)) ; +
            ((0 0) (1 0) (2 0) (2 1) (2 2)) ; ⅃
            ((0 0) (0 1) (0 2) (0 3))       ; |
            ((0 0) (1 0) (0 1) (1 1))))))   ; ◼
    (dolist (pat (take 2022 (make-circular rock-patterns)))
      (let ((rock (spawn-rock pat (y-max board)))
            (prev nil))
        (while (not (equal rock prev))
          (setq rock (push-rock rock (pop jets) board))
          (setq prev rock)
          (setq rock (fall-rock rock board)))
        (dolist (p rock)
          (puthash p t board))))
    (y-max board)))

;; The elephants are not impressed by your simulation. They demand to
;; know how tall the tower will be after 1000000000000 rocks have
;; stopped! Only then will they feel confident enough to proceed
;; through the cave.

;; In the example above, the tower would be 1514285714288 units tall!

;; How tall will the tower be after 1000000000000 rocks have stopped?

(defun board-floor (board)
  (let ((y-max (y-max board)))
    (mapcar
     #'x
     (seq-filter
      (pcase-lambda (`(,x ,y))
        (= y (- y-max 1)))
      (hash-table-keys board)))))

(defun part2 (file n-rocks)
  (let ((hist (make-hash-table :test #'equal))
        (board (make-hash-table :test #'equal))
        (jets (seq-map
               (lambda (c) (cl-case c (?> '(1 0)) (?< '(-1 0))))
               (slurp-raw file)))
        (jet-i 0)
        (i 0)
        (bonus-height 0)
        (rock-patterns
         (seq-copy
          '(((0 0) (1 0) (2 0) (3 0))       ; ̣–
            ((0 1) (1 0) (1 1) (1 2) (2 1)) ; +
            ((0 0) (1 0) (2 0) (2 1) (2 2)) ; ⅃
            ((0 0) (0 1) (0 2) (0 3))       ; |
            ((0 0) (1 0) (0 1) (1 1))))))   ; ◼
    (while (> n-rocks i)
      (let* ((rock-n (mod i (length rock-patterns)))
             (pat (elt rock-patterns rock-n))
             (rock (spawn-rock pat (y-max board)))
             (prev nil))
        (while (not (equal rock prev))
          (setq rock (push-rock rock (elt jets (mod jet-i (length jets))) board))
          (cl-incf jet-i)
          (setq prev rock)
          (setq rock (fall-rock rock board)))
        (let ((key `(,(board-floor board) ,rock-n ,(mod jet-i (length jets)))))
          (when (gethash key hist)
            ;; We've seen this before, speed up the simulation a bit
            (pcase-let ((`(,prev-i ,prev-height) (gethash key hist)))
              (let* ((height-diff (- (y-max board) prev-height))
                     (i-diff (- i prev-i))
                     (bonus-i (/ (- n-rocks i) i-diff)))
                (cl-incf i (* bonus-i i-diff))
                (cl-incf bonus-height (* bonus-i height-diff)))))
          (puthash key `(,i ,(y-max board)) hist))
        (cl-incf i)
        (dolist (p rock)
          (puthash p t board))))
    (+ bonus-height (y-max board))))
