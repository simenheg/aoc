(load-file "util.el")

;; You ask the device for a heightmap of the surrounding area (your
;; puzzle input). The heightmap shows the local area from above broken
;; into a grid; the elevation of each square of the grid is given by a
;; single lowercase letter, where a is the lowest elevation, b is the
;; next-lowest, and so on up to the highest elevation, z.

;; Also included on the heightmap are marks for your current position
;; (S) and the location that should get the best signal (E). Your
;; current position (S) has elevation a, and the location that should
;; get the best signal (E) has elevation z.

;; You'd like to reach E, but to save energy, you should do it in as
;; few steps as possible. During each step, you can move exactly one
;; square up, down, left, or right. To avoid needing to get out your
;; climbing gear, the elevation of the destination square can be at
;; most one higher than the elevation of your current square; that is,
;; if your current elevation is m, you could step to elevation n, but
;; not to elevation o. (This also means that the elevation of the
;; destination square can be much lower than the elevation of your
;; current square.)

;; For example:

;; Sabqponm
;; abcryxxl
;; accszExk
;; acctuvwj
;; abdefghi

;; Here, you start in the top-left corner; your goal is near the
;; middle. You could start by moving down or right, but eventually
;; you'll need to head toward the e at the bottom. From there, you can
;; spiral around to the goal:

;; v..v<<<<
;; >v.vv<<^
;; .>vv>E^^
;; ..v>>>^^
;; ..>>>>>^

;; In the above diagram, the symbols indicate whether the path exits
;; each square moving up (^), down (v), left (<), or right (>). The
;; location that should get the best signal is still E, and . marks
;; unvisited squares.

;; This path reaches the goal in 31 steps, the fewest possible.

;; What is the fewest steps required to move from your current
;; position to the location that should get the best signal?

(defun x (coord) (car coord))
(defun y (coord) (cadr coord))

(defun find (char G)
  (let* ((y (seq-position G char (lambda (e elt) (member elt e))))
         (x (seq-position (elt G y) char)))
    (list x y)))

(defun neighbors (coord width height)
  (pcase-let ((`(,x ,y) coord))
    (seq-filter
     (pcase-lambda (`(,x ,y))
       (and (<= 0 x) (<= 0 y) (< x width) (< y height)))
     `((,x ,(+ y 1)) (,x ,(- y 1)) (,(+ x 1) ,y) (,(- x 1) ,y)))))

(defun reachablep (G from to)
  (let ((from-char (mref G (x from) (y from)))
        (to-char (mref G (x to) (y to))))
    (when (= from-char ?S)
      (setq from-char ?a))
    (when (= to-char ?E)
      (setq to-char ?z))
    (<= -1 (- from-char to-char))))

(defun solve (G)
  "A.K.A. Dijkstra."
  (let* ((width (length (car G)))
         (height (length G))
         (dist (make-list*
                height (lambda () (make-list width (* width height)))))
         (S (find ?S G))
         (E (find ?E G))
         (Q (list S))
         (visited '()))
    (mset dist (x S) (y S) 0)
    (while-let ((cur (pop Q)))
      (let ((candidate (+ 1 (mref dist (x cur) (y cur)))))
        (dolist (next (neighbors cur width height))
          (when (reachablep G cur next)
            (when (> (mref dist (x next) (y next)) candidate)
              (mset dist (x next) (y next) candidate))
            (unless (member next visited)
              (push next visited)
              (push next Q)))))
      (setq Q (seq-sort-by (lambda (c) (mref dist (x c) (y c))) #'< Q)))
    (mref dist (x E) (y E))))

(defun part1 (file)
  (solve (slurp-char-grid file)))

;; As you walk up the hill, you suspect that the Elves will want to
;; turn this into a hiking trail. The beginning isn't very scenic,
;; though; perhaps you can find a better starting point.

;; To maximize exercise while hiking, the trail should start as low as
;; possible: elevation a. The goal is still the square marked
;; E. However, the trail should still be direct, taking the fewest
;; steps to reach its goal. So, you'll need to find the shortest path
;; from any square at elevation a to the square marked E.

;; Again consider the example from above:

;; Sabqponm
;; abcryxxl
;; accszExk
;; acctuvwj
;; abdefghi

;; Now, there are six choices for starting position (five marked a,
;; plus the square marked S that counts as being at elevation a). If
;; you start at the bottom-left square, you can reach the goal most
;; quickly:

;; ...v<<<<
;; ...vv<<^
;; ...v>E^^
;; .>v>>>^^
;; >^>>>>>^

;; This path reaches the goal in only 29 steps, the fewest possible.

;; What is the fewest steps required to move starting from any square
;; with elevation a to the location that should get the best signal?

(defun solve2 (G)
  (let* ((width (length (car G)))
         (height (length G))
         (dist (make-list*
                height (lambda () (make-list width (* width height)))))
         (E (find ?E G))
         (Q (list E))
         (visited '())
         (best (* width height)))
    (mset dist (x E) (y E) 0)
    (while-let ((cur (pop Q)))
      (let ((candidate (+ 1 (mref dist (x cur) (y cur)))))
        (dolist (next (neighbors cur width height))
          (when (reachablep G next cur)
            (when (> (mref dist (x next) (y next)) candidate)
              (mset dist (x next) (y next) candidate)
              (when (and (member (mref G (x next) (y next))
                                 '(?a ?S))
                         (> best candidate))
                (setq best candidate)))
            (unless (member next visited)
              (push next visited)
              (push next Q)))))
      (setq Q (seq-sort-by (lambda (c) (mref dist (x c) (y c))) #'< Q)))
    best))

(defun part2 (file)
  (solve2 (slurp-char-grid file)))
