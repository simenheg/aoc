(load-file "util.el")

;; These caves seem to be lava tubes. Parts are even still
;; volcanically active; small hydrothermal vents release smoke into
;; the caves that slowly settles like rain.

;; If you can model how the smoke flows through the caves, you might
;; be able to avoid it and be that much safer. The submarine generates
;; a heightmap of the floor of the nearby caves for you (your puzzle
;; input).

;; Smoke flows to the lowest point of the area it's in. For example,
;; consider the following heightmap:

;; 2199943210
;; 3987894921
;; 9856789892
;; 8767896789
;; 9899965678

;; Each number corresponds to the height of a particular location,
;; where 9 is the highest and 0 is the lowest a location can be.

;; Your first goal is to find the low points - the locations that are
;; lower than any of its adjacent locations. Most locations have four
;; adjacent locations (up, down, left, and right); locations on the
;; edge or corner of the map have three or two adjacent locations,
;; respectively. (Diagonal locations do not count as adjacent.)

;; In the above example, there are four low points, all highlighted:
;; two are in the first row (a 1 and a 0), one is in the third row (a
;; 5), and one is in the bottom row (also a 5). All other locations on
;; the heightmap have some lower adjacent location, and so are not low
;; points.

;; The risk level of a low point is 1 plus its height. In the above
;; example, the risk levels of the low points are 2, 1, 6, and 6. The
;; sum of the risk levels of all low points in the heightmap is
;; therefore 15.

;; Find all of the low points on your heightmap. What is the sum of
;; the risk levels of all low points on your heightmap?

(defun adjacent (matrix x y)
  (seq-filter
   #'identity
   (mapcar
    (pcase-lambda (`(,xd ,yd))
      (mref matrix (+ x xd) (+ y yd)))
    '((0 1) (0 -1) (1 0) (-1 0)))))

(defun low-point-p (matrix x y)
  (let ((val (mref matrix x y)))
    (seq-every-p
     (lambda (aval) (> aval val))
     (adjacent matrix x y))))

(defun risk-level (matrix x y)
  (if (low-point-p matrix x y)
      (+ 1 (mref matrix x y))
    0))

(defun part1 (file)
  (let ((in (slurp-matrix file)))
    (sum
     (flatten-list
      (seq-map-indexed
       (lambda (row y)
         (seq-map-indexed
          (lambda (val x)
            (risk-level in x y))
          row))
       in)))))

;; Next, you need to find the largest basins so you know what areas
;; are most important to avoid.

;; A basin is all locations that eventually flow downward to a single
;; low point. Therefore, every low point has a basin, although some
;; basins are very small. Locations of height 9 do not count as being
;; in any basin, and all other locations will always be part of
;; exactly one basin.

;; The size of a basin is the number of locations within the basin,
;; including the low point. The example above has four basins.

;; The top-left basin, size 3
;; The top-right basin, size 9
;; The middle basin, size 14
;; The bottom-right basin, size 9

;; Find the three largest basins and multiply their sizes together. In
;; the above example, this is 9 * 14 * 9 = 1134.

;; What do you get if you multiply together the sizes of the three
;; largest basins?

(defun low-points (matrix)
  (let ((low-points '()))
    (seq-do-indexed
     (lambda (row y)
       (seq-do-indexed
        (lambda (val x)
          (when (low-point-p matrix x y)
            (push (list x y) low-points)))
        row))
     in)
    low-points))

(defun adjacent-points (matrix x y)
  (mapcar
   (pcase-lambda (`(,xd ,yd)) (list (+ x xd) (+ y yd)))
   '((0 1) (0 -1) (1 0) (-1 0))))

(defun x (point) (car point))
(defun y (point) (cadr point))

(defun flood-fill (matrix point)
  (let ((queue (list point))
        (done '()))
    (while queue
      (let ((next (pop queue)))
        (push next done)
        (let ((unseen (seq-remove
                       (lambda (p)
                         (or (member p done)
                             (member p queue)
                             (not (mref matrix (x p) (y p)))
                             (= 9 (mref matrix (x p) (y p)))))
                       (adjacent-points matrix (x next) (y next)))))
          (setq queue (append queue unseen)))))
    done))

(defun part2 (file)
  (let ((in (slurp-matrix file)))
    (apply
     #'*
     (seq-take
      (sort
       (mapcar
        (lambda (point)
          (length (flood-fill in point)))
        (low-points in))
       #'>)
      3))))
