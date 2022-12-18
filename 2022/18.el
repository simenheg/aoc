(load-file "util.el")

;; You and the elephants finally reach fresh air. You've emerged near
;; the base of a large volcano that seems to be actively erupting!
;; Fortunately, the lava seems to be flowing away from you and toward
;; the ocean.

;; Bits of lava are still being ejected toward you, so you're
;; sheltering in the cavern exit a little longer. Outside the cave,
;; you can see the lava landing in a pond and hear it loudly hissing
;; as it solidifies.

;; Depending on the specific compounds in the lava and speed at which
;; it cools, it might be forming obsidian! The cooling rate should be
;; based on the surface area of the lava droplets, so you take a quick
;; scan of a droplet as it flies past you (your puzzle input).

;; Because of how quickly the lava is moving, the scan isn't very
;; good; its resolution is quite low and, as a result, it approximates
;; the shape of the lava droplet with 1x1x1 cubes on a 3D grid, each
;; given as its x,y,z position.

;; To approximate the surface area, count the number of sides of each
;; cube that are not immediately connected to another cube. So, if
;; your scan were only two adjacent cubes like 1,1,1 and 2,1,1, each
;; cube would have a single side covered and five sides exposed, a
;; total surface area of 10 sides.

;; Here's a larger example:

;; 2,2,2
;; 1,2,2
;; 3,2,2
;; 2,1,2
;; 2,3,2
;; 2,2,1
;; 2,2,3
;; 2,2,4
;; 2,2,6
;; 1,2,5
;; 3,2,5
;; 2,1,5
;; 2,3,5

;; In the above example, after counting up all the sides that aren't
;; connected to another cube, the total surface area is 64.

;; What is the surface area of your scanned lava droplet?

(defun dist (p1 p2)
  (+ (abs (- (x p2) (x p1)))
     (abs (- (y p2) (y p1)))
     (abs (- (z p2) (z p1)))))

(defun parse (file)
  (mapcar
   (lambda (line)
     (mapcar #'string-to-number (string-split line ",")))
   (slurp file)))

(defun sides (points)
  (let ((sides (* (length points) 6))
        (hist '()))
    (dolist (p1 points)
      (cl-decf
       sides
       (* 2 (seq-count (lambda (p2) (= 1 (dist p1 p2))) hist)))
      (push p1 hist))
    sides))

(defun part1 (file)
  (sides (parse file)))

;; Something seems off about your calculation. The cooling rate
;; depends on exterior surface area, but your calculation also
;; included the surface area of air pockets trapped in the lava
;; droplet.

;; Instead, consider only cube sides that could be reached by the
;; water and steam as the lava droplet tumbles into the pond. The
;; steam will expand to reach as much as possible, completely
;; displacing any air on the outside of the lava droplet but never
;; expanding diagonally.

;; In the larger example above, exactly one cube of air is trapped
;; within the lava droplet (at 2,2,5), so the exterior surface area of
;; the lava droplet is 58.

;; What is the exterior surface area of your scanned lava droplet?

(defun neighbors (p x-min y-min z-min x-max y-max z-max)
  (pcase-let ((`(,x ,y ,z) p))
    (seq-filter
     (pcase-lambda (`(,x ,y ,z))
       (and (<= x-min x) (<= y-min y) (<= z-min z)
            (<= x x-max) (<= y y-max) (<= z z-max)))
     `((,(+ x 1) ,y ,z) (,(- x 1) ,y ,z)
       (,x ,(+ y 1) ,z) (,x ,(- y 1) ,z)
       (,x ,y ,(+ z 1)) (,x ,y ,(- z 1))))))

(defun part2 (file)
  (let* ((points (parse file))
         (x-min (1- (apply #'min (mapcar #'x points))))
         (y-min (1- (apply #'min (mapcar #'y points))))
         (z-min (1- (apply #'min (mapcar #'z points))))
         (x-max (1+ (apply #'max (mapcar #'x points))))
         (y-max (1+ (apply #'max (mapcar #'y points))))
         (z-max (1+ (apply #'max (mapcar #'z points))))
         (Q `((,x-min ,y-min ,z-min)))
         (hist (make-hash-table :test #'equal))
         (sides 0))
    (while-let ((p (pop Q)))
      (dolist (n (neighbors p x-min y-min z-min x-max y-max z-max))
        (unless (gethash n hist)
          (if (member n points)
              (cl-incf sides)
            (push n Q)
            (puthash n t hist)))))
    sides))
