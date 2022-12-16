(load-file "util.el")

;; Sensors and beacons always exist at integer coordinates. Each
;; sensor knows its own position and can determine the position of a
;; beacon precisely; however, sensors can only lock on to the one
;; beacon closest to the sensor as measured by the Manhattan
;; distance. (There is never a tie where two beacons are the same
;; distance to a sensor.)

;; It doesn't take long for the sensors to report back their positions
;; and closest beacons (your puzzle input). For example:

;; Sensor at x=2, y=18: closest beacon is at x=-2, y=15
;; Sensor at x=9, y=16: closest beacon is at x=10, y=16
;; Sensor at x=13, y=2: closest beacon is at x=15, y=3
;; Sensor at x=12, y=14: closest beacon is at x=10, y=16
;; Sensor at x=10, y=20: closest beacon is at x=10, y=16
;; Sensor at x=14, y=17: closest beacon is at x=10, y=16
;; Sensor at x=8, y=7: closest beacon is at x=2, y=10
;; Sensor at x=2, y=0: closest beacon is at x=2, y=10
;; Sensor at x=0, y=11: closest beacon is at x=2, y=10
;; Sensor at x=20, y=14: closest beacon is at x=25, y=17
;; Sensor at x=17, y=20: closest beacon is at x=21, y=22
;; Sensor at x=16, y=7: closest beacon is at x=15, y=3
;; Sensor at x=14, y=3: closest beacon is at x=15, y=3
;; Sensor at x=20, y=1: closest beacon is at x=15, y=3

;; So, consider the sensor at 2,18; the closest beacon to it is at
;; -2,15. For the sensor at 9,16, the closest beacon to it is at
;; 10,16.

;; Drawing sensors as S and beacons as B, the above arrangement of
;; sensors and beacons looks like this:

;;                1    1    2    2
;;      0    5    0    5    0    5
;;  0 ....S.......................
;;  1 ......................S.....
;;  2 ...............S............
;;  3 ................SB..........
;;  4 ............................
;;  5 ............................
;;  6 ............................
;;  7 ..........S.......S.........
;;  8 ............................
;;  9 ............................
;; 10 ....B.......................
;; 11 ..S.........................
;; 12 ............................
;; 13 ............................
;; 14 ..............S.......S.....
;; 15 B...........................
;; 16 ...........SB...............
;; 17 ................S..........B
;; 18 ....S.......................
;; 19 ............................
;; 20 ............S......S........
;; 21 ............................
;; 22 .......................B....

;; This isn't necessarily a comprehensive map of all beacons in the
;; area, though. Because each sensor only identifies its closest
;; beacon, if a sensor detects a beacon, you know there are no other
;; beacons that close or closer to that sensor. There could still be
;; beacons that just happen to not be the closest beacon to any
;; sensor. Consider the sensor at 8,7:

;;                1    1    2    2
;;      0    5    0    5    0    5
;; -2 ..........#.................
;; -1 .........###................
;;  0 ....S...#####...............
;;  1 .......#######........S.....
;;  2 ......#########S............
;;  3 .....###########SB..........
;;  4 ....#############...........
;;  5 ...###############..........
;;  6 ..#################.........
;;  7 .#########S#######S#........
;;  8 ..#################.........
;;  9 ...###############..........
;; 10 ....B############...........
;; 11 ..S..###########............
;; 12 ......#########.............
;; 13 .......#######..............
;; 14 ........#####.S.......S.....
;; 15 B........###................
;; 16 ..........#SB...............
;; 17 ................S..........B
;; 18 ....S.......................
;; 19 ............................
;; 20 ............S......S........
;; 21 ............................
;; 22 .......................B....

;; This sensor's closest beacon is at 2,10, and so you know there are
;; no beacons that close or closer (in any positions marked #).

;; None of the detected beacons seem to be producing the distress
;; signal, so you'll need to work out where the distress beacon is by
;; working out where it isn't. For now, keep things simple by counting
;; the positions where a beacon cannot possibly be along just a single
;; row.

;; So, suppose you have an arrangement of beacons and sensors like in
;; the example above and, just in the row where y=10, you'd like to
;; count the number of positions a beacon cannot possibly exist. The
;; coverage from all sensors near that row looks like this:

;;                  1    1    2    2
;;        0    5    0    5    0    5
;;  9 ...#########################...
;; 10 ..####B######################..
;; 11 .###S#############.###########.

;; In this example, in the row where y=10, there are 26 positions
;; where a beacon cannot be present.

;; Consult the report from the sensors you just deployed. In the row
;; where y=2000000, how many positions cannot contain a beacon?

(cl-defstruct sensor x y dist)

(defun dist (p1 p2)
  (+ (abs (- (x p2) (x p1)))
     (abs (- (y p2) (y p1)))))

(defun parse-coord (c)
  (string-to-number (seq-drop c 2)))

(defun parse (file)
  (let ((sensors '())
        (beacons (make-hash-table :test #'equal)))
    (pcase-dolist (`(_ _ ,sx ,sy _ _ _ _ ,bx ,by)
                   (mapcar #'split-string (slurp file)))
      (push
       (make-sensor
        :x (parse-coord sx)
        :y (parse-coord sy)
        :dist (dist `(,(parse-coord sx) ,(parse-coord sy))
                    `(,(parse-coord bx) ,(parse-coord by))))
       sensors)
      (puthash `(,(parse-coord bx) ,(parse-coord by)) t beacons))
    `(,sensors ,beacons)))

(defun within-reach-p (point sensor)
  (>= (sensor-dist sensor)
     (dist point `(,(sensor-x sensor) ,(sensor-y sensor)))))

(defun part1 (file y)
  (pcase-let ((`(,sensors ,beacons) (parse file)))
    (let ((count 0))
      (cl-loop for x from -10000000 to 10000000 do ; 🥲
        (when (seq-some (lambda (s) (within-reach-p `(,x ,y) s)) sensors)
          (unless (gethash `(,x ,y) beacons)
            (cl-incf count))))
      count)))

;; Your handheld device indicates that the distress signal is coming
;; from a beacon nearby. The distress beacon is not detected by any
;; sensor, but the distress beacon must have x and y coordinates each
;; no lower than 0 and no larger than 4000000.

;; To isolate the distress beacon's signal, you need to determine its
;; tuning frequency, which can be found by multiplying its x
;; coordinate by 4000000 and then adding its y coordinate.

;; In the example above, the search space is smaller: instead, the x
;; and y coordinates can each be at most 20. With this reduced search
;; area, there is only a single position that could have a beacon:
;; x=14, y=11. The tuning frequency for this distress beacon is
;; 56000011.

;; Find the only possible position for the distress beacon. What is
;; its tuning frequency?

(defun sensor-border (s lim)
  (let ((x (sensor-x s))
        (y (sensor-y s))
        (dist (sensor-dist s))
        (res '()))
    (catch 'break
      (dotimes (i (+ dist 1))
        (let ((px (+ x i))
              (py (+ (- y dist) i -1)))
          (when (or (< px 0) (< py 0) (> px lim) (> py lim))
            (throw 'break t))
          (push `(,px ,py) res))))
    (catch 'break
      (dotimes (i dist)
        (let ((px (- (+ x dist 1) i))
              (py (+ y i)))
          (when (or (< px 0) (< py 0) (> px lim) (> py lim))
            (throw 'break t))
          (push `(,px ,py) res))))
    (catch 'break
      (dotimes (i (+ dist 1))
        (let ((px (- x i 1))
              (py (- (+ y dist) i)))
          (when (or (< px 0) (< py 0) (> px lim) (> py lim))
            (throw 'break t))
          (push `(,px ,py) res))))
    (catch 'break
      (dotimes (i dist)
        (let ((px (+ (- x dist) i))
              (py (- y 1 i)))
          (when (or (< px 0) (< py 0) (> px lim) (> py lim))
            (throw 'break t))
          (push `(,px ,py) res))))
    res))

(defun part2 (file lim)
  (pcase-let ((`(,sensors ,beacons) (parse file)))
    (let ((p (catch 'done
               (dolist (sensor sensors)
                 (dolist (p (sensor-border sensor lim))
                   (when (not (seq-some (lambda (s) (within-reach-p `(,(x p) ,(y p)) s)) sensors))
                     (unless (gethash `(,(x p) ,(y p)) beacons)
                       (throw 'done p))))))))
      (+ (* (x p) 4000000) (y p)))))
