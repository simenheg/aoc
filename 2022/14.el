(load-file "util.el")

;; Your scan traces the path of each solid rock structure and reports
;; the x,y coordinates that form the shape of the path, where x
;; represents distance to the right and y represents distance
;; down. Each path appears as a single line of text in your
;; scan. After the first point of each path, each point indicates the
;; end of a straight horizontal or vertical line to be drawn from the
;; previous point. For example:

;; 498,4 -> 498,6 -> 496,6
;; 503,4 -> 502,4 -> 502,9 -> 494,9

;; This scan means that there are two paths of rock; the first path
;; consists of two straight lines, and the second path consists of
;; three straight lines. (Specifically, the first path consists of a
;; line of rock from 498,4 through 498,6 and another line of rock from
;; 498,6 through 496,6.)

;; The sand is pouring into the cave from point 500,0.

;; Using your scan, simulate the falling sand. How many units of sand
;; come to rest before sand starts flowing into the abyss below?

(defun line (from to)
  (let ((next
         (cond
          ((< (x from) (x to)) `(,(+ (x from) 1) ,(y from)))
          ((> (x from) (x to)) `(,(- (x from) 1) ,(y from)))
          ((< (y from) (y to)) `(,(x from) ,(+ (y from) 1)))
          ((> (y from) (y to)) `(,(x from) ,(- (y from) 1))))))
    (if next
        (cons from (line next to))
      (list to))))

(defun parse-point (point)
  (mapcar #'string-to-number (split-string point ",")))

(defun parse-line (line)
  (mapcar #'parse-point (split-string line " -> ")))

(defun fill-rocks (world lines)
  (cl-maplist
   (pcase-lambda (`(,from ,to))
     (when to
       (dolist (point (line from to))
         (puthash point 'R world))))
   lines))

(defun simulate-sand (world sand i)
  (cond
   ((> i 500) nil) ; ~∞
   ((not (gethash `(,(x sand) ,(+ (y sand) 1)) world))
    (simulate-sand world `(,(x sand) ,(+ (y sand) 1)) (+ i 1)))
   ((not (gethash `(,(- (x sand) 1) ,(+ (y sand) 1)) world))
    (simulate-sand world `(,(- (x sand) 1) ,(+ (y sand) 1)) (+ i 1)))
   ((not (gethash `(,(+ (x sand) 1) ,(+ (y sand) 1)) world))
    (simulate-sand world `(,(+ (x sand) 1) ,(+ (y sand) 1)) (+ i 1)))
   (t (puthash sand 'S world))))

(defun simulate-world (world)
  (let ((i 0))
    (while (simulate-sand world '(500 0) 0)
      (cl-incf i))
    i))

(defun part1 (file)
  (let ((world (make-hash-table :test #'equal)))
    (dolist (line (slurp file))
      (fill-rocks world (parse-line line)))
    (simulate-world world)))

;; You realize you misread the scan. There isn't an endless void at
;; the bottom of the scan - there's floor, and you're standing on it!

;; You don't have time to scan the floor, so assume the floor is an
;; infinite horizontal line with a y coordinate equal to two plus the
;; highest y coordinate of any point in your scan.

;; To find somewhere safe to stand, you'll need to simulate falling
;; sand until a unit of sand comes to rest at 500,0, blocking the
;; source entirely and stopping the flow of sand into the cave.

;; Using your scan, simulate the falling sand until the source of the
;; sand becomes blocked. How many units of sand come to rest?

(defun simulate-world-2 (world)
  (let ((i 0))
    (while (not (gethash '(500 0) world))
      (simulate-sand world '(500 0) 0)
      (cl-incf i))
    i))

(defun part2 (file)
  (let ((world (make-hash-table :test #'equal)))
    (dolist (line (slurp file))
      (fill-rocks world (parse-line line)))
    (let ((max-y (apply #'max (mapcar #'y (hash-table-keys world)))))
      (dotimes (i 1000) ; ~∞
        (puthash `(,i ,(+ 2 max-y)) 'R world)))
    (simulate-world-2 world)))
