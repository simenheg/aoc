(load-file "util.el")

;; With your submarine's subterranean subsystems subsisting
;; suboptimally, the only way you're getting out of this cave anytime
;; soon is by finding a path yourself. Not just a path - the only way
;; to know if you've found the best path is to find all of them.

;; Fortunately, the sensors are still mostly working, and so you build
;; a rough map of the remaining caves (your puzzle input). For
;; example:

;; start-A
;; start-b
;; A-c
;; A-b
;; b-d
;; A-end
;; b-end

(defun parse (file)
  (let ((res (mapcar (lambda (line) (split-string line "-"))
                     (slurp file))))
    (pcase-dolist (`(,from ,to) res)
      (unless (or (equal from "start")
                  (equal to "end"))
        (push (list to from) res)))
    res))

;; This is a list of how all of the caves are connected. You start in
;; the cave named start, and your destination is the cave named
;; end. An entry like b-d means that cave b is connected to cave d -
;; that is, you can move between them.

;; So, the above cave system looks roughly like this:

;;     start
;;     /   \
;; c--A-----b--d
;;     \   /
;;      end

;; Your goal is to find the number of distinct paths that start at
;; start, end at end, and don't visit small caves more than
;; once. There are two types of caves: big caves (written in
;; uppercase, like A) and small caves (written in lowercase, like
;; b). It would be a waste of time to visit any small cave more than
;; once, but big caves are large enough that it might be worth
;; visiting them multiple times. So, all paths you find should visit
;; small caves at most once, and can visit big caves any number of
;; times.

;; Given these rules, there are 10 paths through this example cave
;; system:

;; start,A,b,A,c,A,end
;; start,A,b,A,end
;; start,A,b,end
;; start,A,c,A,b,A,end
;; start,A,c,A,b,end
;; start,A,c,A,end
;; start,A,end
;; start,b,A,c,A,end
;; start,b,A,end
;; start,b,end

(defun assoc-all (key alist)
  (mapcar
   #'cadr
   (seq-filter (pcase-lambda (`(,k . ,_)) (equal k key)) alist)))

(defun string-lower-p (str)
  (equal str (downcase str)))

(defun filter-queue-1 (queue hist)
  (seq-remove
   (lambda (node)
     (when (string-lower-p node)
       (member node hist)))
   queue))

(defun walk (graph cur filter-fun &optional trace)
  (push cur trace)
  (if (equal cur "end")
      (list (reverse trace))
    (let ((queue (funcall filter-fun (assoc-all cur graph) trace)))
      (cl-loop for next in queue
               append (walk graph next filter-fun trace)))))

(defun part1 (file)
  (length (walk (parse file) "start" #'filter-queue-1)))

;; After reviewing the available paths, you realize you might have
;; time to visit a single small cave twice. Specifically, big caves
;; can be visited any number of times, a single small cave can be
;; visited at most twice, and the remaining small caves can be visited
;; at most once. However, the caves named start and end can only 0be
;; visited exactly once each: once you leave the start cave, you may
;; not return to it, and once you reach the end cave, the path must
;; end immediately.

(defun small-visited-twice-already (hist)
  (let ((lowers (seq-filter #'string-lower-p hist)))
    (/= (length lowers) (length (delete-dups lowers)))))

(defun filter-queue-2 (queue hist)
  (seq-remove
   (lambda (node)
     (or (equal node "start")
         (when (and (string-lower-p node)
                    (small-visited-twice-already hist))
           (member node hist))))
   queue))

(defun part2 (file)
  (length (walk (parse file) "start" #'filter-queue-2)))
