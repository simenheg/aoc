(load-file "util.el")

;; The sensors have led you to the origin of the distress signal: yet
;; another handheld device, just like the one the Elves gave
;; you. However, you don't see any Elves around; instead, the device
;; is surrounded by elephants! They must have gotten lost in these
;; tunnels, and one of the elephants apparently figured out how to
;; turn on the distress signal.

;; The ground rumbles again, much stronger this time. What kind of
;; cave is this, exactly? You scan the cave with your handheld device;
;; it reports mostly igneous rock, some ash, pockets of pressurized
;; gas, magma... this isn't just a cave, it's a volcano!

;; You need to get the elephants out of here, quickly. Your device
;; estimates that you have 30 minutes before the volcano erupts, so
;; you don't have time to go back out the way you came in.

;; You scan the cave for other options and discover a network of pipes
;; and pressure-release valves. You aren't sure how such a system got
;; into a volcano, but you don't have time to complain; your device
;; produces a report (your puzzle input) of each valve's flow rate if
;; it were opened (in pressure per minute) and the tunnels you could
;; use to move between the valves.

;; There's even a valve in the room you and the elephants are
;; currently standing in labeled AA. You estimate it will take you one
;; minute to open a single valve and one minute to follow any tunnel
;; from one valve to another. What is the most pressure you could
;; release?

;; For example, suppose you had the following scan output:

;; Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
;; Valve BB has flow rate=13; tunnels lead to valves CC, AA
;; Valve CC has flow rate=2; tunnels lead to valves DD, BB
;; Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
;; Valve EE has flow rate=3; tunnels lead to valves FF, DD
;; Valve FF has flow rate=0; tunnels lead to valves EE, GG
;; Valve GG has flow rate=0; tunnels lead to valves FF, HH
;; Valve HH has flow rate=22; tunnel leads to valve GG
;; Valve II has flow rate=0; tunnels lead to valves AA, JJ
;; Valve JJ has flow rate=21; tunnel leads to valve II

;; All of the valves begin closed. You start at valve AA, but it must
;; be damaged or jammed or something: its flow rate is 0, so there's
;; no point in opening it. However, you could spend one minute moving
;; to valve BB and another minute opening it; doing so would release
;; pressure during the remaining 28 minutes at a flow rate of 13, a
;; total eventual pressure release of 28 * 13 = 364. Then, you could
;; spend your third minute moving to valve CC and your fourth minute
;; opening it, providing an additional 26 minutes of eventual pressure
;; release at a flow rate of 2, or 52 total pressure released by valve
;; CC.

;; Making your way through the tunnels like this, you could probably
;; open many or all of the valves by the time 30 minutes have
;; elapsed. However, you need to release as much pressure as possible,
;; so you'll need to be methodical.

;; Work out the steps to release the most pressure in 30 minutes. What
;; is the most pressure you can release?

(defun parse-line (line)
  (pcase-let ((`(_ ,id _ _ ,rate _ _ _ _ . ,to) (split-string line)))
    `(,id
      ,(string-to-number (seq-drop rate 5))
      ,(mapcar (lambda (i) (string-remove-suffix "," i)) to))))

(defun part1 (file)
  (let ((valves (make-hash-table :test #'equal))
        (candidates '()))
    (pcase-dolist (`(,id ,rate ,to) (mapcar #'parse-line (slurp file)))
      (puthash id `(,rate ,to) valves)
      (when (> rate 0) (push id candidates)))
    (let* ((Q `(("AA" 0 30 ,candidates)))
           (seen (make-hash-table :test #'equal))
           (best 0))
      (while-let ((next (pop Q)))
        (pcase-let ((`(,id ,rate ,time ,cand) next))
          (if (or (null cand) (>= 1 time))
              (setq best (max best rate))
            (dolist (to (cadr (gethash id valves)))
              (let ((n-next (if (member id cand)
                                `( ,to ,(+ rate (* (- time 1) (car (gethash id valves))))
                                   ,(- time 2) ,(remove id cand))
                              `(,to ,rate ,(- time 1) ,cand))))
                (unless (gethash n-next seen)
                  (push n-next Q))))))
        (puthash next t seen))
      best)))

;; You're worried that even with an optimal approach, the pressure
;; released won't be enough. What if you got one of the elephants to
;; help you?

;; It would take you 4 minutes to teach an elephant how to open the
;; right valves in the right order, leaving you with only 26 minutes
;; to actually execute your plan. Would having two of you working
;; together be better, even if it means having less time? (Assume that
;; you teach the elephant before opening any valves yourself, giving
;; you both the same full 26 minutes.)

;; With you and an elephant working together for 26 minutes, what is
;; the most pressure you could release?

(defun part2 (file time)
  (let ((valves (make-hash-table :test #'equal))
        (candidates '()))
    (pcase-dolist (`(,id ,rate ,to) (mapcar #'parse-line (slurp file)))
      (puthash id `(,rate ,to) valves)
      (when (> rate 0) (push id candidates)))
    (let* ((Q `(("AA" "AA" 0 ,time ,candidates nil nil)))
           (best-at (make-hash-table :test #'equal))
           (best 0))
      (while-let ((next (pop Q)))
        (pcase-let ((`(,pid ,eid ,rate ,time ,cand ,pbusy ,ebusy) next))
          (if (or (null cand) (>= 1 time))
              (setq best (max best rate))
            (dolist (pto (cadr (gethash pid valves)))
              (dolist (eto (cadr (gethash eid valves)))
                (let ((nrate rate) (npbusy pbusy) (nebusy ebusy) (ncand cand))
                  (cond
                   (npbusy
                    (setq pto pid)
                    (setq npbusy nil))
                   ((member pid ncand)
                    (setq npbusy t)
                    (setq ncand (remove pid ncand))
                    (setq nrate (+ nrate (* (- time 1) (car (gethash pid valves)))))))
                  (cond
                   (nebusy
                    (setq eto eid)
                    (setq nebusy nil))
                   ((member eid ncand)
                    (setq nebusy t)
                    (setq ncand (remove eid ncand))
                    (setq nrate (+ nrate (* (- time 1) (car (gethash eid valves)))))))
                  (let ((n-next `(,pto ,eto ,nrate ,(- time 1) ,ncand ,npbusy ,nebusy))
                        (best-seen (gethash `(,pto ,eto ,(- time 1)) best-at)))
                    (if best-seen
                        (when (< best-seen nrate)
                          (puthash `(,pto ,eto ,(- time 1)) nrate best-at)
                          (push n-next Q))
                      (push n-next Q)))))))
          (puthash `(,pid ,eid ,time) rate best-at)))
      best)))
