(load-file "util.el")

;;     [D]
;; [N] [C]
;; [Z] [M] [P]
;;  1   2   3

;; move 1 from 2 to 1
;; move 3 from 1 to 3
;; move 2 from 2 to 1
;; move 1 from 1 to 2

;; In this example, there are three stacks of crates. Stack 1 contains
;; two crates: crate Z is on the bottom, and crate N is on top. Stack
;; 2 contains three crates; from bottom to top, they are crates M, C,
;; and D. Finally, stack 3 contains a single crate, P.

;; Then, the rearrangement procedure is given. In each step of the
;; procedure, a quantity of crates is moved from one stack to a
;; different stack. In the first step of the above rearrangement
;; procedure, one crate is moved from stack 2 to stack 1, resulting in
;; this configuration:

;; [D]
;; [N] [C]
;; [Z] [M] [P]
;;  1   2   3

;; In the second step, three crates are moved from stack 1 to stack
;; 3. Crates are moved one at a time, so the first crate to be moved
;; (D) ends up below the second and third crates:

;;         [Z]
;;         [N]
;;     [C] [D]
;;     [M] [P]
;;  1   2   3

;; Then, both crates are moved from stack 2 to stack 1. Again, because
;; crates are moved one at a time, crate C ends up below crate M:

;;         [Z]
;;         [N]
;; [M]     [D]
;; [C]     [P]
;;  1   2   3

;; Finally, one crate is moved from stack 1 to stack 2:

;;         [Z]
;;         [N]
;;         [D]
;; [C] [M] [P]
;;  1   2   3

;; The Elves just need to know which crate will end up on top of each
;; stack; in this example, the top crates are C in stack 1, M in stack
;; 2, and Z in stack 3, so you should combine these together and give
;; the Elves the message CMZ.

;; After the rearrangement procedure completes, what crate ends up on
;; top of each stack?

(defun parse-stacks (file)
  (with-temp-buffer
    (save-excursion
      (insert-file-contents file))
    (let ((res '()))
      (while (not (looking-at-p " 1"))
        (when (bolp)
          (push '() res))
        (cl-case (char-after)
          (?\s (when (looking-at-p "   ")
                 (push nil (car res))
                 (forward-char 3)))
          (?\[ (push (char-to-string (char-after (+ (point) 1)))
                     (car res))))
        (forward-char))
      (reverse
       (mapcar
        (lambda (l)
          (reverse (seq-filter #'identity l)))
        (transpose res))))))

(defun move (stacks from to)
  (push (pop (elt stacks (- from 1)))
        (elt stacks (- to 1))))

(defun part1 (file)
  (let ((stacks (parse-stacks file)))
    (dolist (line (slurp file))
      (when (string-prefix-p "move" line)
        (seq-let (_ n _ from _ to) (split-string line)
          (dotimes (_ (string-to-number n))
            (move stacks (string-to-number from) (string-to-number to))))))
    (apply #'concat (mapcar #'car stacks))))

;; Again considering the example above, the crates begin in the same
;; configuration:

;;     [D]
;; [N] [C]
;; [Z] [M] [P]
;;  1   2   3

;; Moving a single crate from stack 2 to stack 1 behaves the same as
;; before:

;; [D]
;; [N] [C]
;; [Z] [M] [P]
;;  1   2   3

;; However, the action of moving three crates from stack 1 to stack 3
;; means that those three moved crates stay in the same order,
;; resulting in this new configuration:

;;         [D]
;;         [N]
;;     [C] [Z]
;;     [M] [P]
;;  1   2   3

;; Next, as both crates are moved from stack 2 to stack 1, they retain
;; their order as well:

;;         [D]
;;         [N]
;; [C]     [Z]
;; [M]     [P]
;;  1   2   3

;; Finally, a single crate is still moved from stack 1 to stack 2, but
;; now it's crate C that gets moved:

;;         [D]
;;         [N]
;;         [Z]
;; [M] [C] [P]
;;  1   2   3

;; In this example, the CrateMover 9001 has put the crates in a
;; totally different order: MCD.

;; After the rearrangement procedure completes, what crate ends up on
;; top of each stack?

(defun move-n (stacks n from to)
  (let ((tmp '()))
    (dotimes (_ n)
      (push (pop (elt stacks (- from 1))) tmp))
    (dotimes (_ n)
      (push (pop tmp) (elt stacks (- to 1))))))

(defun part2 (file)
  (let ((stacks (parse-stacks file)))
    (dolist (line (slurp file))
      (when (string-prefix-p "move" line)
        (seq-let (_ n _ from _ to) (split-string line)
          (move-n stacks (string-to-number n) (string-to-number from)
                  (string-to-number to)))))
    (apply #'concat (mapcar #'car stacks))))
