(load-file "util.el")

;; Start by figuring out the signal being sent by the CPU. The CPU has
;; a single register, X, which starts with the value 1. It supports
;; only two instructions:

;;     addx V takes two cycles to complete. After two cycles, the X
;;     register is increased by the value V. (V can be negative.)

;;     noop takes one cycle to complete. It has no other effect.

;; The CPU uses these instructions in a program (your puzzle input)
;; to, somehow, tell the screen what to draw.

;; Consider the following small program:

;; noop
;; addx 3
;; addx -5

;; Execution of this program proceeds as follows:

;;     At the start of the first cycle, the noop instruction begins
;;     execution. During the first cycle, X is 1. After the first
;;     cycle, the noop instruction finishes execution, doing nothing.

;;     At the start of the second cycle, the addx 3 instruction begins
;;     execution. During the second cycle, X is still 1.

;;     During the third cycle, X is still 1. After the third cycle,
;;     the addx 3 instruction finishes execution, setting X to 4.

;;     At the start of the fourth cycle, the addx -5 instruction
;;     begins execution. During the fourth cycle, X is still 4.

;;     During the fifth cycle, X is still 4. After the fifth cycle,
;;     the addx -5 instruction finishes execution, setting X to -1.

;; Maybe you can learn something by looking at the value of the X
;; register throughout execution. For now, consider the signal
;; strength (the cycle number multiplied by the value of the X
;; register) during the 20th cycle and every 40 cycles after that
;; (that is, during the 20th, 60th, 100th, 140th, 180th, and 220th
;; cycles).

;; Find the signal strength during the 20th, 60th, 100th, 140th,
;; 180th, and 220th cycles. What is the sum of these six signal
;; strengths?

(defun parse (line)
  (pcase (split-string line)
    (`(,instr ,arg) 
     `(2 ,instr ,(string-to-number arg)))
    (`(,instr)
     `(1 ,instr))))

(defun exec (cycle X lines)
  (let ((queue '())
        (sum 0))
    (while (or queue lines)
      (when (or (= 20 cycle) (= 0 (mod (- cycle 20) 40)))
        (cl-incf sum (* cycle X)))
      (unless queue
        (push (parse (pop lines)) queue))
      (pcase-let ((`(,n ,instr ,arg) (pop queue)))
        (cl-decf n)
        (if (= 0 n)
            (when (equal instr "addx")
              (cl-incf X arg))
          (push `(,n ,instr ,arg) queue)))
      (cl-incf cycle))
    sum))

(defun part1 (file)
  (exec 1 1 (slurp file)))

;; It seems like the X register controls the horizontal position of a
;; sprite. Specifically, the sprite is 3 pixels wide, and the X
;; register sets the horizontal position of the middle of that
;; sprite. (In this system, there is no such thing as "vertical
;; position": if the sprite's horizontal position puts its pixels
;; where the CRT is currently drawing, then those pixels will be
;; drawn.)

;; You count the pixels on the CRT: 40 wide and 6 high. This CRT
;; screen draws the top row of pixels left-to-right, then the row
;; below that, and so on. The left-most pixel in each row is in
;; position 0, and the right-most pixel in each row is in position 39.

;; Like the CPU, the CRT is tied closely to the clock circuit: the CRT
;; draws a single pixel during each cycle. Representing each pixel of
;; the screen as a #, here are the cycles during which the first and
;; last pixel in each row are drawn:

;; Cycle   1 -> ######################################## <- Cycle  40
;; Cycle  41 -> ######################################## <- Cycle  80
;; Cycle  81 -> ######################################## <- Cycle 120
;; Cycle 121 -> ######################################## <- Cycle 160
;; Cycle 161 -> ######################################## <- Cycle 200
;; Cycle 201 -> ######################################## <- Cycle 240

;; So, by carefully timing the CPU instructions and the CRT drawing
;; operations, you should be able to determine whether the sprite is
;; visible the instant each pixel is drawn. If the sprite is
;; positioned such that one of its three pixels is the pixel currently
;; being drawn, the screen produces a lit pixel (#); otherwise, the
;; screen leaves the pixel dark (.).

;; Render the image given by your program. What eight capital letters
;; appear on your CRT?

(defun draw (crt)
  (apply
   #'concat
   (seq-map-indexed
    (lambda (x i)
      (concat
       (if (= 0 (mod i 40)) "\n" "")
       (if x "#" ".")))
    crt)))

(defun exec2 (cycle X lines)
  (let ((queue '())
        (crt (make-list (* 40 6) nil)))
    (while (or queue lines)
      (unless queue
        (push (parse (pop lines)) queue))
      (when (>= 1 (abs (- (mod cycle 40) X)))
        (setf (elt crt cycle) t))
      (pcase-let ((`(,n ,instr ,arg) (pop queue)))
        (cl-decf n)
        (if (= 0 n)
            (when (equal instr "addx")
              (cl-incf X arg))
          (push `(,n ,instr ,arg) queue)))
      (cl-incf cycle))
    (draw crt)))

(defun part2 (file)
  (exec2 0 1 (slurp file)))
