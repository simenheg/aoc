(load-file "util.el")

;; You barely reach the safety of the cave when the whale smashes into
;; the cave mouth, collapsing it. Sensors indicate another exit to
;; this cave at a much greater depth, so you have no choice but to
;; press on.

;; As your submarine slowly makes its way through the cave system, you
;; notice that the four-digit seven-segment displays in your submarine
;; are malfunctioning; they must have been damaged during the
;; escape. You'll be in a lot of trouble without them, so you'd better
;; figure out what's wrong.

;; Each digit of a seven-segment display is rendered by turning on or
;; off any of seven segments named a through g:

;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....

;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg

;; So, to render a 1, only segments c and f would be turned on; the
;; rest would be off. To render a 7, only segments a, c, and f would
;; be turned on.

;; The problem is that the signals which control the segments have
;; been mixed up on each display. The submarine is still trying to
;; display numbers by producing output on signal wires a through g,
;; but those wires are connected to segments randomly. Worse, the
;; wire/segment connections are mixed up separately for each
;; four-digit display! (All of the digits within a display use the
;; same connections, though.)

;; So, you might know that only signal wires b and g are turned on,
;; but that doesn't mean segments b and g are turned on: the only
;; digit that uses two segments is 1, so it must mean segments c and f
;; are meant to be on. With just that information, you still can't
;; tell which wire (b/g) goes to which segment (c/f). For that, you'll
;; need to collect more information.

;; For each display, you watch the changing signals for a while, make
;; a note of all ten unique signal patterns you see, and then write
;; down a single four digit output value (your puzzle input). Using
;; the signal patterns, you should be able to work out which pattern
;; corresponds to which digit.

;; For example, here is what you might see in a single entry in your
;; notes:

;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
;; cdfeb fcadb cdfeb cdbaf

;; (The entry is wrapped here to two lines so it fits; in your notes,
;; it will all be on a single line.)

;; Each entry consists of ten unique signal patterns, a | delimiter,
;; and finally the four digit output value. Within an entry, the same
;; wire/segment connections are used (but you don't know what the
;; connections actually are). The unique signal patterns correspond to
;; the ten different ways the submarine tries to render a digit using
;; the current wire/segment connections. Because 7 is the only digit
;; that uses three segments, dab in the above example means that to
;; render a 7, signal lines d, a, and b are on. Because 4 is the only
;; digit that uses four segments, eafb means that to render a 4,
;; signal lines e, a, f, and b are on.

;; Using this information, you should be able to work out which
;; combination of signal wires corresponds to each of the ten
;; digits. Then, you can decode the four digit output
;; value. Unfortunately, in the above example, all of the digits in
;; the output value (cdfeb fcadb cdfeb cdbaf) use five segments and
;; are more difficult to deduce.

;; For now, focus on the easy digits. Consider this larger example:

;; be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
;; fdgacbe cefdb cefbgd gcbe
;; edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
;; fcgedb cgb dgebacf gc
;; fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
;; cg cg fdcagb cbg
;; fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
;; efabcd cedba gadfec cb
;; aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
;; gecf egdcabf bgf bfgea
;; fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
;; gebdcfa ecba ca fadegcb
;; dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
;; cefg dcbef fcge gbcadfe
;; bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
;; ed bcgafe cdgba cbgef
;; egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
;; gbdfcae bgc cg cgb
;; gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
;; fgae cfgab fg bagce

;; Because the digits 1, 4, 7, and 8 each use a unique number of
;; segments, you should be able to tell which combinations of signals
;; correspond to those digits. Counting only digits in the output
;; values (the part after | on each line), in the above example, there
;; are 26 instances of digits that use a unique number of segments
;; (highlighted above).

;; In the output values, how many times do digits 1, 4, 7, or 8
;; appear?

(defun parse-line (line)
  (mapcar #'split-string (split-string line "|" t " ")))

(defun parse (file)
  (mapcar #'parse-line (slurp file)))

(defun part1 (file)
  (sum
   (mapcar
    (lambda (e)
      (seq-count
       (lambda (s) (member (length s) '(2 4 3 7)))
       e))
    (mapcar #'cl-second (parse file)))))

;; Through a little deduction, you should now be able to determine the
;; remaining digits. Consider again the first example above:

;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
;; cdfeb fcadb cdfeb cdbaf

;; After some careful analysis, the mapping between signal wires and
;; segments only make sense in the following configuration:

;;  dddd
;; e    a
;; e    a
;;  ffff
;; g    b
;; g    b
;;  cccc

;; So, the unique signal patterns would correspond to the following
;; digits:

;;     acedgfb: 8
;;     cdfbe: 5
;;     gcdfa: 2
;;     fbcad: 3
;;     dab: 7
;;     cefabd: 9
;;     cdfgeb: 6
;;     eafb: 4
;;     cagedb: 0
;;     ab: 1

;; Then, the four digits of the output value can be decoded:

;;     cdfeb: 5
;;     fcadb: 3
;;     cdfeb: 5
;;     cdbaf: 3

;; Therefore, the output value for this entry is 5353.

;; Following this same process for each entry in the second, larger
;; example above, the output value of each entry can be determined:

;;     fdgacbe cefdb cefbgd gcbe: 8394
;;     fcgedb cgb dgebacf gc: 9781
;;     cg cg fdcagb cbg: 1197
;;     efabcd cedba gadfec cb: 9361
;;     gecf egdcabf bgf bfgea: 4873
;;     gebdcfa ecba ca fadegcb: 8418
;;     cefg dcbef fcge gbcadfe: 4548
;;     ed bcgafe cdgba cbgef: 1625
;;     gbdfcae bgc cg cgb: 8717
;;     fgae cfgab fg bagce: 4315

;; Adding all of the output values in this larger example produces
;; 61229.

;; For each entry, determine all of the wire/segment connections and
;; decode the four-digit output values. What do you get if you add up
;; all of the output values?

(defun count-to-commons (count)
  (cl-case count
    (2 "cf")
    (3 "acf")
    (4 "bcdf")
    (5 "adg")
    (6 "abfg")
    (7 "abcdefg")))

(defun init-map ()
  (cl-loop for c from ?a to ?g collect (cons c "abcdefg")))

(defun narrow (map entry)
  (let ((commons (count-to-commons (length entry))))
    (dolist (key (string-to-list commons))
      (setf (map-elt map key)
            (seq-intersection (map-elt map key) entry)))
    map))

(defun narrow-list (map list)
  (dolist (entry list)
    (setq map (narrow map entry)))
  map)

(defun singles (map)
  (mapcar
   #'cadr
   (seq-filter
    (lambda (entry)
      (= 1 (length (cdr entry))))
    map)))

(defun trim-map (map)
  (let ((num-singles (length (singles map)))
        (prev-singles -1))
    (while (/= num-singles prev-singles)
      (setq prev-singles num-singles)
      (let ((singles (singles map)))
        (setq num-singles (length singles))
        (dolist (single singles)
          (dolist (key (map-keys map))
            (unless (= 1 (length (map-elt map key)))
              (setf (map-elt map key)
                    (delete single (map-elt map key))))))))
    (dolist (single (singles map))
      (dolist (key (map-keys map))
        (unless (= 1 (length (map-elt map key)))
          (setf (map-elt map key)
                (delete single (map-elt map key))))))
    map))

(defun solve (list)
  (trim-map (narrow-list (init-map) list)))

(defun map-rev (map value)
  (car
   (seq-find
    (lambda (entry)
      (= value (cadr entry)))
    map)))

(defun translate (map input)
  (let ((n 1))
    (dolist (in (string-to-list input))
      (princ (map-rev map in))
      (princ "\n")
      (setq n (* n (map-rev map in))))
    n))

(defun prod-to-num (prod)
  (cl-case prod
    (998600489964 0)
    (10098 1)
    (9990000900 2)
    (10088911800 3)
    (98960400 4)
    (9987003600 5)
    (1008687363600 6)
    (979506 7)
    (99860048996400 8)
    (988713356400 9)))

(defun print-map (map)
  (dolist (entry map)
    (princ (format "%s: %s"
                   (char-to-string (car entry))
                   (mapcar #'char-to-string (cdr entry))))
    (princ "\n")))

(defun translate-string (map input-list)
  (string-to-number
   (apply
    #'concat
    (mapcar
     #'number-to-string
     (cl-loop for input in input-list
              collect (prod-to-num (translate map input)))))))

(defun part2 (file)
  (let ((in (parse file))
        (sum 0))
    (pcase-dolist (`(,input ,output) in)
      (let ((solution (solve input)))
        (cl-incf sum (translate-string solution output))))
    sum))

;; ("ab" "dab" "eafb" "acedgfb" "cdfbe" "gcdfa" "fbcad" "cefabd" "cdfgeb" "cagedb")
