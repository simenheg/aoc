(load-file "util.el")

;; For example:

;; NNCB
;;
;; CH -> B
;; HH -> N
;; CB -> H
;; NH -> C
;; HB -> C
;; HC -> B
;; HN -> C
;; NN -> C
;; BH -> H
;; NC -> B
;; NB -> B
;; BN -> B
;; BB -> N
;; BC -> B
;; CC -> N
;; CN -> C

;; The first line is the polymer template - this is the starting point
;; of the process.

;; The following section defines the pair insertion rules. A rule like
;; AB -> C means that when elements A and B are immediately adjacent,
;; element C should be inserted between them. These insertions all
;; happen simultaneously.

;; So, starting with the polymer template NNCB, the first step
;; simultaneously considers all three pairs:

;;     The first pair (NN) matches the rule NN -> C, so element C is
;;     inserted between the first N and the second N.

;;     The second pair (NC) matches the rule NC -> B, so element B is
;;     inserted between the N and the C.

;;     The third pair (CB) matches the rule CB -> H, so element H is
;;     inserted between the C and the B.

;; Note that these pairs overlap: the second element of one pair is
;; the first element of the next pair. Also, because all pairs are
;; considered simultaneously, inserted elements are not considered to
;; be part of a pair until the next step.

;; After the first step of this process, the polymer becomes NCNBCHB.

;; Here are the results of a few steps using the above rules:

;; Template:     NNCB
;; After step 1: NCNBCHB
;; After step 2: NBCCNBBBCBHCB
;; After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
;; After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

;; This polymer grows quickly. After step 5, it has length 97; After
;; step 10, it has length 3073. After step 10, B occurs 1749 times, C
;; occurs 298 times, H occurs 161 times, and N occurs 865 times;
;; taking the quantity of the most common element (B, 1749) and
;; subtracting the quantity of the least common element (H, 161)
;; produces 1749 - 161 = 1588.

;; Apply 10 steps of pair insertion to the polymer template and find
;; the most and least common elements in the result. What do you get
;; if you take the quantity of the most common element and subtract
;; the quantity of the least common element?

(defun parse-rule (rule) (split-string rule " -> "))

(defun parse (file)
  "Return (INIT RULES)."
  (pcase-let ((`(,template _ . ,rules) (slurp file)))
    (list (mapcar #'char-to-string (string-to-list template))
          (mapcar #'parse-rule rules))))

(defun lookup (a b rules)
  (cadr (assoc (concat a b) rules)))

(defun step (template rules)
  (apply
   #'append
   (cl-maplist
    (pcase-lambda (`(,cur . ,rest))
      (if rest
          (list cur (lookup cur (car rest) rules))
        (list cur)))
    template)))

(defun part1 (file)
  (pcase-let ((`(,template ,rules) (parse file)))
    (dotimes (_ 10)
      (setq template (step template rules)))
    (let ((sorted
           (seq-sort-by #'length #'>
                        (seq-group-by #'identity template))))
      (- (length (car sorted))
         (length (car (last sorted)))))))

;; The resulting polymer isn't nearly strong enough to reinforce the
;; submarine. You'll need to run more steps of the pair insertion
;; process; a total of 40 steps should do it.

;; In the above example, the most common element is B (occurring
;; 2192039569602 times) and the least common element is H (occurring
;; 3849876073 times); subtracting these produces 2188189693529.

;; Apply 40 steps of pair insertion to the polymer template and find
;; the most and least common elements in the result. What do you get
;; if you take the quantity of the most common element and subtract
;; the quantity of the least common element?

(defun part2 (file)
  (pcase-let ((`(,template ,rules) (parse file)))
    (dotimes (_ 40)
      (setq template (step template rules)))
    (let ((sorted
           (seq-sort-by #'length #'>
                        (seq-group-by #'identity template))))
      (- (length (car sorted))
         (length (car (last sorted)))))))
