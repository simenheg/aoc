(load-file "util.el")

;; As you finally start making your way upriver, you realize your pack
;; is much lighter than you remember. Just then, one of the items from
;; your pack goes flying overhead. Monkeys are playing Keep Away with
;; your missing things!

;; To get your stuff back, you need to be able to predict where the
;; monkeys will throw your items. After some careful observation, you
;; realize the monkeys operate based on how worried you are about each
;; item.

;; You take some notes (your puzzle input) on the items each monkey
;; currently has, how worried you are about those items, and how the
;; monkey makes decisions based on your worry level. For example:

;; Monkey 0:
;;   Starting items: 79, 98
;;   Operation: new = old * 19
;;   Test: divisible by 23
;;     If true: throw to monkey 2
;;     If false: throw to monkey 3

;; Monkey 1:
;;   Starting items: 54, 65, 75, 74
;;   Operation: new = old + 6
;;   Test: divisible by 19
;;     If true: throw to monkey 2
;;     If false: throw to monkey 0

;; Monkey 2:
;;   Starting items: 79, 60, 97
;;   Operation: new = old * old
;;   Test: divisible by 13
;;     If true: throw to monkey 1
;;     If false: throw to monkey 3

;; Monkey 3:
;;   Starting items: 74
;;   Operation: new = old + 3
;;   Test: divisible by 17
;;     If true: throw to monkey 0
;;     If false: throw to monkey 1

;; Each monkey has several attributes:

;;     Starting items lists your worry level for each item the monkey
;;     is currently holding in the order they will be inspected.

;;     Operation shows how your worry level changes as that monkey
;;     inspects an item. (An operation like new = old * 5 means that
;;     your worry level after the monkey inspected the item is five
;;     times whatever your worry level was before inspection.)

;;     Test shows how the monkey uses your worry level to decide where
;;     to throw an item next.

;;         If true shows what happens with an item if the Test was
;;         true.

;;         If false shows what happens with an item if the Test was
;;         false.

;; After each monkey inspects an item but before it tests your worry
;; level, your relief that the monkey's inspection didn't damage the
;; item causes your worry level to be divided by three and rounded
;; down to the nearest integer.

;; The monkeys take turns inspecting and throwing items. On a single
;; monkey's turn, it inspects and throws all of the items it is
;; holding one at a time and in the order listed. Monkey 0 goes first,
;; then monkey 1, and so on until each monkey has had one turn. The
;; process of each monkey taking a single turn is called a round.

;; When a monkey throws an item to another monkey, the item goes on
;; the end of the recipient monkey's list. A monkey that starts a
;; round with no items could end up inspecting and throwing many items
;; by the time its turn comes around. If a monkey is holding no items
;; at the start of its turn, its turn ends.

;; Chasing all of the monkeys at once is impossible; you're going to
;; have to focus on the two most active monkeys if you want any hope
;; of getting your stuff back. Count the total number of times each
;; monkey inspects items over 20 rounds:

;; Monkey 0 inspected items 101 times.
;; Monkey 1 inspected items 95 times.
;; Monkey 2 inspected items 7 times.
;; Monkey 3 inspected items 105 times.

;; In this example, the two most active monkeys inspected items 101
;; and 105 times. The level of monkey business in this situation can
;; be found by multiplying these together: 10605.

;; Figure out which monkeys to chase by counting how many items they
;; inspect over 20 rounds. What is the level of monkey business after
;; 20 rounds of stuff-slinging simian shenanigans?

(cl-defstruct monkey
  items op n divisor true false (inspect 0))

(defun parse-monkey (str)
  (let ((m (make-monkey))
        (lines (split-string str "\n")))
    (pcase-let ((`(_ _ . ,items) (split-string (elt lines 1))))
      (setf (monkey-items m) (mapcar #'string-to-number items)))
    (pcase-let ((`(_ _ _ _ ,op ,n) (split-string (elt lines 2))))
      (setf (monkey-op m) (intern op))
      (setf (monkey-n m) (if (equal n "old") 'old (string-to-number n))))
    (pcase-let ((`(_ _ _ ,divisor) (split-string (elt lines 3))))
      (setf (monkey-divisor m) (string-to-number divisor)))
    (pcase-let ((`(_ _ _ _ _ ,true) (split-string (elt lines 4))))
      (setf (monkey-true m) (string-to-number true)))
    (pcase-let ((`(_ _ _ _ _ ,false) (split-string (elt lines 5))))
      (setf (monkey-false m) (string-to-number false)))
    m))

(defun parse (str)
  (let ((res '())
        (pos 0))
    (while-let ((beg (and pos (string-search "M" str pos))))
      (let ((end (string-search "M" str (+ pos 1))))
        (push (string-trim (substring str beg end)) res)
        (setq pos end)))
    (mapcar #'parse-monkey (reverse res))))

(defun simulate (monkeys)
  (dolist (m monkeys)
    (while-let ((item (pop (monkey-items m)))
                (op (monkey-op m))
                (n (monkey-n m))
                (div (monkey-divisor m))
                (true (monkey-true m))
                (false (monkey-false m)))
      (let* ((new (/ (funcall op item (if (eq 'old n) item n)) 3))
             (next (if (= 0 (mod new div)) true false)))
        (cl-incf (monkey-inspect m))
        (setf (monkey-items (elt monkeys next))
              (append (monkey-items (elt monkeys next)) `(,new))))))
  monkeys)

(defun part1 (file)
  (let ((monkeys (parse (slurp-raw file))))
    (dotimes (_ 20)
      (simulate monkeys))
    (apply
     #'*
     (mapcar #'monkey-inspect
             (take 2 (seq-sort-by #'monkey-inspect #'> monkeys))))))

;; You're worried you might not ever get your items back. So worried,
;; in fact, that your relief that a monkey's inspection didn't damage
;; an item no longer causes your worry level to be divided by three.

;; Unfortunately, that relief was all that was keeping your worry
;; levels from reaching ridiculous levels. You'll need to find another
;; way to keep your worry levels manageable.

;; After 10000 rounds, the two most active monkeys inspected items
;; 52166 and 52013 times. Multiplying these together, the level of
;; monkey business in this situation is now 2713310158.

;; Worry levels are no longer divided by three after each item is
;; inspected; you'll need to find another way to keep your worry
;; levels manageable. Starting again from the initial state in your
;; puzzle input, what is the level of monkey business after 10000
;; rounds?

(defun simulate2 (monkeys div-prod)
  (dolist (m monkeys)
    (while-let ((item (pop (monkey-items m)))
                (op (monkey-op m))
                (n (monkey-n m))
                (div (monkey-divisor m))
                (true (monkey-true m))
                (false (monkey-false m)))
      (let* ((new (mod (funcall op item (if (eq 'old n) item n)) div-prod))
             (next (if (= 0 (mod new div)) true false)))
        (cl-incf (monkey-inspect m))
        (setf (monkey-items (elt monkeys next))
              (append (monkey-items (elt monkeys next)) `(,new))))))
  monkeys)

(defun part2 (file)
  (let* ((monkeys (parse (slurp-raw file)))
         (div-prod (apply #'* (mapcar #'monkey-divisor monkeys))))
    (dotimes (_ 10000)
      (simulate2 monkeys div-prod))
    (apply
     #'*
     (mapcar #'monkey-inspect
             (take 2 (seq-sort-by #'monkey-inspect #'> monkeys))))))
