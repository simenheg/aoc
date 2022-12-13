(load-file "util.el")

;; You climb the hill and again try contacting the Elves. However, you
;; instead receive a signal you weren't expecting: a distress signal.

;; Your handheld device must still not be working properly; the
;; packets from the distress signal got decoded out of order. You'll
;; need to re-order the list of received packets (your puzzle input)
;; to decode the message.

;; Your list consists of pairs of packets; pairs are separated by a
;; blank line. You need to identify how many pairs of packets are in
;; the right order.

;; For example:

;; [1,1,3,1,1]
;; [1,1,5,1,1]

;; [[1],[2,3,4]]
;; [[1],4]

;; [9]
;; [[8,7,6]]

;; [[4,4],4,4]
;; [[4,4],4,4,4]

;; [7,7,7,7]
;; [7,7,7]

;; []
;; [3]

;; [[[]]]
;; [[]]

;; [1,[2,[3,[4,[5,6,7]]]],8,9]
;; [1,[2,[3,[4,[5,6,0]]]],8,9]

;; Packet data consists of lists and integers. Each list starts with
;; [, ends with ], and contains zero or more comma-separated values
;; (either integers or other lists). Each packet is always a list and
;; appears on its own line.

;; When comparing two values, the first value is called left and the
;; second value is called right. Then:

;;     If both values are integers, the lower integer should come
;;     first. If the left integer is lower than the right integer, the
;;     inputs are in the right order. If the left integer is higher
;;     than the right integer, the inputs are not in the right
;;     order. Otherwise, the inputs are the same integer; continue
;;     checking the next part of the input.

;;     If both values are lists, compare the first value of each list,
;;     then the second value, and so on. If the left list runs out of
;;     items first, the inputs are in the right order. If the right
;;     list runs out of items first, the inputs are not in the right
;;     order. If the lists are the same length and no comparison makes
;;     a decision about the order, continue checking the next part of
;;     the input.

;;     If exactly one value is an integer, convert the integer to a
;;     list which contains that integer as its only value, then retry
;;     the comparison. For example, if comparing [0,0,0] and 2,
;;     convert the right value to [2] (a list containing 2); the
;;     result is then found by instead comparing [0,0,0] and [2].

;; What are the indices of the pairs that are already in the right
;; order? (The first pair has index 1, the second pair has index 2,
;; and so on.) In the above example, the pairs in the right order are
;; 1, 2, 4, and 6; the sum of these indices is 13.

;; Determine which pairs of packets are already in the right
;; order. What is the sum of the indices of those pairs?

(defun comp (left right)
  (let ((l (car left))
        (r (car right)))
    (cond
     ((and (numberp l) (numberp r))
      (cond ((< l r) (throw 'done t))
            ((> l r) (throw 'done nil))
            (t (comp (cdr left) (cdr right)))))
     ((and (listp l) (numberp r))
      (comp left (cons (list r) (cdr right))))
     ((and (numberp l) (listp r))
      (comp (cons (list l) (cdr left)) right))
     ((and (null left) (null right)) nil)
     ((null left) (throw 'done t))
     ((null right) (throw 'done nil))
     ((and (listp l) (listp r))
      (comp l r)
      (comp (cdr left) (cdr right))))))

(defun parse (file)
  (mapcar
   #'read
   (seq-filter
    (lambda (s) (not (string-empty-p s)))
    (split-string
     (thread-last
       (slurp-raw file)
       (string-replace "]" ")")
       (string-replace "[" "(")
       (string-replace "," " "))
     "\n"))))

(defun part1 (file)
  (let ((sum 0))
    (seq-do-indexed
     (pcase-lambda (`(,left ,right) i)
       (when (catch 'done (comp left right))
         (cl-incf sum (+ i 1))))
     (seq-partition (parse file) 2))
    sum))

;; Now, you just need to put all of the packets in the right
;; order. Disregard the blank lines in your list of received packets.

;; The distress signal protocol also requires that you include two
;; additional divider packets:

;; [[2]]
;; [[6]]

;; Using the same rules as before, organize all packets - the ones in
;; your list of received packets as well as the two divider packets -
;; into the correct order.

;; For the example above, the result of putting the packets in the
;; correct order is:

;; []
;; [[]]
;; [[[]]]
;; [1,1,3,1,1]
;; [1,1,5,1,1]
;; [[1],[2,3,4]]
;; [1,[2,[3,[4,[5,6,0]]]],8,9]
;; [1,[2,[3,[4,[5,6,7]]]],8,9]
;; [[1],4]
;; [[2]]
;; [3]
;; [[4,4],4,4]
;; [[4,4],4,4,4]
;; [[6]]
;; [7,7,7]
;; [7,7,7,7]
;; [[8,7,6]]
;; [9]

;; Afterward, locate the divider packets. To find the decoder key for
;; this distress signal, you need to determine the indices of the two
;; divider packets and multiply them together. (The first packet is at
;; index 1, the second packet is at index 2, and so on.) In this
;; example, the divider packets are 10th and 14th, and so the decoder
;; key is 140.

;; Organize all of the packets into the correct order. What is the
;; decoder key for the distress signal?

(defun part2 (file)
  (let ((s (sort (parse file) (lambda (a b) (catch 'done (comp a b))))))
    (* (+ 1 (seq-position s '((2))))
       (+ 1 (seq-position s '((6)))))))
