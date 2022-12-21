(load-file "util.el")

;; The encrypted file is a list of numbers. To mix the file, move each
;; number forward or backward in the file a number of positions equal
;; to the value of the number being moved. The list is circular, so
;; moving a number off one end of the list wraps back around to the
;; other end as if the ends were connected.

;; For example, to move the 1 in a sequence like 4, 5, 6, 1, 7, 8, 9,
;; the 1 moves one position forward: 4, 5, 6, 7, 1, 8, 9. To move the
;; -2 in a sequence like 4, -2, 5, 6, 7, 8, 9, the -2 moves two
;; positions backward, wrapping around: 4, 5, 6, 7, 8, -2, 9.

;; The numbers should be moved in the order they originally appear in
;; the encrypted file. Numbers moving around during the mixing process
;; do not change the order in which the numbers are moved.

;; Then, the grove coordinates can be found by looking at the 1000th,
;; 2000th, and 3000th numbers after the value 0, wrapping around the
;; list as necessary.

;; Mix your encrypted file exactly once. What is the sum of the three
;; numbers that form the grove coordinates?

(defun mix (list)
  (let ((len (length list)))
    (dotimes (i len)
      (let* ((index (seq-position list i (lambda (x y) (= (car x) y))))
             (el (elt list index))
             (new-index (+ (mod (- (+ index (cdr el)) 1) (- len 1)) 1)))
        (setq list (seq-remove-at-position list index))
        (setq list (append (seq-subseq list 0 new-index)
                           (list el)
                           (seq-subseq list new-index (- len 1))))))
    list))

(defun part1 (file)
  (let* ((list (seq-map-indexed
                (lambda (x i) (cons i (string-to-number x)))
                (split-string (slurp-raw file))))
         (res (mapcar #'cdr (mix list))))
    (sum
     (mapcar
      (lambda (x) (elt res (mod (+ x (seq-position res 0)) (length res))))
      '(1000 2000 3000)))))

;; The grove coordinate values seem nonsensical. While you ponder the
;; mysteries of Elf encryption, you suddenly remember the rest of the
;; decryption routine you overheard back at camp.

;; First, you need to apply the decryption key, 811589153. Multiply
;; each number by the decryption key before you begin; this will
;; produce the actual list of numbers to mix.

;; Second, you need to mix the list of numbers ten times. The order in
;; which the numbers are mixed does not change during mixing; the
;; numbers are still moved in the order they appeared in the original,
;; pre-mixed list. (So, if -3 appears fourth in the original list of
;; numbers to mix, -3 will be the fourth number to move during each
;; round of mixing.)

;; Apply the decryption key and mix your encrypted file ten
;; times. What is the sum of the three numbers that form the grove
;; coordinates?

(defun part2 (file)
  (let ((list (seq-map-indexed
               (lambda (x i) (cons i (* 811589153 (string-to-number x))))
               (split-string (slurp-raw file)))))
    (dotimes (_ 10)
      (setq list (mix list)))
    (let ((res (mapcar #'cdr list)))
      (sum
       (mapcar
        (lambda (x) (elt res (mod (+ x (seq-position res 0)) (length res))))
        '(1000 2000 3000))))))
