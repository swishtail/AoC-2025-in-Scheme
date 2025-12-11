(import (rnrs) (io-lines))

(define highest-joltage-n
  (lambda (l n)
    (string->number
     (list->string
      (letrec ((list-head
                (lambda (l k)
                  (if (<= k 0)
                      '()
                      (cons (car l)
                            (list-head (cdr l) (- k 1))))))
               (left-most-highest-in-range
                (lambda (r)
                  (let search-loop ((remr r) (h #\0) (hi 0) (i 0))
                    (cond ((null? remr) hi)
                          ((char>? (car remr) h)
                           (search-loop (cdr remr) (car remr) i (+ i 1)))
                          (else
                           (search-loop (cdr remr) h hi (+ i 1))))))))
        (let build-joltage ((n n) (l l))
          (if (= n 0)
              '()
              (let* ((llength (length l))
                     (range (list-head l (+ 1 (- llength n))))
                     (i (left-most-highest-in-range range)))
                (cons (list-ref l i)
                      (build-joltage (- n 1) (cdr (list-tail l i))))))))))))

(define find-highest-joltage
  (lambda (input n)
    (fold-right +
                0
                (map (lambda (input-line)
                       (highest-joltage-n input-line n))
                     input))))

(let ((input (map string->list (file->lines "input"))))
  (display (find-highest-joltage input 2)) (newline)   ; Part 1
  (display (find-highest-joltage input 12)) (newline)) ; Part 2
