(import (rnrs) (AoC-2025))

(define parse-ranges
  (lambda (input)
    (letrec ((parse-range
              (lambda (r)
                (let ((id (lambda (x) x)))
                  (let loop ((chars (string->list r)) (k id))
                    (if (char=? (car chars) #\-)
                        (list (string->number (list->string (k '())))
                              (string->number (list->string (cdr chars))))
                        (loop (cdr chars)
                              (lambda (x)
                                (k (cons (car chars) x))))))))))
      (if (string=? (car input) "")
          '()
          (cons (parse-range (car input))
                (parse-ranges (cdr input)))))))

(define parse-ids
  (lambda (input)
    (if (string=? (car input) "")
        (map string->number (cdr input))
        (parse-ids (cdr input)))))

(define sort-ranges
  (lambda (l)
    (list-sort (lambda (a b)
                 (< (car a) (car b)))
               l)))

(define count-fresh
  (lambda (input)
    (letrec ((in-range?
              (lambda (n range)
                (and (>= n (car range))
                     (<= n (cadr range)))))
             (fresh-id?
              (lambda (n sorted-ranges)
                  (cond ((null? sorted-ranges) #f)
                        ((< n (caar sorted-ranges)) #f)
                        ((in-range? n (car sorted-ranges)) #t)
                        (else
                         (fresh-id? n (cdr sorted-ranges))))))
             (count-trues
              (lambda (l)
                (cond ((null? l) 0)
                      ((car l)
                       (+ 1 (count-trues (cdr l))))
                      (else
                       (count-trues (cdr l)))))))
      (count-trues
       (let ((sorted-ranges (sort-ranges (parse-ranges input)))
             (ids (parse-ids input)))
         (map (partial (flip fresh-id?) sorted-ranges)
              ids))))))

(define range-lower (lambda (r) (car r)))
(define range-upper (lambda (r) (cadr r)))

(define number-in-range
  (lambda (r)
    (+ 1 (- (range-upper r) (range-lower r)))))

(define no-overlap
  (lambda (ranges)
    (letrec ((range-difference
              (lambda (r1 r2)
                (list (+ 1 (range-upper r1))
                      (range-upper r2)))))
      (let loop ((prevr '(0 0)) (remr ranges))
        (if (null? remr)
            '()
            (let ((lowerc (range-lower (car remr)))
                  (upperc (range-upper (car remr)))
                  (upperp (range-upper prevr)))
              (if (> lowerc upperp)
                  (cons (car remr)
                        (loop (car remr) (cdr remr)))
                  (if (<= upperc upperp)
                      (loop prevr (cdr remr))
                      (let ((rdiff
                             (range-difference prevr (car remr))))
                        (cons rdiff
                              (loop rdiff (cdr remr))))))))))))

(define valid-ids
  (lambda (input)
    (fold-right +
                0
                (map number-in-range
                     (no-overlap
                      (sort-ranges
                       (parse-ranges input)))))))

(let ((input (file->lines "input")))
  (display (count-fresh input)) (newline) ; Part 1
  (display (valid-ids input)) (newline))  ; Part 2
