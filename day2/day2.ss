(import (rnrs) (AoC-2025))

(define parse-input
  (lambda (input)
    (letrec ((id (lambda (x) x))
             (separate
              (lambda (l s)
                (let sep-loop ((reml l) (k id))
                  (cond ((null? reml) (list (k '())))
                        ((char=? (car reml) s)
                         (cons (k '())
                               (sep-loop (cdr reml) id)))
                        (else
                         (sep-loop (cdr reml)
                                   (lambda (x)
                                     (k (cons (car reml) x))))))))))
      (map (partial map (compose string->number list->string))
           (map (partial (flip separate) #\-)
                (separate (string->list input) #\,))))))

(define reduplicated-number?
  (lambda (n)
    (letrec ((split-digits
              (lambda (n)
                (let* ((ndigits (exact (floor (+ (log n 10) 1))))
                       (p (expt 10 (/ ndigits 2))))
                  (list (floor (/ n p)) (mod n p))))))
      (let ((p (split-digits n)))
        (= (car p) (cadr p))))))

(define repeating-number?
  (lambda (n)
    (letrec ((list-head
              (lambda (l n)
                (if (<= n 0)
                    '()
                    (cons (car l)
                          (list-head (cdr l) (- n 1))))))
             (scanner
              (lambda (ld bl)
                (let ((ldlength (length ld)))
                  (if (integer? (/ ldlength bl))
                      (cond ((null? (list-tail ld bl)) #t)
                            ((equal? (list-head ld bl)
                                     (list-head (list-tail ld bl) bl))
                             (scanner (list-tail ld bl) bl))
                            (else #f))
                      #f)))))
      (let ((digits (string->list (number->string n))))
        (let scanner-loop ((bl 1))
          (if (= bl (length digits))
              #f
              (or (scanner digits bl)
                  (scanner-loop (+ bl 1)))))))))

(define make-range
  (lambda (p)
    (let nextn ((n (car p)))
      (if (> n (cadr p))
          '()
          (cons n
                (nextn (+ n 1)))))))

(define answer
  (lambda (input pred)
    (fold-right +
                0
                (map (partial fold-right + 0)
                     (map (partial filter pred)
                          (map make-range (parse-input input)))))))

(let ((input (car (file->lines "input"))))
  (display (answer input reduplicated-number?)) (newline) ; Part 1
  (display (answer input repeating-number?)) (newline))   ; Part 2
