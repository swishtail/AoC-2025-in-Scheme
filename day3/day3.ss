(import (rnrs) (io-lines))

(define highest-joltage
  (lambda (l)
    (string->number
     (list->string
      (letrec ((highest-with-follower-index
                (lambda (l)
                  (let search-loop ((reml l) (h #\0) (hi 0) (i 0))
                    (cond ((null? (cdr reml)) hi)
                          ((char>? (car reml) h)
                           (search-loop (cdr reml) (car reml) i (+ i 1)))
                          (else
                           (search-loop (cdr reml) h hi (+ i 1)))))))
               (second-highest-index
                (lambda (l hi)
                  (let search-loop ((reml (cdr (list-tail l hi)))
                                    (sh #\0) (shi 0) (i (+ hi 1)))
                    (cond ((null? reml) shi)
                          ((char>? (car reml) sh)
                           (search-loop (cdr reml) (car reml) i (+ i 1)))
                          (else
                           (search-loop (cdr reml) sh shi (+ i 1))))))))
        (let* ((hi (highest-with-follower-index l))
               (shi (second-highest-index l hi)))
          (if (< shi hi)
              (list (list-ref l shi) (list-ref l hi))
              (list (list-ref l hi) (list-ref l shi)))))))))

(define part1
  (lambda (input)
    (fold-right +
                0
                (map highest-joltage input))))

(let ((input (map string->list (file->lines "input"))))
  (display (part1 input)) (newline))
