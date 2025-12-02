(import (rnrs) (io-lines))

(define parse-line
  (lambda (l)
    (let ((chars (string->list l)))
      (list (car chars)
            (string->number (list->string (cdr chars)))))))

(define zero-crossings
  (lambda (d v t)
    (let loop ((newv v) (remt t) (c 0))
      (if (zero? remt)
          (list newv c)
          (if (zero? newv)
              (loop (mod (d newv 1) 100) (- remt 1) (+ c 1))
              (loop (mod (d newv 1) 100) (- remt 1) c))))))

(define rotate
  (lambda (l v)
    (if (null? l)
        '()
        (let* ((d (if (char=? (caar l) #\R) + -))
               (v-and-c (zero-crossings d v (cadar l))))
          (cons v-and-c
                (rotate (cdr l) (car v-and-c)))))))

(define part1
  (lambda (input)
    (length
     (filter zero?
             (map car
                  (rotate input 50))))))

(define part2
  (lambda (input)
    (fold-right +
                0
                (map cadr
                     (rotate input 50)))))

(let ((input (map parse-line (file->lines "input"))))
  (display (part1 input)) (newline)
  (display (part2 input)) (newline))
