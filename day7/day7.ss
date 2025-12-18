(import (rnrs) (AoC-2025))
(define (l) (load "day7.ss"))

(define test (file->lines "test"))

(define parse-input
  (lambda (input)
    (letrec ((contains-splitters?
              (lambda (l)
                (if (null? l)
                    #f
                    (if (char=? (car l) #\^)
                        #t
                        (contains-splitters? (cdr l))))))
             (splitter-indices
              (lambda (l)
                (let loop ((i 0) (l l))
                  (if (null? l)
                      '()
                      (if (char=? (car l) #\^)
                          (cons i (loop (+ i 1) (cdr l)))
                          (loop (+ i 1) (cdr l))))))))
      (map splitter-indices
           (filter contains-splitters?
                   (map string->list (cdr input)))))))

(define intersection
  (lambda (set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((< x1 x2)
                 (intersection (cdr set1) set2))
                ((> x1 x2)
                 (intersection set1 (cdr set2)))
                (else ; x1 = x2
                 (cons x1
                       (intersection (cdr set1)
                                     (cdr set2)))))))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((< x1 x2)
                    (cons x1
                          (union (cdr set1) set2)))
                   ((> x1 x2)
                    (cons x2
                          (union set1 (cdr set2))))
                   (else ; x1 = x2
                    (cons x1
                          (union (cdr set1)
                                 (cdr set2))))))))))

(define difference
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((null? set2) set1)
          (else
           (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((< x1 x2)
                    (cons x1
                          (difference (cdr set1) set2)))
                   ((> x1 x2)
                    (difference set1 (cdr set2)))
                   (else ; x1 = x2
                    (difference (cdr set1)
                                (cdr set2)))))))))

(define split-beam
  (let ((c 0))
    (lambda (m)
      (if (eq? m 'reveal)
          c
          (begin
            (set! c (+ c 1))
            (list (- m 1) (+ m 1)))))))

(define split-line
  (lambda (l)
    (letrec ((sort
              (lambda (l)
                (cond ((null? l) '())
                      ((null? (cdr l)) l)
                      ((< (car l) (cadr l))
                       (cons (car l)
                             (sort (cdr l))))
                      (else
                       (cons (cadr l)
                             (cons (car l)
                                   (sort (cddr l))))))))
             (unique
              (lambda (l)
                (if (null? (cdr l))
                    (list (car l))
                    (if (= (car l) (cadr l))
                        (unique (cdr l))
                        (cons (car l)
                              (unique (cdr l))))))))
      (unique
       (sort
        (fold-right append
                    '()
                    (map split-beam l)))))))

(define split-line-duplicates
  (lambda (l)
    (list-sort <
               (fold-right append
                           '()
                           (map split-beam l)))))

(define pascal-rows
  (lambda (n)
    (letrec ((generate-row
              (lambda (prev)
                (let loop ((prev
                            (cons 0
                                  (append prev
                                          (list 0)))))
                  (if (null? (cdr prev))
                      '()
                      (cons (+ (car prev) (cadr prev))
                            (loop (cdr prev))))))))
      (cons (list 1)
            (let loop ((row (list 1)) (n n))
              (if (= n 0)
                  '()
                  (let ((new-row (generate-row row)))
                    (cons new-row
                          (loop new-row (- n 1))))))))))

(define manifold
  (lambda (splitters)
    (let loop ((new-beams (car splitters)) (splitters splitters) (rows 0))
      (if (null? splitters)
          rows
          (let ((beams-to-split (intersection new-beams (car splitters)))
                (unaffected-beams (difference new-beams (car splitters))))
            (loop (union (split-line beams-to-split) unaffected-beams)
                  (cdr splitters)
                  (+ rows 1)))))))

(let ((input (parse-input (file->lines "input"))))
  (begin
    (display (manifold input)) (newline)
    (display (split-beam 'reveal)) (newline)))
