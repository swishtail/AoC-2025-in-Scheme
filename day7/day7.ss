(import (rnrs) (AoC-2025))

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
        (let ((x1 (caar set1)) (x2 (car set2)))
          (cond ((< x1 x2)
                 (intersection (cdr set1) set2))
                ((> x1 x2)
                 (intersection set1 (cdr set2)))
                (else ; x1 = x2
                 (cons (car set1)
                       (intersection (cdr set1)
                                     (cdr set2)))))))))

(define difference
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((null? set2) set1)
          (else
           (let ((x1 (caar set1)) (x2 (car set2)))
             (cond ((< x1 x2)
                    (cons (car set1)
                          (difference (cdr set1) set2)))
                   ((> x1 x2)
                    (difference set1 (cdr set2)))
                   (else ; x1 = x2
                    (difference (cdr set1)
                                (cdr set2)))))))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (caar set1)) (x2 (caar set2)))
             (cond ((< x1 x2)
                    (cons (car set1)
                          (union (cdr set1) set2)))
                   ((> x1 x2)
                    (cons (car set2)
                          (union set1 (cdr set2))))
                   (else ; x1 = x2
                    (cons (list (caar set1)
                                (+ (cadar set1)
                                   (cadar set2)))
                          (union (cdr set1)
                                 (cdr set2))))))))))

(define split-beam
  (let ((c 0))
    (lambda (m)
      (if (eq? m 'reveal)
          c
          (begin
            (set! c (+ c 1))
            (list (list (- (car m) 1) (cadr m))
                  (list (+ (car m) 1) (cadr m))))))))

(define split-line
  (lambda (l)
    (letrec ((sort
              (lambda (l)
                (cond ((null? l) '())
                      ((null? (cdr l)) l)
                      ((< (caar l) (caadr l))
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
                    (if (= (caar l) (caadr l))
                        (unique (cons (list (caadr l)
                                            (+ (cadar l)
                                               (cadadr l)))
                                      (cddr l)))
                        (cons (car l)
                              (unique (cdr l))))))))
      (unique
       (sort
        (fold-right append
                    '()
                    (map split-beam l)))))))

(define manifold
  (lambda (splitters)
    (let loop ((new-beams (list (list (caar splitters) 1)))
               (splitters splitters))
      (if (null? splitters)
          new-beams
          (let ((beams-to-split (intersection new-beams (car splitters)))
                (unaffected-beams (difference new-beams (car splitters))))
            (loop (union (split-line beams-to-split) unaffected-beams)
                  (cdr splitters)))))))

(let ((input (parse-input (file->lines "input"))))
  (let ((result (manifold input)))
    (begin
      (display (split-beam 'reveal)) (newline) ; Part 1
      (display                                 ; Part 2
       (fold-right +
                   0
                   (map cadr result)))
      (newline))))
