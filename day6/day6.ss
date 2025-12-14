(import (rnrs) (rnrs eval) (AoC-2025))

(define id (lambda (x) x))

(define chars->type
  (lambda (chars)
    (if (char-numeric? (car chars))
        (string->number (list->string chars))
        (string->symbol (list->string chars)))))

(define parse-string-human
  (lambda (s)
    (map chars->type
         (filter (compose not null?)
                 (let loop ((chars (string->list s)) (k id))
                   (if (null? chars)
                       (list (k '()))
                       (if (char=? (car chars) #\space)
                           (cons (k '()) (loop (cdr chars) id))
                           (loop (cdr chars)
                                 (lambda (x)
                                   (k (cons (car chars) x)))))))))))

(define parse-strings-cephalopod
  (lambda (s)
    (letrec ((last
              (lambda (l)
                (if (null? (cdr l))
                    (car l)
                    (last (cdr l)))))
             (all-but-last
              (lambda (l)
                (if (null? (cdr l))
                    '()
                    (cons (car l)
                          (all-but-last (cdr l))))))
             (remove-spaces
              (lambda (chars)
                (if (null? chars)
                    '()
                    (if (char=? (car chars) #\space)
                        (remove-spaces (cdr chars))
                        (cons (car chars)
                              (remove-spaces (cdr chars)))))))
             (all-spaces?
              (lambda (chars)
                (if (null? chars)
                    #t
                    (if (char=? (car chars) #\space)
                        (all-spaces? (cdr chars))
                        #f))))
             (group-problems
              (lambda (l)
                (let loop ((reml l) (k id))
                  (if (null? reml)
                      (list (map remove-spaces (k '())))
                      (if (all-spaces? (car reml))
                          (cons (map remove-spaces (k '()))
                                (loop (cdr reml) id))
                          (loop (cdr reml)
                                (lambda (x)
                                  (k (cons (car reml) x)))))))))
             (group->sexp
              (lambda (group)
                (append (list (chars->type (list (last (car group))))
                              (chars->type (all-but-last (car group))))
                        (map chars->type (list-tail group 1))))))
      (map group->sexp
           (group-problems
            (apply map list (map string->list s)))))))

(define part1
  (lambda (input)
    (fold-right +
                0
                (map (partial (flip eval) (environment '(rnrs)))
                     (map reverse
                          (apply map
                                 list
                                 (map parse-string-human input)))))))

(define part2
  (lambda (input)
    (fold-right +
                0
                (map (partial (flip eval) (environment '(rnrs)))
                     (parse-strings-cephalopod input)))))

(let ((input (file->lines "input")))
  (display (part1 input)) (newline)
  (display (part2 input)) (newline))
