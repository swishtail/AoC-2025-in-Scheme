(import (rnrs) (AoC-2025))

(define parse-input
  (lambda (input)
    (letrec ((separate
              (lambda (l)
                (let loop ((l l) (k (lambda (x) x)))
                  (if (char=? (car l) #\,)
                      (cons (k '())(list (cdr l)))
                      (loop (cdr l)
                            (lambda (x)
                              (k (cons (car l) x)))))))))
      (map (partial map (compose string->number list->string))
           (map separate
                (map string->list input))))))

(define corner-pairs
  (lambda (tiles)
    (let outer ((tiles tiles))
      (if (null? (cdr tiles))
          '()
          (let inner ((x (car tiles)) (remt (cdr tiles)))
            (if (null? remt)
                (outer (cdr tiles))
                (cons (list (car tiles) (car remt))
                      (inner x (cdr remt)))))))))

(define rectangle-area
  (lambda (corner-pair)
    (* (+ (abs (- (caar corner-pair) (caadr corner-pair))) 1)
       (+ (abs (- (cadar corner-pair) (cadadr corner-pair))) 1))))

(define part1
  (lambda (input)
    (car
     (list-sort >
                (map rectangle-area
                     (corner-pairs input))))))

(let ((input (parse-input (file->lines "input"))))
  (display (part1 input)) (newline))
