(import (rnrs) (AoC-2025))

(define-record-type tile (fields x y))

(define-record-type corner-pair
  (fields a b area)
  (protocol
   (lambda (new)
     (lambda (a b)
       (new a b (* (+ (abs (- (tile-x a) (tile-x b))) 1)
                   (+ (abs (- (tile-y a) (tile-y b))) 1)))))))

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
      (map (partial apply make-tile)
           (map (partial map (compose string->number list->string))
                (map separate
                     (map string->list input)))))))

(define corner-pairs
  (lambda (tiles)
    (let outer ((tiles tiles))
      (if (null? (cdr tiles))
          '()
          (let inner ((x (car tiles)) (remt (cdr tiles)))
            (if (null? remt)
                (outer (cdr tiles))
                (cons (make-corner-pair (car tiles) (car remt))
                      (inner x (cdr remt)))))))))

(define part1
  (lambda (input)
    (corner-pair-area
     (car
      (list-sort (lambda (a b)
                   (> (corner-pair-area a) (corner-pair-area b)))
                 (corner-pairs input))))))

(let ((input (parse-input (file->lines "input"))))
  (display (part1 input)) (newline))
