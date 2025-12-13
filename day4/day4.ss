(import (rnrs) (AoC-2025))

(define INPUT
  (list->vector
   (map list->vector
        (map string->list (file->lines "input")))))

(define MHEIGHT (matrix-height INPUT))
(define MWIDTH (matrix-width INPUT))

(define neighbours-of-element
  (lambda (mat m n)
    (let ((roll (lambda (x) (if (char=? x #\@) 1 0))))
      (list (matrix-ref mat m n)
            (+ (if (or (< (- m 1) 0) (< (- n 1) 0))
                   0
                   (roll (matrix-ref mat (- m 1) (- n 1))))
               (if (< (- m 1) 0)
                   0
                   (roll (matrix-ref mat (- m 1) n)))
               (if (or (< (- m 1) 0) (= (+ n 1) MWIDTH))
                   0
                   (roll (matrix-ref mat (- m 1) (+ n 1))))
               (if (= (+ n 1) MWIDTH)
                   0
                   (roll (matrix-ref mat m (+ n 1))))
               (if (or (= (+ m 1) MHEIGHT) (= (+ n 1) MWIDTH))
                   0
                   (roll (matrix-ref mat (+ m 1) (+ n 1))))
               (if (= (+ m 1) MHEIGHT)
                   0
                   (roll (matrix-ref mat (+ m 1) n)))
               (if (or (= (+ m 1) MHEIGHT) (< (- n 1) 0))
                   0
                   (roll (matrix-ref mat (+ m 1) (- n 1))))
               (if (< (- n 1) 0)
                   0
                   (roll (matrix-ref mat m (- n 1)))))))))

(define neighbours
  (lambda (mat)
    (let ((newmat (make-matrix MHEIGHT MWIDTH)))
      (let ((i #f) (j #f))
        (for ((set! i 0) (< i MHEIGHT) (set! i (+ i 1)))
          (for ((set! j 0) (< j MWIDTH) (set! j (+ j 1)))
            (matrix-set! newmat i j (neighbours-of-element mat i j))))
        newmat))))

(define accessible?
  (lambda (p)
    (and (char=? (car p) #\@) (< (cadr p) 4))))

(define count-trues
  (lambda (mat)
    (let ((c 0) (i #f) (j #f))
      (for ((set! i 0) (< i MHEIGHT) (set! i (+ i 1)))
        (for ((set! j 0) (< j MWIDTH) (set! j (+ j 1)))
          (if (matrix-ref mat i j) (set! c (+ c 1)))))
      c)))

(define count-rolls
  (lambda (mat)
    (count-trues
     (matrix-map (partial char=? #\@)
                 mat))))

(define mark-accessible-and-reset
  (lambda (mat)
    (let ((newmat (make-matrix MHEIGHT MWIDTH)))
      (let ((i #f) (j #f))
        (for ((set! i 0) (< i MHEIGHT) (set! i (+ i 1)))
          (for ((set! j 0) (< j MWIDTH) (set! j (+ j 1)))
            (if (accessible? (matrix-ref mat i j))
                (matrix-set! newmat i j #\X)
                (if (char=? (car (matrix-ref mat i j)) #\@)
                    (matrix-set! newmat i j #\@)
                    (matrix-set! newmat i j #\.)))))
        newmat))))

(define part1
  (lambda (input)
    (count-trues
     (matrix-map accessible? (neighbours input)))))

(define part2
  (lambda (input)
    (let loop ((mat input))
      (let* ((neighbour-mat (neighbours mat))
             (trues (count-trues (matrix-map accessible? neighbour-mat))))
        (if (= trues 0)
            (- (count-rolls input) (count-rolls mat))
            (loop (mark-accessible-and-reset neighbour-mat)))))))

(display (part1 INPUT)) (newline) ; Part1
(display (part2 INPUT)) (newline) ; Part2
