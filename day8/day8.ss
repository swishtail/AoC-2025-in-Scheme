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
           (map (partial (flip separate) #\,)
                (map string->list input))))))

(define pair-first (lambda (p) (car p)))
(define pair-second (lambda (p) (cadr p)))

(define sorted-pairs
  (lambda (l)
    (let* ((distance
            (lambda (x1 x2)
              (sqrt (+ (expt (- (car x1) (car x2)) 2)
                       (expt (- (cadr x1) (cadr x2)) 2)
                       (expt (- (caddr x1) (caddr x2)) 2)))))
           (make-pair
            (lambda (box1 box2)
              (list box1 box2 (distance box1 box2)))))
      (list-sort (lambda (a b) (< (caddr a) (caddr b)))
                 (let outer ((l l))
                   (if (null? (cdr l))
                       '()
                       (let inner ((x (car l)) (rem (cdr l)))
                         (if (null? rem)
                             (outer (cdr l))
                             (cons (make-pair x (car rem))
                                   (inner x (cdr rem)))))))))))

(define add-pair-to-circuits
  (lambda (pair circuits)
    (letrec ((belongs
              (lambda (box circuits)
                (if (null? circuits)
                    #f
                    (if (member box (car circuits))
                        (car circuits)
                        (belongs box (cdr circuits)))))))
      (let ((a (pair-first pair)) (b (pair-second pair)))
        (let ((a-circuit (belongs a circuits))
              (b-circuit (belongs b circuits)))
          (cond (a-circuit
                 (if (equal? b-circuit a-circuit)
                     circuits
                     (if b-circuit
                         (cons (append a-circuit b-circuit)
                               (remove b-circuit
                                       (remove a-circuit circuits)))
                         (cons (cons b a-circuit)
                               (remove a-circuit circuits)))))
                (b-circuit
                 (if (equal? a-circuit b-circuit)
                     circuits
                     (if a-circuit
                         (cons (append b-circuit a-circuit)
                               (remove a-circuit
                                       (remove b-circuit circuits)))
                         (cons (cons a b-circuit)
                               (remove b-circuit circuits)))))
                (else
                 (cons (list a b) circuits))))))))

(define connect-pairs
  (lambda (boxes m)
    (let ((sort-circuits
           (lambda (circuits)
             (list-sort (lambda (a b) (> (length a) (length b)))
                        circuits))))
      (if (eq? m 'last-pair)
          (let loop ((circuits (map list boxes))
                     (pairs (sorted-pairs boxes))
                     (previous-pair #f))
            (if (= (length circuits) 1)
                previous-pair
                (loop (add-pair-to-circuits (car pairs) circuits)
                      (cdr pairs)
                      (car pairs))))
          (let loop ((circuits '()) (pairs (sorted-pairs boxes)) (i m))
            (if (= i 0)
                (sort-circuits circuits)
                (loop (add-pair-to-circuits (car pairs) circuits)
                      (cdr pairs)
                      (- i 1))))))))

(let ((input (parse-input (file->lines "input"))))
  (let ((final-circuits (connect-pairs input 1000)))
    ;; Part1
    (display
     (* (length (car final-circuits))
        (length (cadr final-circuits))
        (length (caddr final-circuits)))))
  (newline)
  (let ((last-pair (connect-pairs input 'last-pair)))
    ;; Part2
    (display
     (* (car (pair-first last-pair))
        (car (pair-second last-pair)))))
  (newline))
