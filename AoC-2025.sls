(library (AoC-2025)
  
  (export file->lines
          
          make-matrix
          matrix-ref
          matrix-height
          matrix-width
          matrix-set!
          matrix-map
          
          partial
          flip
          compose
          
          for)
  
  (import (rnrs))
  
  (define file->lines
    (lambda (filename)
      (call-with-input-file filename
        (lambda (port)
          (letrec ((read-line
                    (lambda ()
                      (let ((first-char (peek-char port)))
                        (if (eof-object? first-char)
                            #f
                            (list->string
                             (let next-char ((current-char (read-char port)))
                               (cond ((eof-object? current-char) '())
                                     ((char=? current-char #\newline) '())
                                     (else
                                      (cons current-char
                                            (next-char (read-char port))))))))))))
            (let next-line ((current-line (read-line)))
              (if current-line
                  (cons current-line
                        (next-line (read-line)))
                  '())))))))
  
  (define make-matrix
    (lambda (m n . fill)
      (let ((newv (make-vector m)))
        (vector-map (lambda (x)
                      (make-vector n
                                   (if (null? fill)
                                       0
                                       (car fill))))
                    newv))))
  
  (define matrix-ref
    (lambda (v m n)
      (vector-ref (vector-ref v m) n)))
  
  (define matrix-height
    (lambda (m)
      (vector-length m)))
  
  (define matrix-width
    (lambda (m)
      (vector-length (vector-ref m 0))))
  
  (define matrix-set!
    (lambda (v m n val)
      (let ((newv (vector-ref v m)))
        (vector-set! newv n val)
        (vector-set! v m newv))))
  
  (define matrix-map
    (lambda (f v)
      (vector-map (lambda (x)
                    (vector-map f x))
                  v)))

  (define partial
    (lambda (proc . args)
      (lambda new-args
        (apply proc (append args new-args)))))
  
  (define flip
    (lambda (proc)
      (lambda (a b)
        (proc b a))))
  
  (define compose
    (lambda p
      (lambda x
        (let next-proc ((procs p))
          (if (null? (cdr procs))
              (apply (car procs) x)
              ((car procs) (next-proc (cdr procs))))))))
  
  (define compose-apply
    (lambda i
      (let next-item ((items i))
        (if (null? (cdr items))
            (car items)
            ((car items) (next-item (cdr items)))))))
  
  (define-syntax for
    (syntax-rules ()
      ((for (init pred tail) exps ...)
       (let ()
         init
         (let for-loop ()
           (if pred
               (begin exps ... tail (for-loop)))))))))
