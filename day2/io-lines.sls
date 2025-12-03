(library (io-lines)
  (export file->lines lines->file)
  (import (rnrs))
  (define file->lines
    (lambda (filename)
      (call-with-input-file filename
        (lambda (port)
          (define read-line
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
                                    (next-char (read-char port)))))))))))
          (let next-line ((current-line (read-line)))
            (if current-line
                (cons current-line
                      (next-line (read-line)))
                '()))))))
  (define lines->file
    (lambda (lines filename)
      (call-with-output-file filename
        (lambda (port)
          (define write-line
            (lambda (str)
              (let write-next-char ((char-list (string->list str)))
                (if (null? char-list)
                    (newline port)
                    (begin
                      (write-char (car char-list) port)
                      (write-next-char (cdr char-list)))))))
          (let next-line ((remaining-lines lines))
            (if (not (null? remaining-lines))
                (begin
                  (write-line (car remaining-lines))
                  (next-line (cdr remaining-lines))))))))))
