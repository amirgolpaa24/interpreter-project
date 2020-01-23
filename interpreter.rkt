#lang racket

(define read-program
  (lambda (path-str)
    (let ([path (string->path path-str)])
      (let ([in (open-input-file path)])
        (let ([program-str (read-string (file-size path) in)])
          (run-program program-str)
          )
        )
      )
    )
  )

(define run-program
  (lambda (program-str)
    (if (null? program-str)
        'Error-null-input
        (if (not (string? program-str))
            'Error-not-a-string-input
            'ok1;(parse-program (string-split (string-normalize-spaces program-str) " " #:repeat? #t))
            )
        )
    )
  )


(define parse-program
  (lambda (tokens)
    (if (not (equal? (car tokens) "{"))
        'Error-invalid-syntax-in-starting-the-program
        (if (not (equal? (last tokens) "}"))
            'Error-invalid-syntax-in-ending-the-program
            'ok2
            )
        )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define list-except-head-and-tail
  (lambda (lst)
    (if (eqv? (length lst) 1)
        'Error-list-length-one
        (reverse (cdr (reverse (cdr lst))))
        )
    )
  )

(define string-except-head-and-tail
  (lambda (str)
    (if (eqv? (string-length str) 1)
        'Error-string-length-one
        (list->string (reverse (list-except-head-and-tail (reverse (string->list str)))))
        )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(read-program "code.txt")







