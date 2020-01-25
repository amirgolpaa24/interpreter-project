#lang racket

(define init-env
  (lambda ()
    '()
    )
  )

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
            (if (non-empty-string? program-str)
                (evaluate 'program-exp (parse-program (string-normalize-spaces program-str)) (init-env))
                'Error-empty-program
                )
            )
        )
    )
  )


(define parse-program
  (lambda (program)
    (parse-object program)
    )
  )


(define parse-object
  (lambda (object)
    (let ((content (substring object 2 (- (string-length object) 2))))
      (define pairs '())
      (parse-pairs (split-pairs-in-content content pairs))
      )
    )
  )

(define split-pairs-in-content
  (lambda (content pairs)
    (let ((end (get-index-of-value-end content)))
      (begin
        (set! pairs (append
                     (if (< (+ 4 end) (string-length content))
                         (split-pairs-in-content (substring content (+ end 4)) pairs)
                         '()
                         )
                     (list (substring content 0 (+ end 1)))))
        pairs
        )
      )
    )
  )

(define parse-pairs
  (lambda (pairs-list)
    (if (null? pairs-list)
        '()
        (cons
         (parse-pair (car pairs-list))
         (parse-pairs (cdr pairs-list))
         )
        )
    )
  )

(define parse-pair
  (lambda (pair)
    (let ((index (getPosition pair #\:)))
      (cons
       (string-trim (substring pair 0 (- index 1)) "\"")
       (let ((value (substring pair (+ index 2))))
         (cond
           ((string-prefix? value "{")
            (list (parse-object value)))
           ((string-prefix? value "[")
            (list (parse-list value)))
           (else
            (if (string-prefix? value "\"")
                (list (substring value 1 (- (string-length value) 1)))
                (list value)
                ))
           )
         )
       )
      )
    )
  )

(define parse-list
  (lambda (list)
    (let ((content (substring list 1 (- (string-length list) 1))))
      (define terms '())
      (parse-terms content))
    )
  )


(define parse-terms
  (lambda (content)
    (map
     (lambda (s)
       (if (string-prefix? s "\"")
           (substring s 1 (- (string-length s) 1))
           s
           )
       )
     (string-split content " ")
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define evaluate
  (lambda (type exp env)
    (cond
      ((eqv? type 'program-exp)
       (evaluate 'object-exp exp env)
       )

      ((eqv? type 'size-exp)
       (string->number exp)
       )

      ((eqv? type 'size-exp)
       (string->number exp)
       )

      ((eqv? type 'object-exp)
       (begin
         (let ((size-val (get-value-of-key exp "size"))
               (assignment-val (get-value-of-key exp "assignment"))
               (docs-val (get-value-of-key exp "docs"))
               (query-val (get-value-of-key exp "query")))
           
           (begin
             (if (not (null? size-val))
                 (evaluate 'size-exp size-val env)
                 '()
                 )
             (if (not (null? assignment-val))
                 (evaluate 'assignment-exp assignment-val env)
                 '()
                 )
             (if (not (null? docs-val))
                 (evaluate 'docs-exp docs-val env)
                 '()
                 )
             (if (not (null? (find-function exp)))
                 (let ((func-names-list (find-function exp)))
                   (for ([func-name func-names-list])
                     (evaluate 'function (cons func-name (get-value-of-key exp func-name)) env))
                   )
                 '()
                 )
             (if (not (null? query-val))
                 (evaluate 'query-exp query-val env)
                 '()
                 )
             )
           )
           
         )
       )
      
      (else
       '())
      )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require srfi/1) 
(define (getPosition string char)
  (list-index (curry char=? char) 
              (string->list string)))

(define get-index-of-value-end
  (lambda (str)
    (get-index-of-value-end-aux str 0 0)
    )
  )

(define get-index-of-value-end-aux
  (lambda (str n i)
    (if (< i (string-length str))
        (if (and (equal? (string-ref str i) #\,) (eqv? n 0))
            (- i 2)
            (if (equal? (string-ref str i) #\{)
                (get-index-of-value-end-aux str (+ n 1) (+ i 1))
                (if (equal? (string-ref str i) #\})
                    (get-index-of-value-end-aux str (- n 1) (+ i 1))
                    (get-index-of-value-end-aux str n (+ i 1))
                    )
                )
            )
        (- (string-length str) 1)
        )
    )
  )
                

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

(define get-value-of-key
  (lambda (dictionary key)
    (if (null? dictionary)
        '()
        (if (equal? (caar dictionary) key)
            (car (cdar dictionary))
            (get-value-of-key (cdr dictionary) key)
            )
        )
    )
  )

(define find-function
  (lambda (dictionary)
    (flatten
     (map
      (lambda (p) (if (not (member (car p) '("size" "assignment" "docs" "query")))  (car p)  '()))
      dictionary)
     )
    )
  )
  
  


  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(read-program "code.txt")







