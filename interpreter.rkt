#lang racket

(define init-env
  (lambda ()
    '()
    )
  )

(define extend-env
  (lambda (type name value env)
    (cons (list type name value) env)
    )
  )

(define extend-env-multiple-vars
  (lambda (extra-env env)
    (if (null? extra-env)
        env
        (extend-env-multiple-vars (cdr extra-env) (extend-env 'var (caar extra-env) (cadar extra-env) env))
        )
    )
  )

(define extend-env-multiple-funcs
  (lambda (extra-env env)
    (if (null? extra-env)
        env
        (extend-env-multiple-funcs (cdr extra-env) (extend-env 'func (caar extra-env) (cadar extra-env) env))
        )
    )
  )

(define apply-env
  (lambda (env type name)
    (if (null? env)
        '()
        (let ((first-env-entry (car env)))
          (if (and (eqv? (list-ref first-env-entry 0) type) (equal? (list-ref first-env-entry 1) name))
              (list-ref first-env-entry 2)
              (apply-env (cdr env) type name)
              )
          )
        )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (let ((index (get-position pair #\:)))
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

      ((eqv? type 'object-exp)
       (let ((size-val (get-value-of-key exp "size"))
             (assignment-val (get-value-of-key exp "assignment"))
             (docs-val (get-value-of-key exp "docs"))
             (query-val (get-value-of-key exp "query")))
           
         (let ((size (if (not (null? size-val))
                         (string->number size-val)
                         '()
                         )))
           (let ((assignments assignment-val))
             (let ((docs docs-val))
               (let ((functions '()))
                 (begin
                   (if (not (null? (find-function exp)))
                       (let ((func-names-list (find-function exp)))
                         (for ([func-name func-names-list])
                           (if (non-empty-string? func-name)
                               (set! functions
                                     (cons (cons func-name (list (merge-all-inputs (get-value-of-key exp func-name)))) functions))
                               (set! functions functions)
                               ))
                         )
                       '()
                       )
                   (let ((query query-val))

                     ;(display size)
                     ;(newline)
                     ;(display assignments)
                     ;(newline)
                     ;(display functions)
                     ;(newline)
                     ;(display docs)
                     ;(newline)
                     ;(display query)
                     ;(newline)(newline)

                     (let ((extra-env (evaluate-assignments-list assignments env functions assignments)))
                       (let ((new-env (extend-env-multiple-vars extra-env env)))
                         (let ((new-env-with-funcs (extend-env-multiple-funcs (add-env-to-functions functions new-env) new-env)))
                           (let ((docs-answer (evaluate-docs docs new-env-with-funcs)))
                             (evaluate-query query new-env-with-funcs docs-answer)
                             )
                           )
                         )
                       )
                     
                   
                     )
                   ))))))
       )
      (else
       '())
      )
    )
  )

(define evaluate-query
  (lambda (query env files)
    (let ((search-query (evaluate-assignment "" '() query env '() '())))
      (execute search-query files)
      )
    )
  )

;;;;;;;;;;;;; main execution ;;;;;;;;;;;;;;;;

(define execute
  (lambda (search-query files)
    files
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define evaluate-docs
  (lambda (docs env)
    (if (string? docs)
        (flatten
         (map
          (lambda (path)
            (let ([path-name (path->string path)])
              (if (string-suffix? path-name ".txt")
                  (string-append docs path-name)
                  '()
                  )
              )
            )
          (directory-list docs)
          )
         )

        ;--- if it is a sub-program:
        (evaluate 'program-exp docs env)
        )
        
    )
  )
 

(define add-env-to-functions
  (lambda (functions env)
    (if (null? functions)
        '()
        (cons
         (list (caar functions) (append (cadar functions) (list (list "env" env))))
         (add-env-to-functions (cdr functions) env))
        )
    )
  )

(define merge-all-inputs
  (lambda (function)
    (define inputs-list '())
    (begin
      (for ([i (cdr function)])
        (let ((input-name (get-value-of-key function "input")))
          (begin
            (set! inputs-list (cons input-name inputs-list))
            (set! function (remove-key function "input"))
            )
          )
        )
      (list (list "inputs" inputs-list) (list "body" (get-value-of-key function "body")))
      )
    )
  )

(define evaluate-assignments-list
  (lambda (assignments-list env functions assignments)
    (if (null? assignments-list)
        '()
        (let ((var (caar assignments-list))
              (exp (cadar assignments-list)))
      
          (cons
           (list var (evaluate-assignment var '() exp env functions assignments))
           (evaluate-assignments-list (cdr assignments-list) env functions assignments)
           )
          )
        )
    )
  )

(define evaluate-assignment
  (lambda (var-name inputs exp env functions assignments)
    (cond
      ((string-contains?  exp "(")
       (let ((func-name (substring exp 0 (get-position exp #\()))
             (func-args-raw (extract-argumants (substring exp (+ (get-position exp #\() 1) (- (string-length exp) 1)))))
         (let ((func-args (map (lambda (arg-raw) (evaluate-assignment var-name inputs arg-raw  env functions assignments)) func-args-raw)))
           (evaluate-function-call var-name func-name func-args-raw env functions assignments)
           )
         ))

      ((not (null? (get-value-of-key inputs exp)))
       (display "-----> evaluating function input : \"")(display exp)(display "\" to ==> \"")
       (display (get-value-of-key inputs exp))(display "\"")(newline)
       (evaluate-assignment var-name inputs (get-value-of-key inputs exp) env functions assignments))

      ((not (null? (get-value-of-key assignments exp)))
       (if (not (equal? exp var-name))
           (evaluate-assignment exp '() (get-value-of-key assignments exp) env functions assignments)
           exp
           )
       )
      
      ((not (null? (apply-env env 'var exp)))
       (if (not (equal? exp var-name))
           (apply-env env 'var exp)
           exp
           ))
      
      (else exp)
      )
    )
  )
  

(define extract-argumants
  (lambda (str)
    (if (zero? (string-length str))
        '()
        (let ((last-comma-index (go-back-until-comma str 0 (- (string-length str) 1))))
          (let ((cut-index (if (equal? (string-ref str last-comma-index) #\,) (+ last-comma-index 1) last-comma-index)))
            (let ((last-arg (substring str cut-index)))
              (append (extract-argumants (substring str 0 last-comma-index)) (list last-arg)))))
        )
    )
  )

(define go-back-until-comma
  (lambda (str n i)
    (if (and (eqv? n 0) (or (equal? (string-ref str i) #\,) (zero? i)))
        i 
        (if (equal? (string-ref str i) #\))
            (go-back-until-comma str (+ n 1) (- i 1))
            (if (equal? (string-ref str i) #\()
                (go-back-until-comma str (- n 1) (- i 1))
                (go-back-until-comma str n (- i 1))
                )
            )
        )
    )
  )

(define evaluate-function-call
  (lambda (assignment-var-name func-name func-args env functions assignments)
    (if (not (null? (get-value-of-key functions func-name)))
        (let ((function-without-env (get-value-of-key functions func-name)))
          (apply-function assignment-var-name function-without-env env functions assignments func-args)
          )
        (let ((function-with-env (apply-env env 'func func-name)))
          (apply-upper-function function-with-env func-args)
          )
        )
    )
  )

(define apply-upper-function
  (lambda (function args)
    (let ((env (get-value-of-key function "env")))

      ;(newline)(display function)(newline)(display args)(newline)(newline)

      (apply-function "" (remove-key function "env") env '() '() args)
      )
    )
  )

(define apply-function
  (lambda (assignment-var-name function-without-env env functions assignments args)
    (let ((body (get-value-of-key function-without-env "body"))
          (inputs (get-value-of-key function-without-env "inputs")))

      ;(newline)(display args)(newline)(display inputs)(newline)(display body)(newline)(newline)

      (let ((str-exps (extract-even-indices body))
            (operators (extract-even-indices (cdr body)))
            (inputs-args (join-args-with-inputs inputs args)))
        (let ((operands (map (lambda (exp) (evaluate-assignment assignment-var-name inputs-args exp env functions assignments)) str-exps)))

          (get-function-answer operands operators)
          
          )
        )
      )
    )
  )

(define get-function-answer
  (lambda (operands operators)
    (if (null? operators)
        operands
        (if (eqv? (length operators) 1)
            (cons (car operators) operands)
            (list (list-tail operators) (get-function-answer (list-except-tail operands) (list-except-tail operators)) (list-tail operands))
            )
        )
    )
  )

(define join-args-with-inputs
  (lambda (inputs args)
    (if (null? inputs)
        '()
        (cons (list (car inputs) (car args)) (join-args-with-inputs (cdr inputs) (cdr args)))
        )
    )
  )

(define extract-even-indices
  (lambda (lst)
    (if (null? lst)
        '()
        (if (eqv? (length lst) 1)
            lst
            (if (eqv? (length lst) 2)
                (list (car lst))
                (cons (car lst) (extract-even-indices (cddr lst)))
                )
            )
        )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require srfi/1) 
(define (get-position string char)
  (list-index (curry char=? char) 
              (string->list string)))

(define string-contains?
  (lambda (str search-str)
    (if (zero? (string-length str))
        #f
        (or (string-prefix? str search-str) (string-contains? (substring str 1) search-str))
        )
    )
  )

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
                    (if (equal? (string-ref str i) #\()
                        (get-index-of-value-end-aux str (+ n 1) (+ i 1))
                        (if (equal? (string-ref str i) #\))
                            (get-index-of-value-end-aux str (- n 1) (+ i 1))
                            (get-index-of-value-end-aux str n (+ i 1))
                            )
                        )
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

(define list-except-tail
  (lambda (lst)
    (if (eqv? (length lst) 1)
        '()
        (reverse (cdr (reverse lst)))
        )
    )
  )

(define list-tail
  (lambda (lst)
    (if (eqv? (length lst) 1)
        (car lst)
        (car (reverse lst))
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

(define remove-key
  (lambda (dictionary key)
    (if (null? dictionary)
        dictionary
        (if (equal? (caar dictionary) key)
            (cdr dictionary)
            (cons (car dictionary) (remove-key (cdr dictionary) key))
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







