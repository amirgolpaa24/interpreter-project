#lang racket
(require eopl)

(define scanner
  '([white-sp (whitespace) skip]
    [double-qoute ("\"") skip]
    [string((or letter "/" ".") (arbno (or letter "(" ")" "," "_" "/"))) symbol]
    [integer (digit (arbno digit)) number]
    [operator ("*") symbol]
    [operator ("+") symbol]))

(define grammar
  '([program (object) a-program]
    [object ("{" (separated-list pair ",")"}") an-object]
    [pair (string ":" value) a-pair]
    [value (string) str-val]
    [value (integer) int-val]
    [value (list) lst-val]
    [value (object) obj-val]
    [list ("[" (separated-list string operator) "]") a-lst]))

(sllgen:make-define-datatypes scanner grammar)

(define scan&parse
  (sllgen:make-string-parser scanner grammar))

(define read-program
  (lambda (path-str)
    (let ([path (string->path path-str)])
      (let ([in (open-input-file path)])
        (let ([program-str (read-string (file-size path) in)])
          (scan&parse program-str)
          )
        )
      )
    )
  )

(read-program "code.txt")