#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define error-line-number 0)

(define scan
  (lexer
   ["write"
    ; =>
    (cons '"write"
          (scan input-port))]
   ["read"
    ; =>
    (cons '"read" 
          (scan input-port))]
   [(:: (:or (char-range #\a #\z) (char-range #\A #\Z)) (:* (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9))))
    ; =>
    (cons `"id"
          (scan input-port))]
   [#\( 
    ; =>
    (cons '"LPAR"
          (scan input-port))]
   [#\)
    ; =>
    (cons '"RPAR" 
          (scan input-port))]
   [(:+ (:or "." (char-range #\0 #\9)))
    ; =>
    (cons `"lit"
          (scan input-port))]
   [#\+
    ; =>
    (cons `"+"
          (scan input-port))]
   [#\-
    ; =>
    (cons `"-"
          (scan input-port))]
   [#\/
    ; =>
    (cons `"/"
          (scan input-port))]
   [#\*
    ; =>
    (cons `"*"
          (scan input-port))]
   ["$$"
    ; =>
    (cons `"$$"
          (scan input-port))]
   [":="
    ; =>
    (cons `":="
          (scan input-port))]
   [whitespace 
    ; =>
    (scan input-port)]
   [(eof)
    '()]))

(define x 0)

(define (program lst)
  (cond
    [(or (string=? (car lst) "id")
         (string=? (car lst) "read")
         (string=? (car lst) "write"))
     (stmt-list lst)]
    [(string=? (car lst) "$$")
     lst]
    [else
     "error in program"]))

(define (stmt-list lst)
  (cond
    [(or (string=? (car lst) "id")
         (string=? (car lst) "read")
         (string=? (car lst) "write"))
     (set! error-line-number (add1 error-line-number))
     (stmt  lst)]
    [(string=? (car lst) "$$")
     (program lst)]
    [else
     "error in stmt-list"]))

(define (stmt lst)
  (cond
    [(string=? (car lst) "read")
     (cons (car lst) (readID (rest lst)))]
    [(string=? (car lst) "id")
     (cons (car lst) (ID (rest lst)))]
    [(string=? (car lst) "write")
     (cons (car lst) (expr (rest lst)))]
    [else
     "error in stmt"]))

(define (readID lst)
  (cond
    [(string=? (car lst) "id")
     (cons (car lst) (stmt-list (rest lst)))]
    [else
     "error in readID"]))

(define (ID lst)
  (cond
    [(string=? (car lst) ":=")
     (cons (car lst) (expr (rest lst)))]
    [else
     (set! error-line-number(sub1 error-line-number))
     "error in id"]))

(define (expr lst)
  (cond
    [(or (string=? (car lst) "id")
         (string=? (car lst) "lit")
         (string=? (car lst) "LPAR"))
     (term lst)]
    [(string=? (car lst) "$$")
     (stmt-list lst)]
    [else
     "error in expr"]))

(define (term lst)
  (cond
    [(or (string=? (car lst) "id")
         (string=? (car lst) "lit")
         (string=? (car lst) "LPAR"))
     (factor lst)]
    [else
     "eror in term"]))

(define (term-tail lst)
  (cond
    [(or (string=? (car lst) "+")
         (string=? (car lst) "-"))
     (add-op lst)]
    [(and (or
           (string=? (car lst) "id")
           (string=? (car lst) "read")
           (string=? (car lst) "write")
           (string=? (car lst) "$$"))
          (= x 0))
     (stmt-list lst)]
    [else
     "error in term-tail"]))


(define (add-op lst)
  (cond
    [(string=? (car lst) "+")
     (cons (car lst) (term (rest lst)))]
    [(string=? (car lst) "-")
     (cons (car lst) (term (rest lst)))]))


(define (factor lst)
  (cond
    [(string=? (car lst) "id")
     (cons (car lst) (factor-tail (rest lst)))]
    [(string=? (car lst) "lit")
     (cons (car lst) (factor-tail (rest lst)))]
    [(string=? (car lst) "LPAR")
     (set! x (add1 x))
     (cons (car lst) (expr (rest lst)))]
    [else
     "eror in factor"]))

(define (factor-tail lst)
  (cond
    [(or (string=? (car lst) "*")
         (string=? (car lst) "/"))
     (mult-op lst)]
    [(and (string=? (car lst) "RPAR")
          (> x 0))
     (set! x (sub1 x))
     (cons (car lst) (factor-tail (rest lst)))]
    [(or (string=? (car lst) "+")
         (string=? (car lst) "-")
         (string=? (car lst) "id")
         (string=? (car lst) "read")
         (string=? (car lst) "write")
         (string=? (car lst) "$$"))
     (term-tail lst)]
    [else
     "error in factor-tail"]))

(define (mult-op lst)
  (cond
    [(string=? (car lst) "*")
     (cons (car lst) (factor (rest lst)))]
    [(string=? (car lst) "/")
     (cons (car lst) (factor (rest lst)))]))

(define (parse input)
  (set! error-line-number 0)
  (set! x 0)
  (define lst (scan (open-input-file input)))
  (define l (program lst))
  (cond
    [(equal? l lst)
     (display "Accept \n\n")]
    [else
     (display (string-append "Syntax error found on line " (~a error-line-number) "\n"))
     (define x 0)
     (with-input-from-file (~a input)
       (thunk 
        (for ([line (in-lines)])
          (set! x(add1 x))
          (cond
            [(= x error-line-number)
             (displayln line)])
          )
        ))
     (display "\n")])
  )
 
(parse "input01.txt")
(parse "input02.txt")
(parse "input03.txt")
(parse "input04.txt")
(parse "input05.txt")
(parse "test.txt")