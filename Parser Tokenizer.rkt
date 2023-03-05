
#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)

(define (tokenize input)
  (regexp-match* #rx"[0-9]+|[-+*/()]|[a-zA-Z]+" input))

(define (operator? str)
    (cond 
       [(equal? str "(") 'lparen]
       [(equal? str ")") 'rparen]
       [(equal? str "+") 'plus] 
       [(equal? str "-") 'minus] 
       [(equal? str "=") 'equals]
       [(equal? str ":") 'colon] ))

(define (string-alphabeticc? str)
    (cond 
       [(equal? str "goto") 'goto]
            [(equal? str "return") 'return]
            [(equal? str "write") 'write]
            [(equal? str "read") 'read]
            [(equal? str "gosub") 'gosub]
            [(equal? str "if") 'if]
            [(equal? str "then") 'then] ))

(define (string-alphabetic? str)
   (define (iter ch-list)
       (if (empty? ch-list)
          (cond
            [(equal? str "goto") 'goto]
            [(equal? str "return") 'return]
            [(equal? str "write") 'write]
            [(equal? str "read") 'read]
            [(equal? str "gosub") 'gosub]
            [(equal? str "if") 'if]
            [(equal? str "then") 'then]
          ); made it to the end, no non-alphas
          (if (char-alphabetic? (first ch-list))
              (iter (rest ch-list))
              (void)))) ; found non-alpha, bail out
     ;function body (1 line, sets up call to iterator function) 
     (iter (string->list str)))

(define (id? str)
  (and (= (string-length str) 1)
           (char-alphabetic? (string-ref str 0))
      'id))

(define (nonzero_digit? x)
  (and (number? x) (>= x 1) (<= x 9)))

(define (digit? x)
  (or (and (number? x) (= x 0)) (and (nonzero_digit? x))))

(define (line input)
  (let ((label (car input))
        (tokens (map or-3 (cdr input))))
    (stmt(tokens))))

(define (stmt input)
  (cond ((and (eq? (second input) 'id)
              (eq? (third input) 'equals))
         (expr (rest (rest input))))
        ))

(define (expr input)
  (cond ((eq? (car input) 'id) (etail (rest input)))
        ((eq? (car input) 'num) (etail (rest input)))
        ))
  
;split a string into a list of string, check first if nonzero_digit?, use map to check the rest is digit?
(define (idx? str)
  (let ((lst (map (lambda (c) (string c)) (string->list str))))
    (and (nonzero_digit? (string->number (first lst)))
         (map digit? (rest lst))
         'idx)))

;somehow this function only works when id?, idx?, and operator? function place in this order
(define (or-3 input)
    (or (id? input)
        (idx? input)
        (operator? input)
        ))

;updated from or-3 with string-alphabetic? function [not working]
(define (or-2 input)
  (or
   (id? input)
   (idx? input)
   (operator? input)
   (string-alphabeticc? input)
        ))

(define (scanner tokens)
  (map or-3 tokens))

; Read text file using port -> lines
(define (read-file file-path)
  (let ((port (open-input-file file-path)))
    (let ((lines (port->lines port)))
      (close-input-port port)
      lines)))

(define file-contents (read-file "example.txt"))

;split text lines by whitespace
(define split-contents (map string-split file-contents))

(displayln split-contents)

