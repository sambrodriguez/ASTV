#lang racket

;(define strh (file->string "help.txt"))
 
;(define expression "19 2.14 + 4.5 2 4.3 / - *")

(define (string-split str)

  (define (char->string c)
    (make-string 1 c))

  (define (string-first-char str)
    (string-ref str 0))

  (define (string-first str)
    (char->string (string-ref str 0)))

  (define (string-rest str)
    (substring str 1 (string-length str)))

  (define (string-split-helper str chunk lst)
  (cond 
    [(string=? str "") (reverse (cons chunk lst))]
    [else
     (cond
       [(char=? (string-first-char str) #\newline) (string-split-helper (string-rest str) "" (cons chunk lst))]
       [else
        (string-split-helper (string-rest str) (string-append chunk (string-first str)) lst)]
       )
     ]
    )
  )

  (string-split-helper str "" empty)
  )

(provide (all-defined-out))
