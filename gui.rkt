#lang racket
(require "draw.rkt")
(require "defined.rkt")
(require "stringsplit.rkt")
(require racket/gui/base)
(require (lib "eopl.ss" "eopl"))

(define strh (file->string "help.txt"))
(define strhlist (string-split strh))

(define (for-each func lst)
  (let loop ((rest lst))
    (unless (null? rest)
      (func (car rest))
      (loop (cdr rest)))))

(define treef (new frame% [label "Visualized Tree"][width 800][height 800]))

(define helpf (new frame% [label "Help Documentation"][width 600][height 600]))

(define canvasdraw2
  (lambda (canvas dc)
    (send dc draw-text (send textbox get-value) 350 40)
    (program->extractor canvas dc (send textbox get-value))))




(define start (new frame% [label "Input"] [width 800][height 800]))

(define title-font (make-font #:size 32 #:family 'swiss
                            #:weight 'bold))

(define title (new message% [parent start]
                   [label "Abstract Syntax Tree Visualizer"]
                   [font title-font]))

(define prompt (new message% [parent start]
                          [label "Input expression to be visualized"]))

(define textbox (new text-field% [label "Expression:"] [parent start]))


(define panel (new horizontal-panel% [parent start]
                   [alignment '(center center)]))

(new button% [parent panel]
             [label "Draw Tree"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send treef show #t))])

(new button% [parent panel]
             [label "Grammar"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f6 show #t))])

(new button% [parent panel]
             [label "Help!"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send helpf show #t))])

;Add buttons that spawn images
(define panel2 (new horizontal-panel% [parent start]
                   [alignment '(center center)]))

(new button% [parent panel2]
             [label "x"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f show #t))])

(new button% [parent panel2]
             [label "5"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f7 show #t))])

(new button% [parent panel2]
             [label ">(10,100)"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f2 show #t))])

(new button% [parent panel2]
             [label "let x = 5 in +(x,2)"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f3 show #t))])

(new button% [parent panel2]
             [label "proc(x) +(x,1)"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f4 show #t))])

(new button% [parent panel2]
             [label "cons(1,cons(2,3))"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f5 show #t))])

(define panel3 (new horizontal-panel% [parent start]
                   [alignment '(center center)]))


(new button% [parent panel3]
             [label "if zero?(0) then 1 else 0"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f8 show #t))])

(new button% [parent panel3]
             [label "cond zero?(0) ==> +(1,2) equal?(1,2) ==> 1 end"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f9 show #t))])

(new button% [parent panel3]
             [label "<(10,100)"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f10 show #t))])

(new button% [parent panel3]
             [label "*(10,100)"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f11 show #t))])

(new button% [parent panel3]
             [label "(proc(x) +(1,x) 5)"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send f12 show #t))])


;Create bitmaps from images
(define varBit (read-bitmap "varexp.png"))
(define greatBit (read-bitmap "greaterThanexp.png"))
(define letBit (read-bitmap "letexp.png"))
(define procBit (read-bitmap "procexp.png"))
(define consBit (read-bitmap "consexp.png"))
(define gramBit (read-bitmap "grammar.png"))
(define constBit (read-bitmap "constexp.png"))
(define ifBit (read-bitmap "ifexp.png"))
(define condBit (read-bitmap "condexp.png"))
(define lessBit (read-bitmap "lessthanexp.png"))
(define multBit (read-bitmap "multexp.png"))
(define callBit (read-bitmap "callexp.png"))
;(define bitmap (read-bitmap "/tmp/red-arrow.bmp"))

(define f (new frame% [label "Bitmap"]))
(new message% [parent f] [label varBit])
;(send f show #t)

(define f2 (new frame% [label "Bitmap"]))
(new message% [parent f2] [label greatBit])

(define f3 (new frame% [label "Bitmap"]))
(new message% [parent f3] [label letBit])

(define f4 (new frame% [label "Bitmap"]))
(new message% [parent f4] [label procBit])

(define f5 (new frame% [label "Bitmap"]))
(new message% [parent f5] [label consBit])

(define f6 (new frame% [label "Bitmap"]))
(new message% [parent f6] [label gramBit])

(define f7 (new frame% [label "Bitmap"]))
(new message% [parent f7] [label constBit])

(define f8 (new frame% [label "Bitmap"]))
(new message% [parent f8] [label ifBit])

(define f9 (new frame% [label "Bitmap"]))
(new message% [parent f9] [label condBit])

(define f10 (new frame% [label "Bitmap"]))
(new message% [parent f10] [label lessBit])

(define f11 (new frame% [label "Bitmap"]))
(new message% [parent f11] [label multBit])

(define f12 (new frame% [label "Bitmap"]))
(new message% [parent f12] [label callBit])

#|(define helpc (new canvas% [parent helpf]
     ;[paint-callback helpdraw]
     ))|#

(define drawhelp
  (lambda (canvas dc)
               (send dc draw-text strh 0 0)))

(define helptext (new canvas% [parent helpf]
                          [paint-callback drawhelp]))

(define treec (new canvas% [parent treef]
     [paint-callback canvasdraw2]))



(send start show #t)