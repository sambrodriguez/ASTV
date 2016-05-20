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

;(define helpf (new frame% [label "Help Documentation"][width 600][height 600]))

(define canvasdraw2
  (lambda (canvas dc)
     (send dc set-font (make-font #:size 14 #:family 'roman
                             #:weight 'bold))
    (send dc set-text-foreground "Dark Slate Blue")
    (send dc draw-text  (format "~s" (run (send textbox get-value))) 350 60)
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

(new button% [parent panel]
             [label "Help!"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send tutFrame show #t))])


(define tutFrame (new frame% [label "Tutorial"] [width 800][height 300]))

(define tutTitle (new message% [parent tutFrame]
                          [label "Welcome to the Abstract Syntax Tree Visualizer!"]))

(define tutTitle2 (new message% [parent tutFrame]
                          [label ""]))

(define tutTextWelcome (new message% [parent tutFrame]
                          [label "This is the program where you get to visualize abstract syntax trees in exchange for entering the concrete syntax into the textbox, and pressing the Draw Tree button. "]))

(define tutTextWelcome2 (new message% [parent tutFrame]
                             [label "Our program currently supports the LET language from the book, as well as the Proc language from EOPL."]))

(define tutTextWelcome3 (new message% [parent tutFrame]
                             [label "For example, try entering the following into the textbox, without quotes: let x = 5 in -(x,3)"]))

(define tutTextWelcome4 (new message% [parent tutFrame]
                             [label "If you entered it correctly, you will see a tree! Congratulations! You have successfully run the program."]))

(define tutTextWelcome8 (new message% [parent tutFrame]
                             [label "Notice that in addition to the tree, there is also black text that shows the result of evaluation of your entered program"]))

(define tutTextWelcome5 (new message% [parent tutFrame]
                             [label "Now that you know how to run the program, try a few harder examples. Here are a few to choose from:"]))

(define tutTextWelcome6 (new message% [parent tutFrame]
                             [label "let f = proc(x) +(x,5) in (f 6)"]))

(define tutTextWelcome7 (new message% [parent tutFrame]
                             [label "(proc(x) +(x,5) 10)"]))

(define tutTextWelcome9 (new message% [parent tutFrame]
                             [label "Best of luck to you, and thank you for using our program!"]))

(define treec (new canvas% [parent treef]
     [paint-callback canvasdraw2]))



(send start show #t)