#lang racket
(require "draw.rkt")
(require "defined.rkt")
(require racket/gui/base)
(require (lib "eopl.ss" "eopl"))



(define treef (new frame% [label "Visualed Tree"][width 800][height 800]))

(define canvasdraw2
  (lambda (canvas dc)
    (program->extractor canvas dc (send textbox get-value))))

(define start (new frame% [label "Input"] [width 800][height 800]))

(define title (new message% [parent start]
                          [label "Input expression to be visualized"]))

(define textbox (new text-field% [label "expression"] [parent start]))

(new button% [parent start]
             [label "Draw Tree"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send treef show #t))])
                    

(define treec (new canvas% [parent treef]
     [paint-callback canvasdraw2]))

(send start show #t)

;Tutorial Window

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

(new button% [parent start]
             [label "Help"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send tutFrame show #t))])