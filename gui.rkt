#lang racket
(require "draw.rkt")
(require "defined.rkt")
(require racket/gui/base)
(require (lib "eopl.ss" "eopl"))

(define treef (new frame% [label "Visualed Tree"][width 800][height 800]))

(define canvasdraw2
  (lambda (canvas dc)
    (program->extractor canvas dc (send textbox get-value))
    (program->extractor canvas dc (send textbox get-value))
    (send dc set-font (make-font #:size 14 #:family 'roman
                             #:weight 'bold))
    (send dc set-text-foreground "Dark Slate Blue")
    (send dc draw-text  (format "~s" (run (send textbox get-value))) 350 60)))

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