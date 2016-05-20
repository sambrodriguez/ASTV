#lang racket
(require "draw.rkt")
(require "defined.rkt")
(require "stringsplit.rkt")
(require racket/gui/base)
(require (lib "eopl.ss" "eopl"))

(define strh (file->string "help.txt"))

(define treef (new frame% [label "Visualed Tree"][width 800][height 800]))

(define helpf (new frame% [label "Help Documentation"][width 600][height 600]))

(define canvasdraw2
  (lambda (canvas dc)
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
             [label "Help!"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send helpf show #t))])



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