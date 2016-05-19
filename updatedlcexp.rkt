#lang racket

(require (lib "eopl.ss" "eopl"))
(define parse-lcexp
  (lambda (v)
    (cond [(symbol? v) (var-exp v)]
          [(eq? (car v) 'lambda) (lambda-exp (caadr v) (parse-lcexp (caddr v)))]
          [else (app-exp (parse-lcexp (car v)) (parse-lcexp (cadr v)))])))

(define-datatype lcexp lcexp?
  (var-exp (var symbol?))
  (lambda-exp (param symbol?) (body lcexp?))
  (app-exp (rator lcexp?) (rand lcexp?)))

(define var-exp?
  (lambda (e)
    (symbol? e)))
(define lambda-exp?
  (lambda (e)
    (and (pair? e) (eq? (car e) 'lambda))))
(define app-exp?
  (lambda (e)
    (and (pair? e) (not (eq? (car e) 'lambda)))))
(define var-exp->var
  (lambda (e)
    e))
(define lambda-exp->param
  (lambda (e)
    (caadr e)))
(define lambda-exp->body
  (lambda (e)
    (caddr e)))
(define app-exp->rator
  (lambda (e)
    (car e)))
(define app-exp->rand
  (lambda (e)
    (cadr e)))
(define unparse-lcexp
  (lambda (exp)
    (cases lcexp exp
      (var-exp (var) var)
      (lambda-exp (param body) (list 'lambda (cons param '()) (unparse-lcexp body)))
      (app-exp (rator rand) (list (unparse-lcexp rator) (unparse-lcexp rand))))))


;(require swindle)
;(define unparsed (unparse-lcexp(parse-lcexp '(lambda (x) (lambda (y) (y x)))))
;(lambda-exp->param unparsed)
;(findtype (lambda-exp 'x (var-exp 'y)))
;"lambda-exp"

(define (findtype exp)
  (cases lcexp exp
    (var-exp (var) "var-exp")
    (lambda-exp (param body) "lambda-exp")
    (app-exp (rator rand) "app-exp")))

(define (countrect pexp)
  (cases lcexp pexp
    (var-exp (var) 2);Make 2 boxes if there is an var-exp
    (lambda-exp (param body)  (+ 2 (countrect body)));Make 2 boxes for lambda-exp, param and then increase for body
    (app-exp (rator rand) (+ 1 (countrect rator)(countrect rand)))))

#|(define dialog(instantiate dialog% ("Example")))
(new text-field% [parent dialog][label "Your name"])
(define panel (new horizontal-panel% [parent dialog][alignment '(center center)]))
(new button% [parent panel][label "Cancel"])
(new button% [parent panel][label "OK"])
(when (system-position-ok-before-cancel?)
  (send panel change-children reverse))|#

(require racket/gui/base)
;(define x "lambda-exp")

#|(define (add n)
  (cond
    ((= n 0) n )
    ((> n 0) (display n) (add (- n 1)))
    (else
     ("Error"))))
(define copy
  (lambda (canvas dc)
    (send dc draw-text "Hey Guys" 0 0)))|#

; FOr drawing
#|(define frame (new frame% [label "Drawing"] [width 300][height 300]))

(define child1 (new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 3 3)
        (send dc set-text-foreground "blue")
        (send dc draw-rectangle (add 5) 10 30 10)
        (send dc draw-rectangle 0 10 30 10)
        (send dc draw-rectangle 0 10 30 10)
        (send dc draw-rectangle 0 10 30 10)
        (send dc draw-text x 0 0))]))|#

;(send frame show #t)
;(send frame delete-child child1)

;(define child2 (new canvas% [parent frame]
 ;                   (copy canvas dc)))

(define frame2 (new frame% [label "Frame 2"][width 800][height 800]))
(define treef (new frame% [label "Visualed Tree"][width 800][height 800]))
#|(define canvasdc (lambda (canvas dc)  
   (send dc set-text-foreground "black")
   (send dc draw-rectangle 350 0 100 30)
                   (send dc draw-line 370 30 300 150)
   (send dc draw-rectangle 250 60 100 30)
   (send dc draw-rectangle 450 60 100 30)
   (send dc draw-rectangle 150 120 100 30)
   (send dc draw-rectangle 260 120 100 30)
   (send dc draw-rectangle 400 120 100 30)
   (send dc draw-rectangle 520 120 100 30)
   (send dc draw-text "Deepak" 0 0)  
))|#
;(define x 6)
(define canvasdraw  ;Draws rectangles as per the firstcd(first coordinate), secondcd, length and breadth
  (lambda (canvas dc)
    (let coord((n 9)
               (firstcd 350)
               (secondcd 0)
               (length 100)
               (breadth 30))
      (cond
        ((> n 0)(send dc draw-rectangle firstcd secondcd length breadth)(coord (- n 1)(- firstcd (/ length 2)) (+ 10 (+ secondcd breadth)) length breadth))
        (else (send dc draw-text "No rectangles to Draw" 0 0))))))



(define length 100)
(define breadth 30)
(define exp1 '(lambda(x) (x y)))
(define ygap 30)
(define xgap 20)




(define lambda-draw
  (lambda (canvas dc xcd ycd exp)
    (send dc draw-rectangle xcd ycd length breadth)
    (send dc draw-text "lambda-exp" (+ xgap xcd) ycd)
    (send dc draw-line (+ xcd (/ length 2)) (+ breadth ycd) (+ xgap (- xcd (/ length 2))) (+ ygap (+ ycd breadth)))
    (send dc draw-line (+ xcd (/ length 2)) (+ breadth ycd) (+ xgap (+ xcd length)) (+ ygap (+ ycd breadth)))
    (send dc draw-rectangle (- xcd (/ length 2)) (+ ygap (+ ycd breadth)) length breadth)
    (send dc draw-text (symbol->string (lambda-exp->param exp))(+ 10 (- xcd (/ length 2))) (+ ygap (+ ycd breadth)))
    (cond
      ((var-exp? (lambda-exp->body exp)) (var-draw canvas dc (+ 20 xcd) (+ ygap (+ ycd breadth)) exp))
      ((lambda-exp? (lambda-exp->body exp))(lambda-draw canvas dc (+ 20 xcd) (+ ygap (+ ycd breadth)) (lambda-exp->body exp)))
      ((app-exp? (lambda-exp->body exp)) (app-draw canvas dc (+ xcd (/ length 2)) (+ ygap (+ ycd breadth)) (lambda-exp->body exp))))))

(define var-draw
  (lambda (canvas dc xcd ycd exp)
    (send dc draw-rectangle  xcd ycd length breadth)
    (send dc draw-text "var-exp" (+ xgap xcd) ycd)
    (send dc draw-line (+ xcd (/ length 2)) (+ breadth ycd) (+ xcd (/ length 2)) (+ ygap (+ breadth ycd)))
    (send dc draw-rectangle xcd (+ ygap (+ ycd breadth)) length breadth)
    (send dc draw-text  (symbol->string (var-exp->var exp)) (+ xgap xcd) (+ ygap (+ ycd breadth)))))


(define app-draw
  (lambda (canvas dc xcd ycd exp)
    (send dc draw-rectangle (+ xgap xcd) ycd length breadth)
    (send dc draw-text "app-exp" (+ (* 2 xgap) xcd) ycd)
    (send dc draw-line (+ xcd (/ length 2)) (+ breadth ycd) (+ xgap (- xcd (/ length 2))) (+ ygap (+ ycd breadth)))
    (send dc draw-line (+ xcd (/ length 2)) (+ breadth ycd) (+ xgap (+ xcd length)) (+ ygap (+ ycd breadth)))
    (cond
      ((var-exp? (app-exp->rator exp))(var-draw canvas dc (- xcd (/ length 2)) (+ ygap (+ breadth ycd)) (app-exp->rator exp))(cond
                                                                                                    ((var-exp? (app-exp->rand exp)) (var-draw canvas dc (+ xcd (/ length 2)) (+ ygap (+ ycd breadth)) (app-exp->rand exp)))
                                                                                                    ((lambda-exp? (app-exp->rand exp))(lambda-draw canvas dc (+ 20 xcd)(+ ygap (+ breadth ycd)) (app-exp->rand exp)))
                                                                                                    ((app-exp? (app-exp->rand exp))(app-draw canvas dc (+ 20 xcd)(+ ygap (+ ycd breadth)) (app-exp->rand exp)))))
      ((lambda-exp? (app-exp->rator exp))(lambda-draw canvas dc (- xcd 20) (+ ygap (+ breadth ycd))(app-exp->rator exp))
                                         (cond
                                                                                                    ((var-exp? (app-exp->rand exp)) (var-draw canvas dc (+ 20 xcd) (+ ygap (+ ycd breadth)) (app-exp->rand exp)))
                                                                                                    ((lambda-exp? (app-exp->rand exp))(lambda-draw canvas dc (+ 20 xcd)(+ ygap (+ breadth ycd)) (app-exp->rand exp)))
                                                                                                    ((app-exp? (app-exp->rand exp))(app-draw canvas dc (+ 20 xcd)(+ ygap (+ ycd breadth))(app-exp->rand exp)))))
      ((app-exp? (app-exp->rator exp))(app-draw canvas dc (- xcd 20) (+ ygap (+ breadth ycd)) (app-exp->rator exp))
                                      (cond
                                                                                                    ((var-exp? (app-exp->rand exp)) (var-draw canvas dc (+ 20 xcd) (+ ygap (+ ycd breadth)) (app-exp->rand exp)))
                                                                                                    ((lambda-exp? (app-exp->rand exp))(lambda-draw canvas dc (+ 20 xcd)(+ ygap (+ breadth ycd))(app-exp->rand exp)))
                                                                                                    ((app-exp? (app-exp->rand exp))(app-draw canvas dc (+ 20 xcd)(+ ygap (+ ycd breadth))(app-exp->rand exp))))))))
(define canvasdraw2
  (lambda (canvas dc)
    (check canvas dc (send textbox get-value))))

(define check
  (lambda (canvas dc exp)
    (cond
      ((lambda-exp? exp) (lambda-draw canvas dc 250 0 exp))
      ((var-exp? exp)(var-draw canvas dc 250 0 exp) )
      ((app-exp? exp)(app-draw canvas dc 250 0 exp))
      (else (send dc draw-text "Expression Invalid" 0 0 )))))
      

(define start (new frame% [label "Input"] [width 800][height 800]))

(define title (new message% [parent start]
                          [label "Input function to be visualized"]))

(define textbox (new text-field% [label "function"] [parent start]))

(new button% [parent start]
             [label "Draw Tree"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send treef show #t))])

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



(define treec (new canvas% [parent treef]
     [paint-callback canvasdraw2]))

(send start show #t)

#|(define canvas (new canvas% [parent frame2]  
    [paint-callback canvasdraw2]))
(send frame2 show #t)|#






