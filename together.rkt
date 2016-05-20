#lang racket
(require (lib "eopl.ss" "eopl"))
(require racket/gui/base)


(define-datatype program program?
 (a-program(expl1 expression?)))

(define-datatype expression expression?
 (const-exp(num number?))
 (diff-exp
       (exp1 expression?)
       (exp2 expression?))
 (zero?-exp(exp1 expression?))
 (if-exp
      (exp1 expression?)
      (exp2 expression?)
      (exp3 expression?))
 (var-exp(var symbol?))
 (let-exp
      (var symbol?)
      (exp1 expression?)
      (body expression?))
  (add-exp
   (left expression?)
   (right expression?))
  (mult-exp
   (left expression?)
   (right expression?))
  (div-exp
   (left expression?)
   (right expression?))
  
  (Greater-than-exp
   (exp1 expression?)
   (exp2 expression?))
  (Less-than-exp
   (exp1 expression?)
   (exp2 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (cond-exp
   (list1 (list-of expression?))
   (list2 (list-of expression?)))
 (proc-exp
      (var symbol?)
      (body expression?))
 (call-exp
      (rator expression?)
      (rand expression?))
  (cons-exp
   (car expression?)
   (cdr expression?))
  (car-exp(car expression?))
  (cdr-exp(cdr expression?))
  (null?-exp(null expression?))
  (emptylist-exp))

 (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
   (emptylist-val)
   (cons-val (first expval?)(rest expval?)))

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

 (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

(define expval->car
  (lambda (val)
    (cases expval val
      (cons-val (first rest) first)
      (else (expval-extractor-error 'cons val)))))

(define expval->cdr
  (lambda (val)
    (cases expval val
      (cons-val (first rest) rest)
      (else (expval-extractor-error 'cons val)))))

(define expval->emptylist?
  (lambda (val)
    (cases expval val
      (emptylist-val () #t)
      (cons-val (first rest) #f)
      (else (expval-extractor-error 'cons-or-emptylist val)))))


  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))


(define-datatype environment environment?
  (empty-env)
  (extend-env
   (saved-var var?)
   (saved-val scheme-value?)
   (saved-env environment?)))

(define var? symbol?)
(define scheme-value? (lambda (s) #t))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? search-var saved-var)
                      saved-val
                      (apply-env saved-env search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define has-binding?
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 #f)
      (extend-env (saved-var saved-val saved-env)
                  (or (eqv? search-var saved-var)
                      (has-binding? saved-env search-var))))))

(define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)
      (expression
       ("+" "(" expression "," expression ")") add-exp)
       (expression
       ("*" "(" expression "," expression ")") mult-exp)

       (expression
       ("/" "(" expression "," expression ")") div-exp)

       (expression
        (">" "(" expression "," expression ")")
        Greater-than-exp)
      (expression
        ("<" "(" expression "," expression ")")
        Less-than-exp)
      (expression
        ("equal?" "(" expression "," expression ")")
        equal?-exp)

       (expression
        ("cond" (arbno expression "==>" expression) "end")
        cond-exp); !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!3.12 addition
        
      (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)
      (expression
       ("(" expression expression ")")
       call-exp)


      ))

(define scan&parse
     (sllgen:make-string-parser the-lexical-spec the-grammar))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

 (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  (define value-of
    (lambda (exp env)
      (cases expression exp
        (const-exp (num) (num-val num))
        (var-exp (var) (apply-env env var))
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        (add-exp(left right)
                (let ((left-val (value-of left env))
                      (right-val (value-of right env)))
                (let ((left-num(expval->num left-val))
                     (right-num (expval->num right-val)))
                  (num-val
                   (+ left-num right-num)))))
        (mult-exp(left right)
                (let ((left-val (value-of left env))
                      (right-val (value-of right env)))
                (let ((left-num(expval->num left-val))
                     (right-num (expval->num right-val)))
                  (num-val
                   (* left-num right-num)))))
        (div-exp(left right)
                (let ((left-val (value-of left env))
                      (right-val (value-of right env)))
                (let ((left-num(expval->num left-val))
                     (right-num (expval->num right-val)))
                  (num-val
                   (/ left-num right-num)))))

         (Greater-than-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val
                (> num1 num2)))))
         (Less-than-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val
                (< num1 num2)))))
        (equal?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (bool-val
                (= num1 num2)))))
       (cond-exp (l-exp1 l-exp2)       
          (eval-cond l-exp1 l-exp2 env)) 
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))
        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))
        (cons-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (cons-val val1 val2)))
        (car-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (expval->car val1)))
        (cdr-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (expval->cdr val1)))
        (null?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((bool1 (expval->emptylist? val1)))
                       (bool-val bool1))))
        (emptylist-exp ()
                       (emptylist-val))
            )))

(define eval-cond
  (lambda (l-exp1 l-exp2 env)
    (cond
      ((null? l-exp1) "no true test case")
      ((expval->bool (value-of (car l-exp1) env))(value-of (car l-exp2) env))
      (else (eval-cond (cdr l-exp1) (cdr l-exp2) env)))))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (bvar body saved-env)
         (value-of body (extend-env bvar val saved-env))))))


(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))


#|(define (program->extractor prog)
  (cases program prog
  (a-program (exp) 
             (cases expression exp
             (const-exp (exp1) "Const-exp")
             (diff-exp (exp1 exp2) "diff-exp" (exp->extractor exp1)(exp->extractor exp2))
             (zero?-exp (exp1) "zero-exp" (exp->extractor exp1))
             (if-exp (exp1 exp2 exp3) "If-exp" (exp->extractor exp1)(exp->extractor exp2)(exp->extractor exp3))
             (var-exp (var) "Var-exp")
             (let-exp (var exp1 body)"let-exp"(exp->extractor exp1)(exp->extractor body))
             (add-exp (exp1 exp2) "add-exp"(exp->extractor exp1) (exp->extractor exp2))
             (mult-exp (exp1 exp2) "mult-exp"(exp->extractor exp1)(exp->extractor exp2))
             (div-exp (exp1 exp2) "Div-exp" (exp->extractor exp1)(exp->extractor exp2))))))|#

(define (exp->extractor exp canvas dc xcd ycd mid)
  (cases expression exp
    (const-exp(exp1)
              (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd)  
              #|(let ((x1 (- xcd len)) (y1 (- (- ycd ygap) breadth)) (x2 (+ xcd len)) (y2 (- (- ycd ygap) breadth)))
             (if (> xcd mid)(send dc draw-line x1 y1 xcd ycd)(send dc draw-line x2 y1 xcd ycd)))|#
              (send dc draw-rectangle xcd ycd len breadth)
              (send dc draw-text "Const-exp" xcd ycd)
              (send dc draw-rectangle (+ xcd (/ xgap 2)) (+ ycd ygap) (/ len 3) breadth)
              (send dc draw-line (+ xcd (/ len 2)) (+ ycd ygap) (+ xcd (/ len 2)) (+ ycd 30))
              (send dc draw-text (number->string exp1) (+ xcd (/ xgap 2)) (+ ycd ygap)))
    ;(send dc draw-text "const-exp" xcd ycd))
    (diff-exp(exp1 exp2) 
             (send dc draw-rectangle xcd ycd len breadth)
             (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
             (send dc draw-text "Diff-exp" xcd ycd) (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))) (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))

    (zero?-exp(exp1) 
              (send dc draw-rectangle xcd ycd len breadth)
              (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
              (send dc draw-text "Zero-exp" xcd ycd) (exp->extractor exp1 canvas dc xcd (+ (+ ycd breadth) ygap) (+ xcd (/ len 2)))) 
              
    (if-exp (exp1 exp2 exp3 ) (send dc draw-text "If-exp" xcd  ycd) (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))) (exp->extractor exp2 canvas dc xcd (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))) (exp->extractor exp3 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    (var-exp(var)"Var-exp")
    (let-exp(var exp1 body) "let-exp")
    (add-exp (left right )"Add-exp")
    (mult-exp(left right)"Mult-exp")
    (div-exp (left right)"Div-exp")
    (cond-exp (l-exp1 l-exp2) "Cond-exp")
    (Greater-than-exp (left right) "Greater-than-exp")
    (Less-than-exp (left right) "Less-than-exp")
    (equal?-exp (left right) "equal?-exp")
    (proc-exp (left right) "proc-exp")
    (call-exp (left right) "call-exp")
    (cons-exp (exp1 exp2) "cons-exp")
    (car-exp (exp) "car-exp")
    (cdr-exp (exp) "cdr-exp")
    (null?-exp (exp) "null?-exp")
    (emptylist-exp "emptylist-exp")))




(define canvasdraw
  (lambda(canvas dc)
    ;(program->extractor canvas dc (scan&parse "-(-(5,6),4)"))))
      ;(program->extractor canvas dc (scan&parse "zero?(-(4,5))"))))
(program->extractor canvas dc (scan&parse  "if zero?(3) then -(3,4) else -(2,4)"))))

(define xcd 400)
(define ycd 10)
(define len 80)
(define breadth 30)
(define ygap 40)
(define xgap 60)

(define program->extractor 
  (lambda(canvas dc prog)
    (send dc draw-rectangle xcd ycd len breadth)
    (cases program prog
  (a-program (exp) 
             (cases expression exp
             (const-exp (exp1) "Const-exp")
             (diff-exp (exp1 exp2) (send dc draw-text "diff-exp" xcd ycd) (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2))) (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
             (zero?-exp (exp1) (send dc draw-text "Zero-exp" xcd ycd) (exp->extractor exp1 canvas dc xcd (+ (+ ycd breadth) ygap) (+ xcd (/ len 2))))
             (if-exp (exp1 exp2 exp3) (send dc draw-text "If-exp" xcd ycd) (exp->extractor exp1 canvas dc (- xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))) (exp->extractor exp2 canvas dc xcd (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))) (exp->extractor exp3 canvas dc (+ xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
             (var-exp (var) "Var-exp")
             (let-exp (var exp1 body)"let-exp"(exp->extractor exp1)(exp->extractor body))
             (add-exp (exp1 exp2) "add-exp"(exp->extractor exp1) (exp->extractor exp2))
             (mult-exp (exp1 exp2) "mult-exp"(exp->extractor exp1)(exp->extractor exp2))
             (div-exp (exp1 exp2) "Div-exp" (exp->extractor exp1)(exp->extractor exp2))
             (cond-exp (l-exp1 l-exp2) "cond-exp")
             (Greater-than-exp (exp1 exp2) "Greater-than-exp"(exp->extractor exp1) (exp->extractor exp2))
             (Less-than-exp (exp1 exp2) "Less-than-exp"(exp->extractor exp1) (exp->extractor exp2))
             (equal?-exp (exp1 exp2) "equal?-exp"(exp->extractor exp1) (exp->extractor exp2))
             (proc-exp (exp1 exp2) "proc-exp")
             (call-exp (exp1 exp2) "call-exp")
             (cons-exp (exp1 exp2) "cons-exp")
             (car-exp (exp) "car-exp"(exp->extractor exp))
             (cdr-exp (exp) "cdr-exp"(exp->extractor exp))
             (null?-exp (exp) "null?-exp"(exp->extractor exp))
             (emptylist-exp "emptylist-exp")
               )))))

#|(define drawdiff-exp
  (lambda(exp)
    (send dc draw-text "diff-exp" 25 30)
    (cases expression exp
      (
    |#


#|(define frame (new frame% [label "Letlang"][width 800][height 800]))
(define canvas (new canvas% [parent frame]
                    [paint-callback canvasdraw]))
(send frame show #t)|#



  
                     

#|(define constexp->num
(lambda(e)
  e))|#

(define (constexp->num exp)
  (cases expression exp
    (const-exp (num) num)
    (else "Error")))

(define (let-exp->var exp)
  (cases expression exp
    (let-exp (var exp1 exp2) var)
    (else "Error")))

(define (varexp->var exp)
  (cases expression exp
    (var-exp (var) var)
    (else "Error")))


(define treef (new frame% [label "Visualed Tree"][width 800][height 800]))

(define canvasdraw2
  (lambda (canvas dc)
    (program->extractor canvas dc (scan&parse  (send textbox get-value)))))

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