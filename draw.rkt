#lang racket
(require "defined.rkt"); Imports defined.rkt which contains the define-datatype portion of the language
(require (lib "eopl.ss" "eopl"))
(require racket/gui/base)


(define (exp->extractor exp canvas dc xcd ycd mid)
  (cases expression exp
    (const-exp(exp1)
              (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd)  
              
              (send dc set-brush "yellow" 'solid)
              (send dc draw-rectangle xcd ycd len breadth)
              (send dc draw-text "Const-exp" xcd ycd)
              (send dc set-brush "red" 'solid)
              (send dc draw-rectangle (+ xcd (/ xgap 2)) (+ ycd ygap) (/ len 3) breadth)
              (send dc set-brush "green" 'solid)
              (send dc draw-line (+ xcd (/ len 2)) (+ ycd ygap) (+ xcd (/ len 2)) (+ ycd 30))
              
              
              (send dc draw-text "num" (+ xcd (/ len 2)) (+ ycd (/ ygap 2)))
              (send dc draw-text (number->string exp1) (+ xcd (/ xgap 2)) (+ ycd ygap)))
    
    (diff-exp(exp1 exp2)
             (send dc set-brush "yellow" 'solid)
             (send dc draw-rectangle xcd ycd len breadth)
             (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd)
             
             (send dc draw-text "Diff-exp" xcd ycd)
             (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
             (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    
    (zero?-exp(exp1)
              (send dc set-brush "yellow" 'solid)
              (send dc draw-rectangle xcd ycd len breadth)
              (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
              (send dc draw-text "Zero-exp" xcd ycd) (exp->extractor exp1 canvas dc xcd (+ (+ ycd breadth) ygap) (+ xcd (/ len 2)))) 
    
    (if-exp (exp1 exp2 exp3 )
            (send dc set-brush "yellow" 'solid)
            ;(send dc draw-text "If-exp" xcd  ycd)
            (send dc draw-text "If-exp" xcd ycd)
            (send dc draw-text "exp1" (- xcd len) (+ ycd ygap))
            (send dc draw-text "exp2" xcd (+ ycd ygap))
            (send dc draw-text "exp3" (+ xcd (* 2 len)) (+ ycd ygap))
            (exp->extractor exp1 canvas dc (- xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
            (exp->extractor exp2 canvas dc xcd (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
            (exp->extractor exp3 canvas dc (+ xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
            
           ; (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
            ;(exp->extractor exp2 canvas dc xcd (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
            ;(exp->extractor exp3 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    
    (var-exp(var)
            (send dc set-brush "yellow" 'solid)
            (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd)  
            (send dc draw-rectangle xcd ycd len breadth)
            (send dc draw-text "Var-exp" xcd ycd )
            (send dc set-brush "red" 'solid)
            (send dc draw-rectangle (+ xcd (/ xgap 2)) (+ ycd ygap) (/ len 3) breadth)
            (send dc draw-line (+ xcd (/ len 2)) (+ ycd ygap) (+ xcd (/ len 2)) (+ ycd 30))
            (send dc draw-text "var" (+ xcd (/ len 2)) (+ ycd  (/ ygap 2)))
            (send dc draw-text (symbol->string var) (+ xcd (/ xgap 1.5)) (+ ycd ygap)))
    (let-exp(var exp1 body)
            (send dc set-brush "yellow" 'solid)
            (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
            (send dc draw-rectangle  xcd ycd len breadth )
            (send dc draw-text "Let-exp" xcd  ycd)
            
            
            
            (send dc draw-text "var" (- xcd len) (+ ycd ygap))
            (send dc draw-text "body" xcd (+ ycd ygap))
            (send dc draw-text "exp" (+ xcd (* 2 len)) (+ ycd ygap))
            (send dc set-brush "red" 'solid)
            (send  dc draw-rectangle (- xcd (* len 3)) (+ (+ breadth ycd) ygap) len breadth)
            (send dc draw-line (+ xcd (/ len 2)) (+ ycd breadth) (+ (- xcd (* len 3)) (/ len 2)) (+ (+ breadth ycd) ygap))
            (send dc draw-text (symbol->string  var) (- xcd (* len 2.5)) (+ (+ breadth ycd) ygap))
            
            (exp->extractor  exp1 canvas dc xcd (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
            (exp->extractor body canvas dc (+ xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
            
    
    (add-exp (exp1 exp2 )
             (send dc set-brush "yellow" 'solid)
             (send dc draw-rectangle xcd ycd len breadth)
             (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
             (send dc draw-text "Add-exp" xcd ycd)
             (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
             (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    (mult-exp(exp1 exp2)
             (send dc set-brush "yellow" 'solid)
             (send dc draw-rectangle xcd ycd len breadth)
             (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
             (send dc draw-text "Mult-exp" xcd ycd)
             (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
             (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    
    (div-exp (exp1 exp2)
             (send dc set-brush "yellow" 'solid)
             (send dc draw-rectangle xcd ycd len breadth)
             (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
             (send dc draw-text "Div-exp" xcd ycd)
             (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
             (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    
   
    (cond-exp (l-exp1 l-exp2)
              (cond
                ((null? l-exp1)  #f)
                (else
                       (exp->extractor (car l-exp1) canvas dc (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2) mid)
                       (exp->extractor (car l-exp2) canvas dc (/ (+ (* 2.6 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2) mid)
                       (exp->extractor (cond-exp (cdr l-exp1) (cdr l-exp2)) canvas dc
                                       (+ xcd 425) ycd mid))))
    
    
    
    
    (Greater-than-exp (exp1 exp2)
                      (send dc set-brush "yellow" 'solid)
                      (send dc draw-rectangle xcd ycd len breadth)
                      (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
                      (send dc draw-text "Greater-than-exp" xcd ycd)
                      (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                      (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                      (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
                      (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    
    (Less-than-exp (exp1 exp2)
                   (send dc set-brush "yellow" 'solid)
                   (send dc draw-rectangle xcd ycd len breadth)
                   (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
                   (send dc draw-text "Less-than-exp" xcd ycd)
                   (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                   (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                   (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
                   (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    
    (equal?-exp (exp1 exp2)
                (send dc set-brush "yellow" 'solid)
                (send dc draw-rectangle xcd ycd len breadth)
                (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
                (send dc draw-text "equal?-exp" xcd ycd)
                (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                (exp->extractor exp1 canvas dc (- xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
                (exp->extractor exp2 canvas dc (+ xcd xgap) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    
    (proc-exp (var exp2)
              (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd)
              (send dc set-brush "yellow" 'solid)
              (send dc draw-rectangle xcd ycd len breadth)
              (send dc draw-text "proc-exp" xcd ycd)
              (send dc draw-line (+ xcd (/ len 2)) (+ ycd breadth) (- xcd (/ len 2)) (+ (+ breadth ycd) ygap))
              (send dc set-brush "red" 'solid)
              (send dc draw-rectangle (- xcd len) (+ (+ breadth ycd) ygap) len breadth)
              (send dc draw-text (symbol->string var) (- xcd (/ len 2)) (+ (+ ycd ygap) breadth))
              (send dc draw-text "var" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
              (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
              (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    (call-exp (exp1 exp2)
              (send dc set-brush "yellow" 'solid)
              (send dc draw-rectangle xcd ycd len breadth)
              (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
             
              (send dc draw-text "call-exp" xcd ycd)
              (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
              (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
              (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
              (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    (cons-exp (exp1 exp2)
              (send dc set-brush "yellow" 'solid)
              (send dc draw-rectangle xcd ycd len breadth)
              (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
              (send dc draw-text "cons-exp" xcd ycd)
              (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
              (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
              (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
              (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    (car-exp (exp1 exp2)
             (send dc set-brush "yellow" 'solid)
             (send dc draw-rectangle xcd ycd len breadth)
            (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
             (send dc draw-text "car-exp" xcd ycd)
             (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (exp->extractor exp1 canvas dc (- xcd (* 2 len)) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
             (exp->extractor exp2 canvas dc (+ xcd (* 2 len)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    (cdr-exp (exp1 exp2)
             (send dc set-brush "yellow" 'solid)
             (send dc draw-rectangle xcd ycd len breadth)
             (send dc draw-line mid (- ycd ygap) (+ xcd (/ len 2)) ycd) 
             (send dc draw-text "cdr-exp" xcd ycd)
             (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
             (exp->extractor exp1 canvas dc (- xcd (* 2 len)) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
             (exp->extractor exp2 canvas dc (+ xcd (* 2 len)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
    (null?-exp (exp)
               (send dc draw-text "null?-exp" xcd ycd)
               
               (exp->extractor exp canvas dc xcd (+ (+ ycd breadth) ygap) (+ xcd (/ len 2))))
    (emptylist-exp  (send dc draw-text "emptylist-exp" xcd ycd))))

(define tree  "car(2,3)")

(define canvasdraw
  (lambda(canvas dc)
    ;(program->extractor canvas dc (scan&parse "-(-(5,6),4)"))))
    ;(program->extractor canvas dc (scan&parse "zero?(-(4,5))"))))
    (program->extractor canvas dc tree)
    ;(program->extractor canvas dc "let x = 30 in -(x, 20)")
    
    ;(program->extractor canvas dc  " let y= -(2,3) in +(9,5)")
    ;(program->extractor canvas dc "car(-(2,3), +(2,4))")
    ;(send dc draw-text (format "~s" (run tree)) xcd (- ycd 90))))

;(program->extractor canvas dc "let x=2 in let y=4 in +(x,y)")
; (program->extractor canvas dc  " let y= -(2,3) in +(9,5)")
;(program->extractor canvas dc "car(-(2,3), +(2,4))")
;(program->extractor canvas dc "cond zero?(0) ==> 0 end")
(send dc draw-text (format "~s" (run "-(2,3)")) xcd (- ycd 90))))



(define xcd 400)
(define ycd 100)
(define len 80)
(define breadth 30)
(define ygap 60)
(define xgap 60)

(define program->extractor 
  (lambda(canvas dc prog1)
    (send dc set-brush "green" 'solid)
    (send dc draw-rectangle xcd ycd len breadth)
    (let ((prog (scan&parse prog1)))
      (cases program prog
        (a-program (exp) 
                   (cases expression exp
                     (const-exp (exp1)
                                (send dc draw-text "Const-exp" xcd ycd)
                                (send dc set-brush "red" 'solid)
                                (send dc draw-rectangle (+ xcd (/ xgap 2)) (+ ycd ygap) (/ len 3) breadth)
                                (send dc draw-line (+ xcd (/ len 2)) (+ ycd ygap) (+ xcd (/ len 2)) (+ ycd 30))
                                (send dc draw-text "num" (+ xcd (/ len 2)) (+ ycd (/ ygap 2)))
                                (send dc draw-text (number->string exp1) (+ xcd (/ xgap 2)) (+ ycd ygap)))
                     (diff-exp (exp1 exp2)
                               (send dc draw-text "diff-exp" xcd ycd)
                               (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                               (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (zero?-exp (exp1)
                                (send dc draw-text "Zero-exp" xcd ycd)
                                (exp->extractor exp1 canvas dc xcd (+ (+ ycd breadth) ygap) (+ xcd (/ len 2))))
                     (if-exp (exp1 exp2 exp3)
                             (send dc draw-text "If-exp" xcd ycd)
                             (send dc draw-text "exp1" (- xcd len) (+ ycd ygap))
                             (send dc draw-text "exp2" xcd (+ ycd ygap))
                             (send dc draw-text "exp3" (+ xcd (* 2 len)) (+ ycd ygap))
                             (exp->extractor exp1 canvas dc (- xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
                             (exp->extractor exp2 canvas dc xcd (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
                             (exp->extractor exp3 canvas dc (+ xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (var-exp (var)
                              (send dc draw-text "Var-exp" xcd ycd)
                              (send dc set-brush "red" 'solid)
                              (send dc draw-rectangle xcd (+ (+ ycd ygap) breadth) len breadth)
                              (send dc draw-line (+ xcd (/ len 2)) (+ ycd breadth)(+ xcd (/ len 2)) (+ (+ ycd breadth) ygap)) 
                              (send dc draw-text (symbol->string var) (+ xcd (/ len 2)) (+ (+ ycd ygap) breadth))) 
                     (let-exp (var exp1 body)
                              (send dc draw-text "Let-exp" xcd ycd)
                              (send dc draw-text "var" (- xcd len) (+ ycd ygap))
                              (send dc draw-text "body" xcd (+ ycd ygap))
                              (send dc draw-text "exp" (+ xcd (* 2 len)) (+ ycd ygap))
                              (send dc set-brush "red" 'solid)
                              (send  dc draw-rectangle (- xcd (* len 3)) (+ (+ breadth ycd) ygap) len breadth)
                              (send dc set-brush "yellow" 'solid)
                              (send dc draw-line (+ xcd (/ len 2)) (+ ycd breadth) (+ (- xcd (* len 3)) (/ len 2)) (+ (+ breadth ycd) ygap))
                              (send dc draw-text (symbol->string  var) (- xcd (* len 2.5)) (+ (+ breadth ycd) ygap))
                              (send dc draw-line (+ xcd (/ len 2)) (+ ycd breadth) (+ xcd (/ len 2)) (+ (+ ycd breadth) ygap))
                              
                              (exp->extractor  exp1 canvas dc xcd (+ (+ breadth ycd) ygap) (+ xcd (/ len 2)))
                              (exp->extractor body canvas dc (+ xcd (* len 3)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     
                     (add-exp (exp1 exp2)
                              (send dc draw-text "add-exp" xcd ycd)
                              (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                              (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     
                     (mult-exp (exp1 exp2)
                               (send dc draw-text "mult-exp" xcd ycd)
                               (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))   
                               (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                               (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (div-exp (exp1 exp2)
                              (send dc draw-text "div-exp" xcd ycd)
                              (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                              (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     
                       
                     (cond-exp (l-exp1 l-exp2)
                                 (send dc draw-text "Cond-exp" xcd ycd)
                                 (exp->extractor (cond-exp l-exp1 l-exp2)
                                                 canvas dc (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2) (+ xcd (/ len 2))))
           
                     
                     (Greater-than-exp (exp1 exp2)
                                       (send dc draw-text "Greater-than-exp" xcd ycd)
                                       (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                                       (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                                       (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                                       (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (Less-than-exp (exp1 exp2)
                                    (send dc draw-text "Less-than-exp" xcd ycd)
                                    (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                                    (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                                    (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                                    (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (equal?-exp (exp1 exp2)
                                 (send dc draw-text "equal?-exp" xcd ycd)
                                 (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                                 (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                                 (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                                 (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (proc-exp (var exp2)
                               
                               (send dc draw-text "proc-exp" xcd ycd)
                               (send dc draw-line (+ xcd (/ len 2)) (+ ycd breadth) (- xcd (/ len 2)) (+ (+ breadth ycd) ygap))
                               (send dc draw-rectangle (- xcd len) (+ (+ breadth ycd) ygap) len breadth)
                               (send dc draw-text "proc-exp" xcd ycd)
                               (send dc draw-text (symbol->string var) (- xcd (/ len 2)) (+ (+ ycd ygap) breadth))
                               (send dc draw-text "var" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                
                     (call-exp (exp1 exp2)
                               (send dc draw-text "call-exp" xcd ycd)
                               (send dc draw-text "call-exp" xcd ycd)
                               (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                               (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                   (cons-exp (exp1 exp2)
                               (send dc draw-text "cons-exp" xcd ycd)
                               (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                               (exp->extractor exp1 canvas dc (- xcd len) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                               (exp->extractor exp2 canvas dc (+ xcd len) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (car-exp (exp1 exp2)
                              (send dc draw-text "car-exp" xcd ycd)
                              (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (exp->extractor exp1 canvas dc (- xcd (* 2 len)) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                              (exp->extractor exp2 canvas dc (+ xcd (* 2 len)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (cdr-exp (exp1 exp2)
                              
                              (send dc draw-text "cdr-exp" xcd ycd)
                              (send dc draw-text "left" (/ (- (* 2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (send dc draw-text "right" (/ (+ (* 2.2 xcd)len)2) (/  (+ (+ breadth (* 2 ycd)) ygap) 2))
                              (exp->extractor exp1 canvas dc (- xcd (* 2 len)) (+ (+ breadth ycd) ygap)  (+ xcd (/ len 2)))
                              (exp->extractor exp2 canvas dc (+ xcd (* 2 len)) (+ (+ breadth ycd) ygap) (+ xcd (/ len 2))))
                     (null?-exp (exp)
                                (send dc draw-text "null?-exp" xcd ycd)
                                
                                (exp->extractor exp canvas dc xcd (+ (+ ycd breadth) ygap) (+ xcd (/ len 2))))
                     (emptylist-exp  (send dc draw-text "emptylist-exp" xcd ycd)
                                     )))))))
#|
(define frame (new frame% [label "Letlang"][width 800][height 800]))
(define canvas (new canvas% [parent frame]
                    [paint-callback canvasdraw]))
(send frame show #t)|#

(provide (all-defined-out))