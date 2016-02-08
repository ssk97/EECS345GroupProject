(load "simpleParser.scm")

;state is a list of pairs
;the first in the pair is the varname. the second is either the value (number/bool) or empty list if undefined

(define interpreter
  (lambda (filename)
    (interpret (parser filename) '())))

;dealing with variables
(define addVar
    (lambda (info state)
        (if (pair? (cdr info))
            (cons (cons (car info) (Mvalue (cadr info) state)) state)
            (cons (cons (car info) '()) state))))
(define removeVar
    (lambda (varname state)
        (cond
            ((eq? varname (caar state)) (cdr state))
            (else (cons (car state) (removeVar varname (cdr state)))))))
(define setVar
    (lambda (info state)
        (addVar info (removeVar (car info) state))))
(define findVar
    (lambda (varname state)
        (cond
            ((eq? varname (caar state)) (cdar state))
            (else (findVar varname (cdr state))))))
;Primary doing stuff
(define evalBool
    (lambda (condition statements parsetree state)
        (if (Mboolean condition state)
            (interpret (cdr parsetree) (Mstate (car statements) state))
            (if (pair? (cdr statements))
                (interpret (cdr parsetree) (Mstate (cadr statements) state))
                (interpret (cdr parsetree) state)))))
(define outputNice ;converts #t and #f to 'true and 'false respectively
    (lambda (a)
        (cond
            ((eq? a #t) 'true)
            ((eq? a #f) 'false)
            (else a))))
(define interpret
  (lambda (parsetree state)
    (cond
        ((number? state) state)
        ((boolean? state) (outputNice state))
        ((eq? (caar parsetree) 'return) (outputNice (Mvalue (cadar parsetree) state)))
        ((eq? (caar parsetree) 'if) (evalBool (cadar parsetree) (cddar parsetree) parsetree state))
        (else (interpret (cdr parsetree) (Mstate (car parsetree) state))))))
    
(define Mstate
    (lambda (statement state)
        (cond
            ((eq? (car statement) 'return) (Mvalue (cadr statement) state));this replaces state with a number
                                                                           ;and ends execution immediately
            ((eq? (car statement) 'var) (addVar (cdr statement) state))
            ((eq? (car statement) '=) (setVar (cdr statement) state))
            (else state)
)))
(define Mboolean
    (lambda (statement state)
        (cond
            ((eq? (car statement) '==) (eq? (Mvalue (cadr statement) state) (Mvalue (caddr statement) state)))
            ((eq? (car statement) '!=) (not (eq? (Mvalue (cadr statement) state) (Mvalue (caddr statement) state))))
            ((eq? (car statement) '>) (> (Mvalue (cadr statement) state) (Mvalue (caddr statement) state)))
            ((eq? (car statement) '<) (< (Mvalue (cadr statement) state) (Mvalue (caddr statement) state)))
            ((eq? (car statement) '>=) (>= (Mvalue (cadr statement) state) (Mvalue (caddr statement) state)))
            ((eq? (car statement) '<=) (<= (Mvalue (cadr statement) state) (Mvalue (caddr statement) state)))
)))
            
(define Mvalue
    (lambda (statement state)
        (cond
            ((number? statement) statement)
            ((eq? statement 'true) #t)
            ((eq? statement 'false) #f)
            ((symbol? statement) (findVar statement state));variable
            (else (Mboolean statement state))
)))

#|
((var x)
    (= x 10) 
    (var y (+ (* 3 x) 5))
    (while (!= (% y x) 3) (= y (+ y 1)))
    (if (> x y)
        (return x) 
        (if (> (* x x) y)
            (return (* x x))
            (if (> (* x (+ x x)) y)
                (return (* x (+ x x)))
                (return (- y 1)))))) 
|#