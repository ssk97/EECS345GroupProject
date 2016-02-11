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
					;Info is a 2 member list:name and the expression to set name to.
(define setVar
    (lambda (info state)
	(cond
	 ((null? state) (error "Variable assigned before declared."))
	 ((eq? (car info) (caar state)) (cons (cons (car info) (Mvalue (cadr info) state)) (cdr state)))
	 (else (cons (car state) (setVar info (cdr state)))))))
(define findVar
    (lambda (varname state)
	(cond
	 ((null? state) (error "Variable used before declared."))
	 ((not (eq? varname (caar state))) (findVar varname (cdr state)))
	 ((null? (cdar state)) (error "Variable used before assigned."))
	 (else (cdar state)))))
;Primary doing stuff
;converts #t and #f to 'true and 'false respectively
(define outputNice 
    (lambda (a)
        (cond
	 ((eq? a #t) 'true)
	 ((eq? a #f) 'false)
	 (else a))))
(define interpret
    (lambda (parsetree state)
        (cond
	 ((number? state) state)
	 ((boolean? state) (outputNice state));if state is a single value, immediately return
	 (else (interpret (cdr parsetree) (Mstate (car parsetree) state))))))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)

(define Mstate
    (lambda (statement state)
        (cond
	 ((eq? (operator statement) 'return) (Mvalue (cadr statement) state));this replaces state with a value
					;and ends execution immediately
	 ((eq? (operator statement) 'var) (addVar (cdr statement) state))
	 ((eq? (operator statement) '=) (setVar (cdr statement) state))
	 ((eq? (operator statement) 'if) (Mstate_if (cadr statement) (cddr statement) state)) ;cddr can have 1 or 2 statements in it: if 2 then it has an 'else' case.
	 ((eq? (operator statement) 'while) (Mstate_while (operand1 statement) (operand2 statement) state))
	 (else state)
	 )))

(define Mboolean
    (lambda (statement state)
        (cond
	 ((eq? statement 'true) #t)
	 ((eq? statement 'false) #f)
	 ((symbol? statement) (findVar statement state));variable
	 ((eq? (operator statement) '==) (eq? (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
	 ((eq? (operator statement) '!=) (not (eq? (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state))))
	 ((eq? (operator statement) '>) (> (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
	 ((eq? (operator statement) '<) (< (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
	 ((eq? (operator statement) '>=) (>= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
	 ((eq? (operator statement) '<=) (<= (Mvalue (operand1 statement) state) (Mvalue (operand2 statement) state)))
	 
	 ((eq? (operator statement) '&&) (and (Mboolean (operand1 statement) state) (Mboolean (operand2 statement) state)))
	 ((eq? (operator statement) '||) (or (Mboolean (operand1 statement) state) (Mboolean (operand2 statement) state)))
	 ((eq? (operator statement) '!) (not (Mboolean (operand1 statement) state)))
	 )))

(define Mvalue
    (lambda (expr state)
        (cond
	 ((number? expr) expr)
	 ((eq? expr 'true) #t)
	 ((eq? expr 'false) #f)
	 ((symbol? expr) (findVar expr state));variable
	 ((eq? (operator expr) '+) (+ (Mvalue (operand1 expr) state) (Mvalue (operand2 expr) state)))
	 ((eq? (operator expr) '*) (* (Mvalue (operand1 expr) state) (Mvalue (operand2 expr) state)))
	 ((eq? (operator expr) '/) (quotient (Mvalue (operand1 expr) state) (Mvalue (operand2 expr) state)))
	 ((eq? (operator expr) '-) (if (eq? (length expr) 3)
				       (- (Mvalue (operand1 expr) state) (Mvalue (operand2 expr) state))
				       (- (Mvalue (operand1 expr) state))));unary - operator
	 ((eq? (operator expr) '%) (remainder (Mvalue (operand1 expr) state) (Mvalue (operand2 expr) state)))
	 (else (Mboolean expr state))
	 )))

(define Mstate_while
    (lambda (condition statement state)
        (if (Mboolean condition state)
            (Mstate_while condition statement (Mstate statement state))
            state)))

(define Mstate_if
    (lambda (condition statements state)
        (if (Mboolean condition state)
            (Mstate (car statements) state)
            (if (pair? (cdr statements))
                (Mstate (cadr statements) state)
                state))))
