;Ron Weber and Steven Knipe
(load "simpleParser.scm")
(interpreter "test")
;state is a list of substates
;a substate is a list of pairs with the same scope
;the first in the pair is the varname. the second is either the value (number/bool) or empty list if undefined
(define interpreter
  (lambda (filename)
    (interpret (parser filename) '())))
;dealing with variables
;Returns true if the substate contains a variable named varname
(define varExists_sub
  (lambda (varname substate)
    (cond
     ((null? substate) #f)
     ((eq? varname (caar substate)) #t)
     (else (varExists_sub varname (cdr substate))))))
;Adds a variable named varname with value to substate.  Errors if it already exists
(define addVar_sub
  (lambda (varname value substate)
    (if (varExists varname substate)
	(error "Variable declared multiple times.")
        (cons (cons varname value) substate))))
;Returns substate modified so that the entry for varname is set to value.
(define setVar_sub
    (lambda (varname value substate)
	(cond
	 ((null? substate) (error "Variable assigned before declared."))
	 ((eq? varname (caar substate)) (cons (cons varname value) (cdr substate)))
	 (else (cons (car substate) (setVar info (cdr substate)))))))
;Returns the value associated with varname in substate
(define findVar_sub
    (lambda (varname substate)
	(cond
	 ((null? substate) (error "Variable used before declared."))
	 ((not (eq? varname (caar substate))) (findVar_sub varname (cdr substate)))
	 ((null? (cdar substate)) (error "Variable used before assigned."))
	 (else (cdar substate)))))
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
(define operand2-or-empty
  (lambda (l)
    (if (null? (cddr l)) '() (operand2 l))))

(define Mstate
    (lambda (statement state)
        (cond
	 ((eq? (operator statement) 'return) (Mvalue (cadr statement) state));this replaces state with a value
					;and ends execution immediately
	 ((eq? (operator statement) 'var) (addVar (operand1 statement) (Mvalue (operand2-or-empty statement) state) state))
	 ((eq? (operator statement) '=) (setVar (operand1 statement) (Mvalue (operand2-or-empty statement) state) state))
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
     ((null? expr) expr)
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
