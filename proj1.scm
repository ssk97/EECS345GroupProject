;Ron Weber and Steven Knipe
(load "simpleParser.scm")
;state is a list of substates
;a substate is a list of pairs with the same scope
;the first in the pair is the varname. the second is either the value (number/bool) or empty list if undefined
(define interpret
  (lambda (filename)
    (outputNice
     (call/cc
      (lambda (return-c)
        (interpreter (parser filename) '(()) return-c))))))
;dealing with variables

;substate functions

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
    (if (varExists_sub varname substate)
	(error "Variable declared multiple times.")
        (cons (cons varname value) substate))))
;Returns substate modified so that the entry for varname is set to value.
(define setVar_sub
    (lambda (varname value substate)
	(cond
	 ((null? substate) (error "Variable assigned before declared- setVar_sub"))
	 ((eq? varname (caar substate)) (cons (cons varname value) (cdr substate)))
	 (else (cons (car substate) (setVar_sub varname value (cdr substate)))))))
;Returns the value associated with varname in substate
(define findVar_sub
    (lambda (varname substate)
	(cond
	 ((null? substate) (error "Variable used before declared- findVar_sub"))
	 ((not (eq? varname (caar substate))) (findVar_sub varname (cdr substate)))
	 ((null? (cdar substate)) (error "Variable used before assigned."))
	 (else (cdar substate)))))
     
;functions that work across entire state
(define varExists
    (lambda (varname state)
        (cond
            ((null? state) #f)
            ((varExists_sub varname (car state)) #t)
            (else (varExists varname (cdr state))))))
;Adds varname to the topmost substate
(define addVar
    (lambda (varname value state)
        (cons (addVar_sub varname value (car state)) (cdr state))))
;returns state modified so that varname is set to value
(define setVar
    (lambda (varname value state)
        (cond
            ((null? state) (error "Variable used before declared- setVar"))
            ((varExists_sub varname (car state)) (cons (setVar_sub varname value (car state)) (cdr state)))
            (else (cons (car state) (setVar varname value (cdr state)))))))
;returns the variable varname
(define findVar
    (lambda (varname state)
	(cond
	 ((null? state) (error "Variable used before declared- findVar"))
	 ((varExists_sub varname (car state)) (findVar_sub varname (car state)))
	 (else (findVar varname (cdr state))))))
;adds one more layer of scope onto state
(define stateBegin
    (lambda (state)
        (cons '() state)))
;removes a layer of scope from state
(define stateEnd
    (lambda (state)
        (if (list? state)
            (cdr state);normal case
            state)));if program has ended, and you are returning a value
  
;Primary doing stuff
;converts #t and #f to 'true and 'false respectively
(define outputNice 
    (lambda (a)
        (cond
	 ((eq? a #t) 'true)
	 ((eq? a #f) 'false)
	 (else a))))
(define interpreter
    (lambda (parsetree state return-c)
        (cond
         ((null? parsetree) state) ;if you're at the end of your parsetree and haven't returned, return the full current state
	 (else (interpreter (cdr parsetree) (Mstate (car parsetree) state return-c) return-c)))))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand2-or-empty
  (lambda (l)
    (if (null? (cddr l)) '() (operand2 l))))

(define Mstate
    (lambda (statement state return-c)
        (cond
         ((eq? (operator statement) 'begin) (stateEnd (interpreter (cdr statement) (stateBegin state) return-c)))
	 ((eq? (operator statement) 'return) (return-c (Mvalue (cadr statement) state)))
	 ((eq? (operator statement) 'var) (addVar (operand1 statement) (Mvalue (operand2-or-empty statement) state) state))
	 ((eq? (operator statement) '=) (setVar (operand1 statement) (Mvalue (operand2-or-empty statement) state) state))
	 ((eq? (operator statement) 'if) (Mstate_if (cadr statement) (cddr statement) state return-c)) ;cddr can have 1 or 2 statements in it: if 2 then it has an 'else' case.
	 ((eq? (operator statement) 'while) (Mstate_while (operand1 statement) (operand2 statement) state return-c))
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
    (lambda (condition statement state return-c)
        (if (Mboolean condition state)
            (Mstate_while condition statement (Mstate statement state return-c) return-c)
            state)))

(define Mstate_if
    (lambda (condition statements state return-c)
        (if (Mboolean condition state)
            (Mstate (car statements) state return-c)
            (if (pair? (cdr statements));Else
                (Mstate (cadr statements) state return-c)
                state))))

(interpret "test")
