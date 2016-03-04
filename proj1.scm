;Ron Weber and Steven Knipe
(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (outputNice
     (call/cc
      (lambda (return-c)
        (interpreter
         (parser filename)
         '(())
         return-c ;Where return goes
         (lambda(x)(error "Break not in loop.")) ;Break
         (lambda(x)(error "Continue not in loop.")) ;Continue
         (lambda(x y)(error "Throw not in try")) ;Throw
         (lambda (v) v))))))) ;Normal exit.  Ejects the whole state.

;converts #t and #f to 'true and 'false respectively
(define outputNice 
    (lambda (a)
        (cond
	 ((eq? a #t) 'true)
	 ((eq? a #f) 'false)
	 (else a))))

;dealing with variables

;state is a list of substates
;a substate is a list of pairs with the same scope
;the first in the pair is the varname. the second is either the value (number/bool) or empty list if undefined

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
	 ((null? substate) (error "Variable used before declared- findVar_sub"));We've gone too far, there are no substates left
	 ((not (eq? varname (caar substate))) (findVar_sub varname (cdr substate))) ;Didn't find it, iterate
	 ((null? (cdar substate)) (error "Variable used before assigned."))
	 (else (cdar substate)))))
     
;functions that work across entire state
;Check if a variable with varname as a name exists in the state at all
(define varExists
    (lambda (varname state)
        (cond
            ((null? state) #f)
            ((varExists_sub varname (car state)) #t)
            (else (varExists varname (cdr state))))))
;Adds varname to the topmost substate
;Takes a name, value, and state
(define addVar
    (lambda (varname value state)
        (cons (addVar_sub varname value (car state)) (cdr state))))
;returns state modified so that varname is set to value
;Takes a name, value, and state to modify
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
      (cdr state)))
  
;Primary doing stuff

(define interpreter
    (lambda (parsetree state return-c break-c continue-c throw-c normal-c)
        (cond
         ((null? parsetree) (normal-c state)) ;if you're at the end of your parsetree and haven't returned, return the full current state
         (else (Mstate (car parsetree) state return-c break-c continue-c throw-c
                       (lambda (v)
                         (interpreter (cdr parsetree) v return-c break-c continue-c throw-c normal-c)))))))

;Adds a new layer onto the state, interprets statements, and adds a stateEnd to all of the continuations
;except for the return continuation, since we don't care about the state after that.
(define interpret_in_new_layer
  (lambda (statements state return-c break-c continue-c throw-c normal-c)
    (interpreter statements (stateBegin state) return-c
                 (lambda (v) (break-c (stateEnd v)))
                 (lambda (v) (continue-c (stateEnd v)))
                 (lambda (v v2) (throw-c (stateEnd v) v2))
                 (lambda (v) (normal-c (stateEnd v))))))

(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define operand2-or-empty
  (lambda (l)
    (if (null? (cddr l)) '() (operand2 l))))

(define Mstate
    (lambda (statement state return-c break-c continue-c throw-c normal-c)
        (cond
         ((eq? (operator statement) 'begin) (interpret_in_new_layer (cdr statement) state return-c  break-c continue-c throw-c normal-c))
	 ((eq? (operator statement) 'return) (return-c (Mvalue (operand1 statement) state)))
	 ((eq? (operator statement) 'var) (normal-c (addVar (operand1 statement) (Mvalue (operand2-or-empty statement) state) state)))
	 ((eq? (operator statement) '=) (normal-c (setVar (operand1 statement) (Mvalue (operand2 statement) state) state)))
	 ((eq? (operator statement) 'if) (Mstate_if (operand1 statement) (cddr statement) state return-c break-c continue-c throw-c normal-c)) ;cddr can have 1 or 2 statements in it: if 2 then it has an 'else' case.
	 ((eq? (operator statement) 'while) (Mstate_while (operand1 statement) (operand2 statement) state return-c normal-c continue-c throw-c normal-c))
         ((eq? (operator statement) 'try) (Mstate_try (operand1 statement) (operand2 statement) (operand3 statement) state return-c break-c continue-c throw-c normal-c))
         ((eq? (operator statement) 'throw) (throw-c state (Mvalue (operand1 statement) state)))
         ((eq? (operator statement) 'continue) (continue-c state))
         ((eq? (operator statement) 'break) (break-c state))
	 (else (normal-c state))
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
    (lambda (condition statement state return-c break-c continue-c throw-c normal-c)
        (if (Mboolean condition state)
            (Mstate statement state return-c break-c
                    (lambda (v) (Mstate_while condition statement v return-c break-c continue-c throw-c normal-c));Continue
                    throw-c
                    (lambda (v) (Mstate_while condition statement v return-c break-c continue-c throw-c normal-c)))
            (normal-c state))))

(define Mstate_if
    (lambda (condition statements state return-c break-c continue-c throw-c normal-c)
        (if (Mboolean condition state)
            (Mstate (car statements) state return-c break-c continue-c throw-c normal-c)
            (if (pair? (cdr statements));Else
                (Mstate (cadr statements) state return-c break-c continue-c throw-c normal-c)
                (normal-c state)))))

    

(define Mstate_try
  (lambda (tryBody catch finally state return-c break-c continue-c throw-c normal-c)
    (let* ((execute-finally
            (lambda(v) (if (null? finally)
                           (normal-c v) ;No finally.
                           (interpret_in_new_layer (cadr finally) v return-c break-c continue-c throw-c normal-c))))
           (execute-catch
            (lambda(v thrown) (if (null? catch)
                           (execute-finally v)
                           (interpret_in_new_layer (caddr catch) (addVar (caadr catch) thrown v) return-c break-c continue-c throw-c execute-finally)))))
      (interpret_in_new_layer tryBody state return-c break-c continue-c execute-catch execute-finally))))
                           

(interpret "test")
