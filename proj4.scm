;Ron Weber and Steven Knipe
(load "classParser.scm")

(define interpret
  (lambda (filename className)
    (outputNice (call_function 'main () (call/cc
      (lambda (return-c)
        (interpreter
         (parser filename)
         '(())
         return-c ;Where return goes
         (lambda(x)(error "Break not in loop.")) ;Break
         (lambda(x)(error "Continue not in loop.")) ;Continue
         (lambda(x y)(error "Throw not in try")) ;Throw
         (lambda(v) v))));Exits without return, provide whole state for debugging
                               (lambda (x y) (error "Attempt to call main() threw exception")) ;This should never be needed, as it's only used if evaluating arguments throws an exception.
                               ))))

;converts #t and #f to 'true and 'false respectively
(define outputNice 
    (lambda (a)
        (cond
	 ((eq? a #t) 'true)
	 ((eq? a #f) 'false)
	 (else a))))

;code for dealing with states/variables

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
;Returns substate modified to have a new variable named varname in it
;Initially sets varname to value.  Errors if it already exists
(define addVar_sub
  (lambda (varname value substate)
    (if (varExists_sub varname substate)
	(error "Variable declared multiple times.")
        (cons (cons varname (box value)) substate))))
;modifies the value of the box in substate
;returns the value set to or errors out
(define setVar_sub
    (lambda (varname value substate)
	(cond
	 ((null? substate) (error "Variable assigned before declared- setVar_sub"))
	 ((eq? varname (caar substate)) (begin (set-box! (cdar substate) value) value))
	 (else (setVar_sub varname value (cdr substate))))))
;Returns the value associated with varname in substate
(define findVar_sub
    (lambda (varname substate)
	(cond
	 ((null? substate) (error "Variable used before declared- findVar_sub"));We've gone too far, there are no substates left
	 ((not (eq? varname (caar substate))) (findVar_sub varname (cdr substate))) ;Didn't find it, iterate
	 ((null? (unbox (cdar substate))) (error "Variable used before assigned."))
	 (else (unbox (cdar substate))))))
     
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
;returns the new state
(define addVar
    (lambda (varname value state)
        (cons (addVar_sub varname value (car state)) (cdr state))))
;modifies the boxes such that varname is set to value
;Takes a name, value, and state to modify
;returns the value it set to on success, errors out on failure
(define setVar
    (lambda (varname value state)
        (cond
            ((null? state) (error "Variable used before declared- setVar"))
            ((varExists_sub varname (car state)) (setVar_sub varname value (car state)))
            (else (setVar varname value (cdr state))))))
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
  
;end of code for dealing with states/variables

;The formula for our class will be:
;Parent, list of fields, list of methods
;Now for code that deals with classes
(define classParent car)
(define classInstanceFields cadr)
(define classMethods caddr)
(define makeClass
  (lambda (extendsPair classList)
    (if (null? extendsPair)
        (list '() '() '())
        (list (findVar (cadr extendsPair) classList) '() '()))))

(define objTruetype car)
(define objFieldValues cadr)
(define makeObject
  (lambda (className classList)
    (list (findVar className classList) '())))

;interprets code in parsetree
(define interpreter
    (lambda (parsetree state return-c break-c continue-c throw-c normal-c)
        (cond
         ((null? parsetree) (normal-c state)) ;if you're at the end of your parsetree and haven't returned, return the full current state
         (else (Mstate (car parsetree) state return-c break-c continue-c throw-c
                       (lambda (v)
                         (interpreter (cdr parsetree) v return-c break-c continue-c throw-c normal-c)))))))

;Creates a class list
;The top level parse tree should be a list of statements
(define makeClassList
  (lambda (parseTree classList)
    (if (null? parseTree)
        classList
        (makeClassList (cdr parseTree) (classEntry (car parseTree) classList)))))

;Adds information to a class
;Takes the parseTree of the class body and the current class to add things like function calls to.
;Returns the complete class.
(define interpreter-class
  (lambda (parseTree currentClass)
    currentClass))

;Interprets statements, and adds a stateEnd to all of the continuations
;except for the return continuation, since we don't care about the state after that.
;requires the calling function to call stateBegin on the state first (this is so that Catch can add the thrown value onto the stack)
(define interpret_in_new_layer
    (lambda (statements state return-c break-c continue-c throw-c normal-c)
        (interpreter statements state return-c
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

;function format: (args, state, function)
; args = list of names of arguments, state = Everything the function has access to except itself, function = The parse tree of the function.
(define define_function
  (lambda (name args fn state)
    (addVar name (list args state fn) state)))
;returns a substate containing all of the args using state
;TODO- allow for by-reference/box rather than by-value passing.
;Throw-c needed for function chaining if one of the arguments throws. 
(define evalArgs (lambda (args argvals state throw-c)
  (cond
    ((and (null? args) (null? argvals)) '())
    ((or (null? args) (null? argvals)) (error "Argument count does not match function definition"))
    (else (addVar_sub (car args) (Mvalue (car argvals) state throw-c) (evalArgs (cdr args) (cdr argvals) state throw-c))))
  ))
;Call a function.
;Create a new state from the state saved inside the function, and add the function itself to that state
;name = Name of the function; args = A list of the variables sent as arguments to this function call; state = The state as the function is called; Throw-c = where to go when this function throws,
(define call_function
  (lambda (name args state throw-c)
    (let ([func (findVar name state)])
     (call/cc
      (lambda (return-c)
        (interpreter
          (caddr func);function
          (cons (addVar_sub name func (evalArgs (car func) args state throw-c)) (cadr func));new state, plus recursion opportunity
          return-c ;Where return goes
          (lambda(x)(error "Break not in loop.")) ;Break
          (lambda(x)(error "Continue not in loop.")) ;Continue
          (lambda(v thrown)(throw-c state thrown)) ;Like return, we throw out our manufactured state and return to the state above.
          (lambda(v) v))))))) ;Exits without return, provide whole state for debugging

;Adds the entry for the class defined by statement to classList
(define classEntry
  (lambda (statement classList)
    (if (eq? (operator statement) 'class) ;Make sure this is actually a class statment we're looking at.
        (addVar (operand1 statement) (makeClass (operand2 statement) classList) classList)
        (error "Statement other than class at top level"))))

;Evaluates a single statement in a class body and adds it to class.
(define classBody
  (lambda (statement class)
    (cond
      ((eq? (operator statement) 'function)  (define_function (operand1 statement) (operand2 statement) (operand3 statement) (classMethods class))))))

;returns the new state after evaluating statement
(define Mstate
  (lambda (statement state return-c break-c continue-c throw-c normal-c)
    (cond
      ((eq? (operator statement) 'begin)    (interpret_in_new_layer (cdr statement) (stateBegin state) return-c  break-c continue-c throw-c normal-c))
      ((eq? (operator statement) 'return)   (return-c (Mvalue (operand1 statement) state throw-c)))
      ((eq? (operator statement) 'var)      (normal-c (addVar (operand1 statement) (Mvalue (operand2-or-empty statement) state throw-c) state)))
      ((eq? (operator statement) '=)        (begin (setVar (operand1 statement) (Mvalue (operand2 statement) state throw-c) state) (normal-c state)))
      ((eq? (operator statement) 'if)       (Mstate_if (operand1 statement) (cddr statement) state return-c break-c continue-c throw-c normal-c)) ;cddr can have 1 or 2 statements in it: if 2 then it has an 'else' case.
      ((eq? (operator statement) 'while)    (Mstate_while (operand1 statement) (operand2 statement) state return-c normal-c continue-c throw-c normal-c))
      ((eq? (operator statement) 'try)      (Mstate_try (operand1 statement) (operand2 statement) (operand3 statement) state return-c break-c continue-c throw-c normal-c))
      ((eq? (operator statement) 'throw)    (throw-c state (Mvalue (operand1 statement) state throw-c)))
      ((eq? (operator statement) 'continue) (continue-c state))
      ((eq? (operator statement) 'break)    (break-c state))
      ((eq? (operator statement) 'function) (normal-c (define_function (operand1 statement) (operand2 statement) (operand3 statement) state)))
      ((eq? (operator statement) 'funcall)  (begin (call_function (operand1 statement) (cddr statement) state throw-c) (normal-c state)))
      (else (normal-c state))
    )))

;returns the boolean value of statement (or unknown value that could return a boolean)
;Note: This may also evalute function calls that don't necessarily return booleans.
;Also takes throw-c in case of a function call
(define Mboolean
  (lambda (statement state throw-c)
    (cond
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      ((symbol? statement) (findVar statement state));variable
      ((eq? (operator statement) '==) (eq? (Mvalue (operand1 statement) state throw-c) (Mvalue (operand2 statement) state throw-c)))
      ((eq? (operator statement) '!=) (not (eq? (Mvalue (operand1 statement) state throw-c) (Mvalue (operand2 statement) state throw-c))))
      ((eq? (operator statement) '>)  (> (Mvalue (operand1 statement) state throw-c) (Mvalue (operand2 statement) state throw-c)))
      ((eq? (operator statement) '<)  (< (Mvalue (operand1 statement) state throw-c) (Mvalue (operand2 statement) state throw-c)))
      ((eq? (operator statement) '>=) (>= (Mvalue (operand1 statement) state throw-c) (Mvalue (operand2 statement) state throw-c)))
      ((eq? (operator statement) '<=) (<= (Mvalue (operand1 statement) state throw-c) (Mvalue (operand2 statement) state throw-c)))

      ((eq? (operator statement) '&&) (and (Mboolean (operand1 statement) state throw-c) (Mboolean (operand2 statement) state throw-c)))
      ((eq? (operator statement) '||) (or (Mboolean (operand1 statement) state throw-c) (Mboolean (operand2 statement) state throw-c)))
      ((eq? (operator statement) '!)  (not (Mboolean (operand1 statement) state throw-c)))
      ((eq? (operator statement) '=)        (setVar (operand1 statement) (Mvalue (operand2 statement) state throw-c) state))
      ((eq? (operator statement) 'funcall)  (call_function (operand1 statement) (cddr statement) state throw-c))
      (else (error "Value/Boolean unable to be evaluated"))
    )))

;returns the value of expr
;Also takes throw-c in case of a function call
(define Mvalue
  (lambda (expr state throw-c)
    (cond
     ((null? expr) expr)
     ((number? expr) expr)
     ((eq? expr 'true) #t)
     ((eq? expr 'false) #f)
     ((symbol? expr) (findVar expr state));variable
     ((eq? (operator expr) '+) (+ (Mvalue (operand1 expr) state throw-c) (Mvalue (operand2 expr) state throw-c)))
     ((eq? (operator expr) '*) (* (Mvalue (operand1 expr) state throw-c) (Mvalue (operand2 expr) state throw-c)))
     ((eq? (operator expr) '/) (quotient (Mvalue (operand1 expr) state throw-c) (Mvalue (operand2 expr) state throw-c)))
     ((eq? (operator expr) '-) (if (eq? (length expr) 3)
				   (- (Mvalue (operand1 expr) state throw-c) (Mvalue (operand2 expr) state throw-c))
				   (- (Mvalue (operand1 expr) state throw-c))));unary - operator
     ((eq? (operator expr) '%) (remainder (Mvalue (operand1 expr) state throw-c) (Mvalue (operand2 expr) state throw-c)))
     (else (Mboolean expr state throw-c))
     )))

;helper function for while loops
(define Mstate_while
    (lambda (condition statement state return-c break-c continue-c throw-c normal-c)
        (if (Mboolean condition state throw-c)
            (Mstate statement state return-c break-c
                    (lambda (v) (Mstate_while condition statement v return-c break-c continue-c throw-c normal-c));Continue loop
                    throw-c
                    (lambda (v) (Mstate_while condition statement v return-c break-c continue-c throw-c normal-c)));normally continue loop
            (normal-c state))));end of loop
;helper function for if conditions (possible else)
;statements is a list that is either 1 long (if only) or 2 long (if, else)
(define Mstate_if
    (lambda (condition statements state return-c break-c continue-c throw-c normal-c)
        (if (Mboolean condition state throw-c)
            (Mstate (car statements) state return-c break-c continue-c throw-c normal-c)
            (if (pair? (cdr statements));Else is a pair
                (Mstate (cadr statements) state return-c break-c continue-c throw-c normal-c)
                (normal-c state)))));if no else, just return the state
;helper function for try-catch block
(define Mstate_try
  (lambda (tryBody catch finally state return-c break-c continue-c throw-c normal-c)
    (let* ((execute-finally
      (lambda(v) (if (null? finally)
                     (normal-c v) ;No finally, just continue execution
                     (interpret_in_new_layer (cadr finally) (stateBegin v) return-c break-c continue-c throw-c normal-c))))
     (execute-catch
      (lambda(v thrown) (if (null? catch)
                     (execute-finally v) ;no catch, just go straight to finally
                     (interpret_in_new_layer (caddr catch) (addVar (caadr catch) thrown (stateBegin v)) return-c break-c continue-c throw-c execute-finally)))))
      (interpret_in_new_layer tryBody (stateBegin state) return-c break-c continue-c execute-catch execute-finally))));try block
                           

(interpret "test" "A");run the code
