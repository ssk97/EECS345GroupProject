;Ron Weber and Steven Knipe
(load "classParser.scm")

(define interpret
  (lambda (filename className)
    (let ([classList (makeClassList (parser filename) '(()))])
      (outputNice
       (call_function
        'main ;For now static functions are the same as dynamic functions
        (getMethod 'main (findVar (string->symbol className) classList))
        () ;No args.  It's main.
        '(()) ;No initial state
        (lambda (x y) (error "Throw not in try"))
        '() ;We don't do static methods, so we pass an invalid object and hope noone looks anything up.
        (findVar (string->symbol className) classList)
        '()
        (findVar (string->symbol className) classList)
        classList)))))

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
;Parent, substate of fields, substate of methods
;Now for code that deals with classes
(define classParent car)
(define classFields cadr)
(define classMethods caddr)
(define makeClass
  (lambda (extendsPair classList)
    (if (null? extendsPair)
        (list '() '() '())
        (list (findVar (cadr extendsPair) classList) '() '()))))
;adds the method to the current class
(define addMethod
  (lambda (name args fn class)
    (list (classParent class)
          (classFields class)
          (define_method name args fn (classMethods class)))))
;adds the field to the current class
(define addField
  (lambda (name value class)
    (list (classParent class)
          (addVar_sub name value (classFields class))
          (classMethods class))))
;getMethodLayer returns the layer of the object in which class resides
(define getMethodLayer
  (lambda (name obj)
    (cond
      ((null? obj) (error "Method not found."))
      ((varExists_sub name (classMethods obj)) obj)
      (else (getMethodLayer name (classParent obj))))))
;getMethod returns the method with the given name
(define getMethod
  (lambda (name class)
    (cond
      ((null? class) (error "Method not found."))
      ((varExists_sub name (classMethods class)) (findVar_sub name (classMethods class)))
      (else (getMethod name (classParent class))))))
;getField returns the field with the given name
(define getField
  (lambda (name obj)
    (cond
      ((null? obj) (error "Field not found."))
      ((varExists_sub name (classFields obj)) (findVar_sub name (classFields obj)))
      (else (getField name (classParent obj))))))
;setField sets the field with the given name to the given value
(define setField
  (lambda (name value obj)
    (cond
      ((null? obj) (error "Field not found."))
      ((varExists_sub name (classFields obj)) (setVar_sub name value (classFields obj)))
      (else (setField name value (classParent obj))))))
;an object is just a copy of the class with all the boxes replaced with new ones
(define newObject
  (lambda (class)
    (if (null? class) '()
        (list (newObject(classParent class)) (newBoxes (classFields class)) (classMethods class) ))))
(define newBoxes
  (lambda (substate)
    (if (null? substate) '()
    (cons (cons (caar substate) (box (unbox (cdar substate)))) (newBoxes (cdr substate))))))
    
;We have to store both the compile type of objects and the object itself.
;Here are some helper functions for that.
(define makeObjectEntry
  (lambda (className classList)
    (newObject (findVar className classList))))

;interprets code in parsetree
(define interpreter
    (lambda (parsetree state return-c break-c continue-c throw-c normal-c this class classList)
        (cond
         ((null? parsetree) (normal-c state)) ;if you're at the end of your parsetree and haven't returned, return the full current state
         (else (Mstate (car parsetree) state return-c break-c continue-c throw-c
                       (lambda (v)
                         (interpreter (cdr parsetree) v return-c break-c continue-c throw-c normal-c this class classList))
                       this class classList)))))

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
    (if (null? parseTree)
        currentClass
        (interpreter-class (cdr parseTree) (classBody (car parseTree) currentClass)))))

;Interprets statements, and adds a stateEnd to all of the continuations
;except for the return continuation, since we don't care about the state after that.
;requires the calling function to call stateBegin on the state first (this is so that Catch can add the thrown value onto the stack)
(define interpret_in_new_layer
    (lambda (statements state return-c break-c continue-c throw-c normal-c this class classList)
        (interpreter statements state return-c
            (lambda (v) (break-c (stateEnd v)))
            (lambda (v) (continue-c (stateEnd v)))
            (lambda (v v2) (throw-c (stateEnd v) v2))
            (lambda (v) (normal-c (stateEnd v)))
            this class classList)))

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
;define_method defines a function except its initial state is empty, and it only updates a substate
(define define_method
  (lambda (name args fn substate)
    (addVar_sub name (list args '() fn) substate)))
;returns a substate containing all of the args using state
;TODO- allow for by-reference/box rather than by-value passing.
;Throw-c needed for function chaining if one of the arguments throws. 
(define evalArgs (lambda (args argvals state throw-c this class classList)
  (cond
    ((and (null? args) (null? argvals)) '())
    ((or (null? args) (null? argvals)) (error "Argument count does not match function definition"))
    (else (addVar_sub (car args) (Mvalue (car argvals) state throw-c this class classList) (evalArgs (cdr args) (cdr argvals) state throw-c this class classList))))
  ))
  
;Call a function/method, after we know what exactly it is we're calling
;Create a new state from the state saved inside the function, and add the function itself to that state
;name = The name of the function to call.  (Only needed for inner functions that might recurse). func = The function to call; args = A list of the variables sent as arguments to this function call; state = The state as the function is called; Throw-c = where to go when this function throws
;newThis = the object on which the method is called.  newClass = The class that newThis was called with.
;currentThis and currentClass are needed for arg evaluations.
;This = the layer we're currently on, class = the base layer
(define call_function
  (lambda (name func args state throw-c newThis newClass currentThis currentClass classList)
    (call/cc
     (lambda (return-c)
       (interpreter
        (caddr func);function
        (addVar 'this newThis (cons (addVar_sub name func (evalArgs (car func) args state throw-c currentThis currentClass classList)) (cadr func)));new state, plus recursion opportunity, plus this.
        return-c ;Where return goes
        (lambda(x)(error "Break not in loop.")) ;Break
        (lambda(x)(error "Continue not in loop.")) ;Continue
        (lambda(v thrown)(throw-c state thrown)) ;Like return, we throw out our manufactured state and return to the state above.
        (lambda(v) v) ;Exits without return, provide whole state for debugging
        newThis
        newClass
        classList)))))

;Adds the entry for the class defined by statement to classList
(define classEntry
  (lambda (statement classList)
    (if (eq? (operator statement) 'class) ;Make sure this is actually a class statment we're looking at.
        (addVar (operand1 statement) (interpreter-class (operand3 statement) (makeClass (operand2 statement) classList)) classList)
        (error "Statement other than class at top level"))))

;Evaluates a single statement in a class body and adds it to class.
(define classBody
  (lambda (statement class)
    (cond
      ((eq? (operator statement) 'function)  (addMethod (operand1 statement) (operand2 statement) (operand3 statement) class))
      ((eq? (operator statement) 'var)  (addField (operand1 statement) (Mvalue (operand2-or-empty statement) '() (lambda (x y) (error "Throw not in try")) '() '() '()) class)) ;calls Mvalue with blanks for basically everything- can only evaluate constants
      ((eq? (operator statement) 'static-function)  (addMethod (operand1 statement) (operand2 statement) (operand3 statement) class)))));Pretend a static function is a normal function.

;returns the new state after evaluating statement
(define Mstate
  (lambda (statement state return-c break-c continue-c throw-c normal-c this class classList)
    (cond
      ((eq? (operator statement) 'begin)    (interpret_in_new_layer (cdr statement) (stateBegin state) return-c  break-c continue-c throw-c normal-c this class classList))
      ((eq? (operator statement) 'return)   (return-c (Mvalue (operand1 statement) state throw-c this class classList)))
      ((eq? (operator statement) 'var)      (normal-c (addVar (operand1 statement) (Mvalue (operand2-or-empty statement) state throw-c this class classList) state)))
      ((eq? (operator statement) '=)        (begin (set_something (operand1 statement) (Mvalue (operand2 statement) state throw-c this class classList) state throw-c this class classList) (normal-c state)))
      ((eq? (operator statement) 'if)       (Mstate_if (operand1 statement) (cddr statement) state return-c break-c continue-c throw-c normal-c this class classList)) ;cddr can have 1 or 2 statements in it: if 2 then it has an 'else' case.
      ((eq? (operator statement) 'while)    (Mstate_while (operand1 statement) (operand2 statement) state return-c normal-c continue-c throw-c normal-c this class classList))
      ((eq? (operator statement) 'try)      (Mstate_try (operand1 statement) (operand2 statement) (operand3 statement) state return-c break-c continue-c throw-c normal-c this class classList))
      ((eq? (operator statement) 'throw)    (throw-c state (Mvalue (operand1 statement) state throw-c this class classList)))
      ((eq? (operator statement) 'continue) (continue-c state))
      ((eq? (operator statement) 'break)    (break-c state))
      ((eq? (operator statement) 'function) (normal-c (define_function (operand1 statement) (operand2 statement) (operand3 statement) state)))
      ((eq? (operator statement) 'funcall)  (begin (call_something (operand1 statement) (cddr statement) state throw-c this class classList) (normal-c state)))
      (else (normal-c state))
    )))
(define set_something
  (lambda (name value state throw-c this class classList)
    (cond
      ((and (symbol? name) (varExists name state)) (setVar name value state))
      ((symbol? name) (setField name value this))
      (else (setField (operand2 name) value (objOfDot name state throw-c this class classList))))))
(define find_something
  (lambda (name state throw-c this class classList)
    (cond
      ((and (symbol? name) (varExists name state)) (findVar name state))
      ((symbol? name) (getField name this))
      (else (getField (operand2 name) (objOfDot name state throw-c this class classList))))))
;calls either a method or a function
;depending on what's possible. Evaluates name to a function
(define call_something
  (lambda (name args state throw-c this class classList)
    (cond
      ((and (symbol? name) (varExists name state)) (call_function name (findVar name state)args state throw-c this class this class classList));It's a function in state
      ((eq? name 'super) (call_function name (getMethod name (classParent this)) args state throw-c (getMethodLayer name (classParent this)) class this class classList))
      ((symbol? name) (call_function name (getMethod name class) args state throw-c (getMethodLayer name class) class this class classList)) ;It's a method of this
      ((and (eq? (operator name) 'dot) (eq? (operand1 name) 'super))       (call_function (operand2 name) (getMethod (operand2 name) (classParent this)) args state throw-c (getMethodLayer (operand2 name) (classParent this)) class this class classList)) ;Calling super.method()
      ((eq? (operator name) 'dot) (let* ;It must be a method of another object
        ([obj (Mvalue_function (operand1 name) state throw-c this class classList)]
        [func (getMethod (operand2 name) obj)])
          (call_function (operand2 name) func args state throw-c (getMethodLayer (operand2 name) obj) obj this class classList)))
      (else (error "function call could not resolve")))))
;Returns the object refered to by statement.
(define Mvalue_function
  (lambda (statement state throw-c this class classList)
    (cond
      ((eq? statement 'this) class)
      ((and (list? statement) (eq? (operator statement) 'dot)) (classOfDot (operand2 statement) (Mvalue (operand1 statement) state throw-c this class classList))) ;Look up the method on the object
      (else (Mvalue statement state throw-c this class classList))))) ;It's either a function call or new.  Throw this down to Mvalue and let them take care of this.

;Returns the object on the left of the dot if statement is a dot.  Returns this otherwise
(define objOfDot
  (lambda (statement state throw-c this class classList)
    (cond
      ((not (pair? statement)) this) ;It's definently not a dot
      ((not (eq? (operator statement) 'dot)) this) ;It's not a dot.
      ((eq? (operand1 statement) 'super) (error "super on field")) ;Super is applied to a field. Problem.
      (else (Mvalue (operand1 statement) state throw-c this class classList))))) ;Do some evaluatin'

;returns the boolean value of statement (or unknown value that could return a boolean)
;Note: This may also evalute function calls that don't necessarily return booleans.
;Also takes throw-c in case of a function call
(define Mboolean
  (lambda (statement state throw-c this class classList)
    (cond
      ((eq? statement 'true) #t)
      ((eq? statement 'false) #f)
      ((symbol? statement) (find_something statement state throw-c this class classList));variable
      ((eq? (operator statement) '==) (eq? (Mvalue (operand1 statement) state throw-c this class classList) (Mvalue (operand2 statement) state throw-c this class classList)))
      ((eq? (operator statement) '!=) (not (eq? (Mvalue (operand1 statement) state throw-c this class classList) (Mvalue (operand2 statement) state throw-c this class classList))))
      ((eq? (operator statement) '>)  (> (Mvalue (operand1 statement) state throw-c this class classList) (Mvalue (operand2 statement) state throw-c this class classList)))
      ((eq? (operator statement) '<)  (< (Mvalue (operand1 statement) state throw-c this class classList) (Mvalue (operand2 statement) state throw-c this class classList)))
      ((eq? (operator statement) '>=) (>= (Mvalue (operand1 statement) state throw-c this class classList) (Mvalue (operand2 statement) state throw-c this class classList)))
      ((eq? (operator statement) '<=) (<= (Mvalue (operand1 statement) state throw-c this class classList) (Mvalue (operand2 statement) state throw-c this class classList)))

      ((eq? (operator statement) '&&) (and (Mboolean (operand1 statement) state throw-c this class classList) (Mboolean (operand2 statement) state throw-c this class classList)))
      ((eq? (operator statement) '||) (or (Mboolean (operand1 statement) state throw-c this class classList) (Mboolean (operand2 statement) state throw-c this class classList)))
      ((eq? (operator statement) '!)  (not (Mboolean (operand1 statement) state throw-c this class classList)))
      ((eq? (operator statement) '=) (set_something (operand1 statement) (Mvalue (operand2 statement) state throw-c this class classList) state throw-c this class classList))
      ((eq? (operator statement) 'funcall)  (call_something (operand1 statement) (cddr statement) state throw-c this class classList))
      ((eq? (operator statement) 'dot) (getField (operand2 statement) (objOfDot statement state throw-c this class classList)))
      (else (error "Value/Boolean unable to be evaluated"))
    )))

;returns the value of expr
;Also takes throw-c in case of a function call
;Cannot be used to find methods (it only looks for fields in case of the dot operator)
;Function and method calls, however, are fine.
(define Mvalue
  (lambda (expr state throw-c this class classList)
    (cond
     ((null? expr) expr)
     ((number? expr) expr)
     ((eq? expr 'true) #t)
     ((eq? expr 'false) #f)
     ((eq? expr 'this) this)
     ((symbol? expr) (find_something expr state throw-c this class classList));variable
     ((eq? (operator expr) 'new) (makeObjectEntry (operand1 expr) classList)) ;New statement.  Make a new object.
     ((eq? (operator expr) '+) (+ (Mvalue (operand1 expr) state throw-c this class classList) (Mvalue (operand2 expr) state throw-c this class classList)))
     ((eq? (operator expr) '*) (* (Mvalue (operand1 expr) state throw-c this class classList) (Mvalue (operand2 expr) state throw-c this class classList)))
     ((eq? (operator expr) '/) (quotient (Mvalue (operand1 expr) state throw-c this class classList) (Mvalue (operand2 expr) state throw-c this class classList)))
     ((eq? (operator expr) '-) (if (eq? (length expr) 3)
				   (- (Mvalue (operand1 expr) state throw-c this class classList) (Mvalue (operand2 expr) state throw-c this class classList))
				   (- (Mvalue (operand1 expr) state throw-c this class classList))));unary - operator
     ((eq? (operator expr) '%) (remainder (Mvalue (operand1 expr) state throw-c this class classList) (Mvalue (operand2 expr) state throw-c this class classList)))
     (else (Mboolean expr state throw-c this class classList))
     )))

;helper function for while loops
(define Mstate_while
    (lambda (condition statement state return-c break-c continue-c throw-c normal-c this class classList)
        (if (Mboolean condition state throw-c this class classList)
            (Mstate statement state return-c break-c
                    (lambda (v) (Mstate_while condition statement v return-c break-c continue-c throw-c normal-c this class classList));Continue loop
                    throw-c
                    (lambda (v) (Mstate_while condition statement v return-c break-c continue-c throw-c normal-c this class classList)) this class classList);normally continue loop
            (normal-c state))));end of loop
;helper function for if conditions (possible else)
;statements is a list that is either 1 long (if only) or 2 long (if, else)
(define Mstate_if
    (lambda (condition statements state return-c break-c continue-c throw-c normal-c this class classList)
        (if (Mboolean condition state throw-c this class classList)
            (Mstate (car statements) state return-c break-c continue-c throw-c normal-c this class classList)
            (if (pair? (cdr statements));Else is a pair
                (Mstate (cadr statements) state return-c break-c continue-c throw-c normal-c this class classList)
                (normal-c state)))));if no else, just return the state
;helper function for try-catch block
(define Mstate_try
  (lambda (tryBody catch finally state return-c break-c continue-c throw-c normal-c this class classList)
    (let* ((execute-finally
      (lambda(v) (if (null? finally)
                     (normal-c v) ;No finally, just continue execution
                     (interpret_in_new_layer (cadr finally) (stateBegin v) return-c break-c continue-c throw-c normal-c this class classList))))
     (execute-catch
      (lambda(v thrown) (if (null? catch)
                     (execute-finally v) ;no catch, just go straight to finally
                     (interpret_in_new_layer (caddr catch) (addVar (caadr catch) thrown (stateBegin v)) return-c break-c continue-c throw-c execute-finally this class classList)))))
      (interpret_in_new_layer tryBody (stateBegin state) return-c break-c continue-c execute-catch execute-finally this class classList))));try block
                           

;(interpret "test" "C");run the code
