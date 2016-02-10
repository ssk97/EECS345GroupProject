(define operator cadr)
(define operand1 car)
(define operand2 caddr)

(define M_value
  (lambda (expr)
    (cond
     ((number? expr) expr)
     ((eq? (operator expr) '+) (+ (M_value (operand1 expr)) (M_value (operand2 expr))))
     ((eq? (operator expr) '*) (* (M_value (operand1 expr)) (M_value (operand2 expr))))
     ((eq? (operator expr) '/) (quotient (M_value (operand1 expr)) (M_value (operand2 expr))))
     ((eq? (operator expr) '-) (- (M_value (operand1 expr)) (M_value (operand2 expr))))
     ((eq? (operator expr) '%) (remainder (M_value (operand1 expr)) (M_value (operand2 expr))))
     (else (error 'unknown "unknown expression")))))

(define Mstate_while
  (lambda (condition statement state)
    (if (M_boolean condition state)
	(Mstate_while condition statement (Mstate_statement statement state))
	state)))
