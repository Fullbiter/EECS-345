; David Pendergast
; Joel Kalos
; Kevin Nash

(load "simpleParser.scm")
(load "state.scm")
 
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; returns the numerical value of given expression.
(define M_value
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((symbol? expression) (state-get expression s))
      ((eq? (operator expression) '+) (+ (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '-) (- (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '*) (* (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '/) (quotient (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '%) (remainder (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      (else (error 'unknown expression "unknown expression")))))

; returns the boolean value of given expression.
(define M_boolean
  (lambda (expression s)
    (cond
      ((number? expression) expression)
      ((symbol? expression) (state-get expression s))
      ((eq? (operator expression) '==) (equal? (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '!=) (not (equal? (M_value (operand1 expression) s) (M_value (operand2 expression) s))))
      ((eq? (operator expression) '<) (< (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '>) (> (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '<=) (<= (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '>=) (>= (M_value (operand1 expression) s) (M_value (operand2 expression) s)))
      ((eq? (operator expression) '&&) (and (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '||) (or (M_boolean (operand1 expression) s) (M_boolean (operand2 expression) s)))
      ((eq? (operator expression) '!) (not (M_boolean (operand1 expression) s)))
      (else (error 'unknown expression "unknown expression")))))

; takes the filename of a valid program, and returns that program's return value.
(define execfile
  (lambda (filename)
    (interpret (parser filename) '(()()))))

; takes a parse tree, returns the return value of the program. 
(define interpret
  (lambda (parsetree s)
    (cond
      ((number? s) s) ; return value reached
      ((null? parsetree) (error "parse tree reached no return statement."))
      (else (interpret (cdr parsetree) (M_state (car parsetree) s))))))

; returns the state which results from executing the given statement on the given state.
; if the statement results in a value being returned, then that value is returned instead
(define M_state
  (lambda (stmt s)
    (cond
      ((declare? stmt) (M_declare (cadr stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (cadr stmt) (caddr stmt) s))
      ((assign? stmt) (M_assign (cadr stmt) (caddr stmt) s))
      ((if? stmt) (M_if (cadr stmt) (caddr stmt) s))
      ((if_with_else? stmt) (M_if_else (cadr stmt) (caddr stmt) (cadddr stmt) s))
      ((while? stmt) (M_while (cadr stmt) (caddr stmt) s))
      ((return? stmt) (M_return (cadr stmt) s))
      (else (error stmt "unknown statement.")))))

(define M_declare
  (lambda (variable s)
    (state-declare variable s)))

(define M_declare_with_assign
  (lambda (variable expression s)
    (state-assign variable (M_value expression (state-declare variable s)) (state-declare variable s))))

(define M_assign
  (lambda (variable expression s)
    (state-assign variable (M_value expression s) s)))

; returns a value (number), not a state
(define M_return
  (lambda (expression s)
    (M_value expression s)))

(define M_if_else
  (lambda (condition then-stmt else-stmt s)
   (if (M_boolean condition s)
        (M_state then-stmt s)
        (M_state else-stmt s))))

(define M_if
  (lambda (condition then-stmt s)
    (if (M_boolean condition s)
        (M_state then-stmt s)
        s)))

(define M_while
  (lambda (condition loop-body s)
    (if (M_boolean condition s)
        (M_while condition  loop-body (M_state loop-body s))
        s)))

; the following functions determine what kind of statement they are passed.
(define declare?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'var (car stmt))
        #f)))

(define declare_with_assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'var (car stmt))
        #f)))

(define assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? '= (car stmt))
        #f)))

(define return?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'return (car stmt))
        #f)))

(define if?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'if (car stmt))
        #f)))

(define if_with_else?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'if (car stmt))
        #f)))

(define while?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'while (car stmt))
        #f)))

; tests
(M_state '(var x) '(()()))
(M_state '(var x 10) '((y z)(15 40)))
(M_state '(= x 20) '((x) (10)))
(M_state '(= x 20) '((y x z) (0 () 6)))
(M_state '(while (< i 10) (= i (+ i x))) '((i x)(0 3)))
(M_state '(if (< x 2) (= x 2)) '((x)(1)))
(M_state '(if (>= x 2) (= x 7) (= x (+ x 1))) '((x)(0)))

; running the sample file.
(execfile "testfile.txt")