; EECS 345 - Programming Project 1
; 
; David Pendergast
; Joel Kalos
; Kevin Nash

(load "simpleParser.scm")
(load "state.scm")

; Defines the elements of an expression in prefix notation
(define operator car)
(define operand1 cadr)
(define operand2 caddr)

; Returns the numerical value of given expression
(define M_value
  (lambda (expr s)
    (cond
      ((number? expr) expr)
      ((boolean? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((symbol? expr) (state-get expr s))
      ((and (eq? (operator expr) '-)
            (null? (cddr expr))) (* (M_value (operand1 expr) s) -1))
      ((eq? (operator expr) '+) (+ (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '-) (- (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '*) (* (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '/) (quotient (M_value (operand1 expr) s)
                                          (M_value (operand2 expr) s)))
      ((eq? (operator expr) '%) (remainder (M_value (operand1 expr) s)
                                           (M_value (operand2 expr) s)))
      (else (error "unknown expression" expr)))))

; Returns the boolean value of given expression
(define M_boolean
  (lambda (expr s)
    (cond
      ((number? expr) expr)
      ((boolean? expr) expr)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #f)
      ((symbol? expr) (state-get expr s))
      ((eq? (operator expr) '==) (equal? (M_value (operand1 expr) s)
                                         (M_value (operand2 expr) s)))
      ((eq? (operator expr) '!=) (not (equal? (M_value (operand1 expr) s)
                                              (M_value (operand2 expr) s))))
      ((eq? (operator expr) '<) (< (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '>) (> (M_value (operand1 expr) s)
                                   (M_value (operand2 expr) s)))
      ((eq? (operator expr) '<=) (<= (M_value (operand1 expr) s)
                                     (M_value (operand2 expr) s)))
      ((eq? (operator expr) '>=) (>= (M_value (operand1 expr) s)
                                     (M_value (operand2 expr) s)))
      ((eq? (operator expr) '&&) (and (M_boolean (operand1 expr) s)
                                      (M_boolean (operand2 expr) s)))
      ((eq? (operator expr) '||) (or (M_boolean (operand1 expr) s)
                                     (M_boolean (operand2 expr) s)))
      ((eq? (operator expr) '!) (not (M_boolean (operand1 expr) s)))
      (else (error "unknown expression" expr)))))

; Returns true if the expression is a condition or truth value
(define condition?
  (lambda (expr s)
    (cond
      ((boolean? expr) #t)
      ((eq? 'true expr) #t)
      ((eq? 'false expr) #t)
      ((symbol? expr) (boolean? (state-get expr s)))
      ((not (list? expr)) #f)
      (else (or (eq? (operator expr) '==) (eq? (operator expr) '!=)
                (eq? (operator expr) '<) (eq? (operator expr) '>)
                (eq? (operator expr) '<=) (eq? (operator expr) '>=)
                (eq? (operator expr) '&&) (eq? (operator expr) '||)
                (eq? (operator expr) '!))))))
  
; Given the filename of a valid program, returns the return value of the program
(define execfile
  (lambda (filename)
    (interpret (parser filename) '(()()))))

; Given a parse tree, returns the return value of the program
(define interpret
  (lambda (parsetree s)
    (cond
      ((number? s) s) ; return value reached
      ((boolean? s) (if (eq? s #t) 'true 'false))
      ((null? parsetree) (error "parse tree reached no return statement"))
      (else (interpret (cdr parsetree) (M_state (car parsetree) s))))))

; Returns the state which results from executing the given statement OR
; Returns a value if the statement simplifies to a value
(define M_state
  (lambda (stmt s)
    (cond
      ((declare? stmt) (M_declare (cadr stmt) s))
      ((declare_with_assign? stmt) (M_declare_with_assign (cadr stmt)
                                                          (caddr stmt) s))
      ((assign? stmt) (M_assign (cadr stmt) (caddr stmt) s))
      ((if? stmt) (M_if (cadr stmt) (caddr stmt) s))
      ((if_with_else? stmt) (M_if_else (cadr stmt) (caddr stmt)
                                       (cadddr stmt) s))
      ((while? stmt) (M_while (cadr stmt) (caddr stmt) s))
      ((return? stmt) (M_return (cadr stmt) s))
      (else (error stmt "unknown statement")))))

; Declares a variable
(define M_declare
  (lambda (variable s)
    (state-declare variable s)))

; Declares a variable and assigns it a numerical or boolean value
(define M_declare_with_assign
  (lambda (var expr s)
    (if (condition? expr s)
        (state-assign var (M_boolean expr (state-declare var s))
                      (state-declare var s))
        (state-assign var (M_value expr (state-declare var s))
                      (state-declare var s)))))

; Assigns a numerical or boolean value to a variable
(define M_assign
  (lambda (var expr s)
    (if (condition? expr s)
        (state-assign var (M_boolean expr s) s)
        (state-assign var (M_value expr s) s))))

; Returns a numerical value, not a state
(define M_return
  (lambda (expr s)
    (if (condition? expr s)
        (M_boolean expr s)
        (M_value expr s))))

; Executes an if-else pair of statements according to the if condition
(define M_if_else
  (lambda (condition then-stmt else-stmt s)
   (if (M_boolean condition s)
        (M_state then-stmt s)
        (M_state else-stmt s))))

; Executes an if statement according to its condition
(define M_if
  (lambda (condition then-stmt s)
    (if (M_boolean condition s)
        (M_state then-stmt s)
        s)))

; Executes a while loop according to its condition
(define M_while
  (lambda (condition loop-body s)
    (if (M_boolean condition s)
        (M_while condition  loop-body (M_state loop-body s))
        s)))

; Returns true if given a statement that only declares a variable
(define declare?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'var (car stmt))
        #f)))

; Returns true if given a statement that declares a variable and assigns a value
(define declare_with_assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'var (car stmt))
        #f)))

; Returns true if given a statement that assigns a value to a declared variable
(define assign?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? '= (car stmt))
        #f)))

; Returns true if given a return statement
(define return?
  (lambda (stmt)
    (if (eq? (length stmt) 2)
        (eq? 'return (car stmt))
        #f)))

; Returns true if given an if statement that is NOT followed by an else
(define if?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'if (car stmt))
        #f)))

; Returns true if given an if statement that is followed by an else
(define if_with_else?
  (lambda (stmt)
    (if (eq? (length stmt) 4)
        (eq? 'if (car stmt))
        #f)))

; Returns true if a statement begins a while loop
(define while?
  (lambda (stmt)
    (if (eq? (length stmt) 3)
        (eq? 'while (car stmt))
        #f)))

; -----------
; State tests
; -----------
(M_state '(var x) '(()()))
(M_state '(var x 10) '((y z)(15 40)))
(M_state '(var x true) '((y z)(15 40)))
(M_state '(= x 20) '((x) (10)))
(M_state '(= x 20) '((y x z) (0 () 6)))
(M_state '(while (< i 10) (= i (+ i x))) '((i x)(0 3)))
(M_state '(if (< x 2) (= x 2)) '((x)(1)))
(M_state '(if (>= x 2) (= x 7) (= x (+ x 1))) '((x)(0)))

; --------------
; Language tests
; --------------
(display "Test 0: ") (equal? (execfile "test0.txt") 100)
(display "Test 1: ") (equal? (execfile "test1.txt") 150)
(display "Test 2: ") (equal? (execfile "test2.txt") -4)
(display "Test 3: ") (equal? (execfile "test3.txt") 10)
(display "Test 4: ") (equal? (execfile "test4.txt") 16)
(display "Test 5: ") (equal? (execfile "test5.txt") 220)
(display "Test 6: ") (equal? (execfile "test6.txt") 5)
(display "Test 7: ") (equal? (execfile "test7.txt") 6)
(display "Test 8: ") (equal? (execfile "test8.txt") 10)
(display "Test 9: ") (equal? (execfile "test9.txt") 5)
(display "Test 10: ") (equal? (execfile "test10.txt") -39)
; When enabled, tests 11-14 should produce specific errors
;(display "Test 11: ") (execfile "test11.txt") ; variable not declared
;(display "Test 12: ") (execfile "test12.txt") ; variable not declared
;(display "Test 13: ") (execfile "test13.txt") ; variable not initialized
;(display "Test 14: ") (execfile "test14.txt") ; variable already declared 
(display "Test 15: ") (equal? (execfile "test15.txt") 'true)
(display "Test 16: ") (equal? (execfile "test16.txt") 100)
(display "Test 17: ") (equal? (execfile "test17.txt") 'false)
(display "Test 18: ") (equal? (execfile "test18.txt") 'true)
(display "Test 19: ") (equal? (execfile "test19.txt") 128)
(display "Test 20: ") (equal? (execfile "test20.txt") 12)
; Tests 21-28 are expected to fail. The feature they test is not implemented.
;(display "Test 21: ") (equal? (execfile "test21.txt") 30)
;(display "Test 22: ") (equal? (execfile "test22.txt") 11)
;(display "Test 23: ") (equal? (execfile "test23.txt") 1106)
;(display "Test 24: ") (equal? (execfile "test24.txt") 12)
;(display "Test 25: ") (equal? (execfile "test25.txt") 16)
;(display "Test 26: ") (equal? (execfile "test26.txt") 72)
;(display "Test 27: ") (equal? (execfile "test27.txt") 21)
;(display "Test 28: ") (equal? (execfile "test28.txt") 164)
