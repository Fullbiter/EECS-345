; Kevin Nash (kjn33)
; EECS 345, PE1

; Given a list of numbers, returns #t if the numbers are in non-decreasing order
(define inorder?
  (lambda (l)
    (cond
      ((or (null? l) (null? (cdr l))) #t)
      ((and (number? (cadr l))(> (car l) (cadr l))) #f)
      (else (inorder? (cdr l))))))

; Given two vectors, returns the dot product of the vectors
(define dotproduct
  (lambda (v1 v2)
    (cond
      ((or (null? v1) (null? v2)) 0)
      ((or (null? (cdr v1)) (null? (cdr v2))) (* (car v1) (car v2)))
      (else (+ (* (car v1) (car v2)) (dotproduct (cdr v1) (cdr v2)))))))

; Given a value and number of iterations, returns the square of the value
; using the given number of Newton's method iterations
;     Note: I apologize for the 126-column line
(define squareroot
  (lambda (n i)
    (if (< i 1)
        n
        (- (squareroot n (- i 1)) (/ (- (* (squareroot n (- i 1)) (squareroot n (- i 1))) n) (* 2 (squareroot n (- i 1))))))))

; Given a sequence of atoms and a subsequence of that sequence, returns the
; sequence having removed the subsequence
(define removesubsequence
  (lambda (sub seq)
    (cond
      ((or (null? sub) (null? seq)) seq)
      ((eq? (car sub) (car seq)) (removesubsequence (cdr sub) (cdr seq)))
      (else (cons (car seq) (removesubsequence sub (cdr seq)))))))

; Helper function for reverse*
(define (innerreverse l p)
  (cond
    ((null? l) p)
    ((list? (car l)) (innerreverse (cdr l) (cons (reverse* (car l)) p)))
    (else (innerreverse (cdr l) (cons (car l) p)))))

; Given a list, returns a list with all atoms and inner lists reversed
(define reverse*
  (lambda (l)
    (innerreverse* l '())))

; Given a list of lists, returns the first atom that appears in the list,
; regardless of how nested it is
(define first*
  (lambda (l)
    (cond
      ((null? l) l)
      ((pair? (car l)) (first* (car l)))
      (else (car l)))))

; Given a list of lists, returns the last atom that appears in the list,
; regardless of how nested it is
(define last*
  (lambda (l)
    (cond
      ((not (pair? l)) l)
      ((null? (cdr l)) (last* (car l)))
      (else (last* (cdr l))))))

; asd
(define numorder*?
  (lambda (l)
    (cond
      ((or (null? l) (null? (cdr l))) #t)
      ((and (number? (cadr l))(> (car l) (cadr l))) #f)
      (else (inorder? (cdr l))))))

; Given a matrix, returns the given matrix without its first column
; Necessary for vectormult
(define rmcolumn
  (lambda (m)
    (if (null? m)
        m
        (cons (cdar m) (rmcolumn (cdr m))))))

; Given a matrix, returns the first column in the matrix
; Necessary for vectormult
(define getcolumn
  (lambda (m)
    (if (null? m)
        m
        (cons (caar m) (getcolumn (cdr m))))))

; Given a vector and a matrix, returns the product of the vector and the matrix
(define vectormult
  (lambda (v m)
    (if (or (null? m) (null? (car m)))
        '()
        (cons (dotproduct v (getcolumn m)) (vectormult v (rmcolumn m))))))

; Given two matrices, returns the product of the two matrices
(define matrixmultiply
  (lambda (m1 m2)
    (if (or (null? m1) (null? m2))
        '()
        (cons (vectormult (car m1) m2) (matrixmultiply (cdr m1) m2)))))