;;; Example 1
(let ((a 2) ; Type: Integer
      (b 3) ; Type: Integer
      (c 4) ; Type: Integer
      (d 1) ; Type: Integer
      (x 5) ; Type: Integer
      (y 6) ; Type: Integer
      (z 2) ; Type: Integer
      (p 3) ; Type: Integer
      (q 4) ; Type: Integer
      (r 5)) ; Type: Integer
  (format t "Example 1:~%Result 1: ~a~%" (* (+ a b) (- c d))) ; Returns 15 formula: (a + b) * (c - d) 
  (format t "Result 2: ~a~%" (/ (* 2 (+ x y)) (- 3 z))) ; Returns 22  formula: 2 * (x + y) / (3 - z)          
  (format t "Result 3: ~a~%" (sqrt (/ (+ (expt p 2) (expt q 2)) r)))) ; Returns 2.236068 formula: sqrt((p^2 + q^2) / r)


;;; Example 2
(let ((a 2) ; Define variable a and assign it 2
      (b 3)) ; Define variable b and assign it 3
  (format t "~%Example 2 QUOTE:~%Result: ~a~%" (quote (+ a b)))) ; Print unevaluated form (+ a b) to console
                                            ; Quote special form prevents evaluation of form
                                            ; Returns (+ A B)


;;; Example 3
#|
This code demonstrates the use of the and and or special forms in Lisp.

The and special form returns the first argument that evaluates to nil,
 or the last argument if all arguments evaluate to non-nil.

The or special form returns the first argument that evaluates to non-nil, 
 or nil if all arguments evaluate to nil.
|#

(let ((result-1 (and 1 2 3)) ; Type: Integer
      (result-2 (and 0 1 nil (+ 3 4) 3)) ; Type: Nil
      (result-3 (and 0 1 2 (* 3 3))) ; Type: Integer
      (result-4 (or 10 (* 3 3) 2 1)) ; Type: Integer
      (result-5 (or (> 5 6) (< 3 4) (= 1 2)))) ; Type: T
  (format t "~%Example 3 AND/OR:~%Result 1: ~a~%" result-1) ; Returns 3
  (format t "Result 2: ~a~%" result-2) ; Returns nil
  (format t "Result 3: ~a~%" result-3) ; Returns 9
  (format t "Result 4: ~a~%" result-4) ; Returns 10
  (format t "Result 5: ~a~%" result-5)) ; Returns T


;;; Example 4
#|
Rules for if statements:

- The first expression evaluates to nil, so the second expression is evaluated and its result is returned.
- The second expression evaluates to nil, so the third expression is evaluated and its result is returned.
- The third expression evaluates to non-nil, so the fourth expression is not evaluated and its result is returned.

Syntax for the if statement:
(if test-form then-form else-form?)

The test-form is evaluated, and if it returns a non-nil value, the then-form is evaluated and its result is returned.
 Otherwise, the else-form is evaluated and its result is returned. If the else-form is not provided, nil is returned.
|#

(format t "~%Example 4 If Statements:~%")

(if nil 4 (* 3 2)) ;==> 6
(format t "Result 1: ~a~%" (if nil 4 (* 3 2)))

(if (and 0 1 nil (+ 3 4) 3) ; Notice: indentation enhances readability
  3
  10) ;==> 10
(format t "Result 2: ~a~%" (if (and 0 1 nil (+ 3 4) 3) 3 10))

(if (or 0 (* 3 3) 2 1)
  (+ 5 5)
  (* 10 10)) ;==> 10
(format t "Result 3: ~a~%" (if (or 0 (* 3 3) 2 1) (+ 5 5) (* 10 10)))


;;; Example 5:
(format t "~%Example 5:~%")

#|
Rules for input code:

- The first expression defines a lambda function that takes one argument and returns the square of that argument.
- The second expression applies the lambda function to the argument 4, which returns 16.
- The third expression applies a lambda function that takes two arguments and returns the sum of the product of the arguments and the first argument.

Documentation:

The lambda special form is used to define anonymous functions in Lisp. 
It takes a list of parameters and a body of expressions, and returns a function object that can be applied to arguments using the function application form.

The function application form is used to apply a function to arguments in Lisp. 
It takes a function object and a list of arguments, and returns the result of applying the function to the arguments.

Steps:

1. Define a lambda function that takes one argument and returns the square of that argument.
2. Apply the lambda function to the argument 4, which returns 16.
3. Apply a lambda function that takes two arguments and returns the sum of the product of the arguments and the first argument.

Types:

The lambda function in the first expression takes an integer argument and returns an integer result.
The lambda function in the third expression takes two integer arguments and returns an integer result.
|#

; (lambda (x) (* x x))        ;==> #<FUNCTION (LAMBDA (X)) {535F802B}> - this is a special form
(format t "Lambda expression: ~a~%" (lambda (x) (* x x)))
(format t "Result 1: ~a~%" ((lambda (x) (* x x)) 4))          ;==> 16 - this is a function application form

((lambda (x y) (+ (* x y) x)) 3 1)         ;==> 6 - this is also function application form
(format t "Result 2: ~a~%" ((lambda (x y) (+ (* x y) x)) 3 1))

;;; Example 6:
(format t "~%Example 6:~%")

#|
Rules for input code:

- The let special form is used to define local variables in Lisp. 
It takes a list of variable bindings and a body of expressions,
 and returns the result of evaluating the last expression in the body.
- The let* special form is similar to let, but it evaluates each binding in order, 
so that each binding can refer to the previous bindings.

Steps:

1. Define a let expression that binds two variables and returns their sum.
2. Define a let* expression that binds two variables, where the second variable depends on the first, and returns their sum.

Types:

The let and let* expressions in this example both bind integer variables and return integer results.

Syntax:

(let ((var1 val1) (var2 val2) ...) body)

(let* ((var1 val1) (var2 val2) ...) body)
|#

(let ((x 1) ; Type: Integer
      (y 2)) ; Type: Integer
  (format t "Result 1: ~a~%" (+ x y))) ;==> 3

(let* ((x 1) ; Type: Integer
       (y (+ x 1))) ; Type: Integer
  (format t "Result 2: ~a~%" (+ x y))) ;==> 3


;;; Example 7:
(format t "~%Example 7:~%")

#|
Rules for input code:

- The defun special form is used to define named functions in Lisp. 
It takes a function name, a list of parameters, and a body of expressions, 
and returns a function object that can be applied to arguments using the function application form.

- The function special form is used to define anonymous functions in Lisp. 
It takes a list of parameters and a body of expressions, and returns a function object that can be applied to arguments using the function application form.

- The apply function is used to apply a function to a list of arguments in Lisp. 
It takes a function object and a list of arguments, 
and returns the result of applying the function to the arguments.

Steps:

1. Define a named function that takes one argument and returns the square of that argument.
2. Define an anonymous function that takes one argument and returns the square of that argument.
3. Bind the + function to a variable.
4. Use the named function to compute the square of 3.
5. Use the anonymous function to compute the square of 3.
6. Use the apply function to compute the sum of a list of integers.

Types:

The square function in this example takes an integer argument and returns an integer result.
The anonymous function in this example takes an integer argument and returns an integer result.
The + function in this example takes any number of integer arguments and returns an integer result.

Syntax:

(defun function-name (parameter-list) body)

(function (lambda (parameter-list) body))

(apply function args)
|#

(defun square (x) ; Define a named function that takes one argument
  (* x x)) ; Return the square of the argument

(format t "Result 1: ~a~%" (square 3)) ;==> 9

(let ((f (function (lambda (x) (* x x))))) ; Define an anonymous function and bind it to variable f
  (format t "Result 2: ~a~%" (funcall f 3))) ;==> 9

(let ((f (function +))) ; Bind the + function to variable f
  (format t "Result 3: ~a~%" (apply f '(1 2 3)))) ;==> 6



; -----------------------------------------------------------------------
; Run code 
;(load "lec_1.lisp")

