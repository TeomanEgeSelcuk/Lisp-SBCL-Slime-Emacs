;;; Example 1:
(format t "~%Example 1:~%")

#|
Rules for input code:
- The code contains a function called `convert-to-letter-grade`.
- The function takes a single argument `numeric-grade`.
- The function uses the `case` special form to determine the letter grade based on the numeric grade.

Steps:
1. Define a function `convert-to-letter-grade` that takes a numeric grade.
2. Use the `case` special form to match the numeric grade to a letter grade.

Syntax:
- (defun convert-to-letter-grade (numeric-grade) ...)

|#

; Type: (function (number) (values (or string null)))
(defun convert-to-letter-grade (numeric-grade)
  (case (floor numeric-grade 10)
    (10 "A")
    (9 "A")
    (8 "B")
    (7 "C")
    (6 "D")
    (otherwise "F")))

;; Call the function with a numeric grade (e.g., 85)
(format t "Grade: ~a~%" (convert-to-letter-grade 85))


;;; Example 2:
(format t "~%Example 2:~%")

#|
Rules for input code:
- The code contains a function called `mypair`.
- The function takes three arguments: `x`, `y`, and `msg`.
- The `case` special form is used to perform different operations based on the `msg` argument.

Steps:
1. Define a function `mypair` that takes three arguments.
2. Use the `case` special form to match the `msg` argument and perform different operations accordingly.

Syntax:
- (defun mypair (x y msg) ...)

|#

; Type: (function (number number symbol) (or number null))
(defun mypair (x y msg)
  (case msg
    (sum (+ x y))
    (diff (abs (- x y)))
    (dist (sqrt (+ (* x x) (* y y))))
    (print (pprint x) (pprint y))))

;; Call the function with different messages (e.g., 'sum')
(format t "Result: ~a~%" (mypair 3 4 'sum))
(format t "Result: ~a~%" (mypair 5 7 'dist))

;;; Example 3:
(format t "~%Example 3:~%")

#|
Rules for input code:
- We will define and use global variables to store the lower and upper bound numbers.
- The global variables are named *lowerb* and *upperb*.
- We use the ASH function to perform a bit shift operation.
- The ASH function is used to halve a number by shifting its bits one position to the right.

Steps:
1. Define global variables *lowerb* and *upperb* to store lower and upper bound numbers.
2. Use the ASH function to perform bit shift operations for halving numbers.
3. Demonstrate changing the values of *lowerb* and *upperb*.

Syntax:
- (defvar *lowerb* ...)
- (defvar *upperb* ...)
- (ash ...)

|#

; Type: This code demonstrates the use of global variables, bit shifting, and variable assignment.
; (output of code;==> The output includes the values of lower and upper bounds, as well as the results of bit shift operations.)
(defvar *lowerb* 1) ; Define lower bound
(defvar *upperb* 100) ; Define upper bound

(format t "Lower Bound: ~a~%" *lowerb*) ; Output lower bound
(format t "Upper Bound: ~a~%" *upperb*) ; Output upper bound

(let ((shifted (ash 5 -1))) ; Halve a number using bit shift to the right 
  (format t "Halving 5: ~a~%" shifted)) ; Output halved result

(let ((shifted (ash 6 -1))) ; Halve another number using bit shift to the right 
  (format t "Halving 6: ~a~%" shifted)) ; Output halved result

(let ((shifted (ash 6 1))) ; Shifting bits to the left
  (format t "Shifting bits left with 6 (does not mean multiplying): ~a~%" shifted)) ; Output shifted result

; Usage:
; (Code to run the input code)
; Example usage:
(format t "Example usage:~%")
(defvar *lowerb* 10) ; Define lower bound
(defvar *upperb* 20) ; Define upper bound
(format t "Lower Bound: ~a~%" *lowerb*) ; Output lower bound
(format t "Upper Bound: ~a~%" *upperb*) ; Output upper bound


;;; Example 4:
(format t "~%Example 4:~%")

#|
Use case:
(In simple terms: SETF and := are used to manipulate global variables. They are helpful for modifying global variables' values, providing flexibility in programming.)

Rules for input code:
- Demonstrates the use of SETF and := to manipulate global variables.
- Explores whether SETF and := are the same thing.

Steps:
1. Define a global variable.
2. Use SETF to modify its value.
3. Use := to modify its value.
4. Check if the two methods produce the same result.

Syntax:
- (defparameter variable initial-value)
- (setf variable new-value)
- (:= variable new-value)

|#

; Type: This code illustrates the manipulation of global variables using SETF and :=, investigating their equivalence.
; (output of code;==> The output includes the values of the global variable before and after modification, as well as a comparison of the results.)

; Usage:
(defvar *x* 10) ; Define a global variable x with an initial value of 10
(format t "Initial value of *x*: ~a~%" *x*) ; Output the initial value of x

; Modify the value of *x* using SETF
(setf *x* 20)
(format t "Value of *x* after SETF: ~a~%" *x*) ; Output the new value of *x*

; Modify the value of *x* using :=
(:= *x* 30)
(format t "Value of *x* after :=: ~a~%" *x*) ; Output the new value of *x*

(format t "Are SETF and := the same? ~a~%" (equal *x* 30)) ; Output the comparison result

;;; Example 5:
(format t "~%Example 5:~%")

#|
Use case:
(In simple terms: The code defines functions for guessing numbers by adjusting upper and lower bounds. This is helpful for implementing number guessing games.)
Defines two global variables, *upperb* and *lowerb*, which represent the upper and lower bounds of the number range. 
The guess-my-number function calculates the mean of the upper and lower bounds, and returns it as an integer.
The smaller and bigger functions modify the upper and lower bounds based on whether the user's guess is too high or too low, 
and then call guess-my-number to calculate the new guess.
The format function is used to output the initial bounds, the initial guess, and the subsequent guesses to the console.

Rules for input code:
- Defines functions for guessing numbers and adjusting bounds.
- Utilizes the ash function for bit manipulation.

Steps:
1. Define a function guess-my-number that returns the integer mean of *upperb* and *lowerb*.
2. Define a function smaller that adjusts *upperb* and returns a new guess.
3. Define a function bigger that adjusts *lowerb* and returns a new guess.

Syntax:
- (defun function-name () ...)
- (ash expression -1)
- (setf variable value)

|#

; Type: This code provides functions for guessing numbers and modifying bounds, useful for implementing number guessing games.
; (output of code;==> The output includes the results of functions and adjustments made to *upperb* and *lowerb*.)

; Usage:
(defparameter *upperb* 100) ; Define initial upper bound
(defparameter *lowerb* 1) ; Define initial lower bound

(defun guess-my-number ()
 "Returns the integer mean of *upperb* and *lowerb*"
 (ash (+ *upperb* *lowerb*) -1))
 
(defun smaller ()
 (setf *upperb* (1- (guess-my-number))) ; Note: 1- is the name of a function
 (guess-my-number))

(defun bigger () ; Define a function called bigger with no arguments
    (setf *lowerb* (1+ (guess-my-number))) ; 1+ is the name of a function and set it to the new value of *lowerb*
    (guess-my-number)) ; Call the guess-my-number function to calculate the new guess and return it

(format t "Initial upper bound: ~a~%" *upperb*) ; Output initial upper bound
(format t "Initial lower bound: ~a~%" *lowerb*) ; Output initial lower bound

(format t "Guess 1: ~a~%" (guess-my-number)) ; Output initial guess
(format t "Guess 2 (smaller): ~a~%" (smaller)) ; Output smaller guess
(format t "Guess 3 (bigger): ~a~%" (bigger)) ; Output bigger guess


; -----------------------------------------------------------------------
; Run code 
;(load "lec_3.lisp")