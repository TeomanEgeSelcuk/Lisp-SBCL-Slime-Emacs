#|
The file defines a function called `square` that takes a single argument `x` and returns the square of `x`. 
The function is defined using the `defun` special form, which takes the name of the function, its arguments, and its body as arguments.
The file also contains a commented-out call to the `square` function with the argument `5`. To uncomment the call and see its result,
 remove the semicolon at the beginning of the line.

Example usage:
(square 5) ; Returns 25
|#

(defun square (x) (* x x))

; (square 5)