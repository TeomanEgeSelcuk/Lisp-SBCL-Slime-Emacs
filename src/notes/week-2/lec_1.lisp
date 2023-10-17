;;; Example 1:
(format t "~%Example 1:~%")

#|
Use case:
(In simple terms: The code defines a function to determine the country of a city based on its name. It is helpful for mapping cities to countries.)

Rules for input code:
- Defines a function whereis that takes a city name as input.
- Uses the cond function to map cities to countries.

Steps:
1. Define the whereis function.
2. Use cond to map city names to countries.

Syntax:
- (defun function-name (parameter) ...)
- (cond (test-1 form-1 form-2 ... form-n)
      (test-2 form-1 form-2 ... form-n)
      ...
      (test-n form-1 form-2 ... form-n)
      (t form-1 form-2 ... form-n))

|#

; Type: This code provides a function to determine the country of a city based on its name.
; (output of code;==> The output includes the country name for the given city.)

; Usage:
(defun whereis (city)
  (cond
    ((equal city "Toronto") 'Canada)
    ((equal city "东风航天城") 'China)
    ((equal city "Звёздный городок") 'Russia)
    ((or (equal city "וופי-ביבא־לת") (equal city "افاي–بيبألت")) 'Israel)
    (t 'Unknown)))

(format t "City: Toronto, Country: ~a~%" (whereis "Toronto"))
(format t "City: 东风航天城, Country: ~a~%" (whereis "东风航天城"))
(format t "City: Звёздный городок, Country: ~a~%" (whereis "Звёздный городок"))
(format t "City: וופי-ביבא־לת, Country: ~a~%" (whereis "וופי-ביבא־לת"))
(format t "City: افاي–بيبألت, Country: ~a~%" (whereis "افاي–بيبألت"))
(format t "City: Unknown, Country: ~a~%" (whereis "Unknown"))


; ---------------------------------------

;;; Example 2:
(format t "~%Example 2:~%")

#|
Use case:
(In simple terms: The code demonstrates the use of the DOTIMES macro for iterative counting. It is helpful for performing tasks a specific number of times.)

Rules for input code:
- Uses the DOTIMES macro to perform a loop for a specified number of times.
- Provides an example of iterating through a range of numbers.

Steps:
1. Use the DOTIMES macro to perform a loop with a specified iteration count.
2. Print the current iteration index during each iteration.

Syntax:
- (dotimes (variable limit) ...)
- (format t "Text with ~a placeholder" variable)

|#

; Usage:

(format t "Result 1:")
(let ((count 5)) ; Set the number of iterations
  (dotimes (i count)
    (format t "Iteration ~a~%" (1+ i))))

(format t "Result 2:")
(dotimes (i 6 "done") (print i)) ; DOTIMES returns the value of the RESULT-FORM
; Output: 0 1 2 3 4 5 "done"

(format t "Result 3:")
(dotimes (i 6) (if (= i 3) (return) (print i))) ; RETURN without an argument returns NIL
; Output: 0 1 2 NIL

(format t "Result 4:")
(dotimes (i 6) (if (= i 3) (return "exited") (print i))) ; RETURN with an argument returns its argument
; Output: 0 1 2 "exited"


;;; Example 3:
(format t "~%Example 3:~%")

#|
Use case:
(In simple terms: The code defines a function to capitalize characters in a string by converting lowercase letters to uppercase.)
- defun: Defines a new function with a given name and argument list. The body of the function is defined using one or more Lisp expressions.
- if: A conditional expression that evaluates a test expression and returns one of two possible values depending on whether the test expression is true or false.
- char>=: A comparison function that returns true if the first character argument is greater than or equal to the second character argument, according to their ASCII values.
- char<=: A comparison function that returns true if the first character argument is less than or equal to the second character argument, according to their ASCII values.
- char-code: Returns the ASCII code of a given character.
- code-char: Returns the character corresponding to a given ASCII code.
- length: Returns the length of a given string.

aref: Returns the character at a given index in a string.

setf: Sets the value of a given place (such as a variable or array element) to a given value.
Rules for input code:
- Defines two functions: cpt-char and capitalize.
- cpt-char checks if a character is lowercase and converts it to uppercase.
- capitalize uses the cpt-char function to capitalize characters in a string.

Steps:
1. Define the cpt-char function to convert lowercase characters to uppercase.
2. Define the capitalize function to capitalize characters in a string using cpt-char.
3. Return the capitalized string.

Syntax:
- (defun function-name (arguments) ...)
- (if condition then-expression else-expression)
- (char>= character1 character2)
- (char<= character1 character2)
- (char-code character)
- (code-char code)
- (length string)
- (aref string index)
- (setf place value)

|#

(defun cpt-char (c) ; Define a function called cpt-char that takes a character c as input

    ;Lowercase if: 
    ;(char>= c #\a): c is greater than or equal to the lowercase letter a.
    ;(char<= c #\z): checks if c is less than or equal to the lowercase letter z. 

    (if (and (char>= c #\a) (char<= c #\z)) ; If c is a lowercase letter

            #|
            If c is a lowercase letter, it is converted to uppercase by subtracting the difference 
            between the ASCII codes of the lowercase and uppercase letters from the ASCII code of c.
            For example, the ASCII code of a is 97 and the ASCII code of A is 65, 
            so (code-char (- (char-code #\a) 32)) returns the character #\A.
            |#

            (code-char (- (char-code c) 32)) ; Convert c to uppercase and return it
            c)) ; Otherwise, return c as is

(defun capitalize (s) ; Define a function called capitalize that takes a string s as input
    "Capitalizes the characters in string s"
    (dotimes (i (length s) s) ; Loop over the characters in s
        (setf (aref s i) (cpt-char (aref s i))))) ; Convert each character to uppercase using cpt-char and set it in s

; Usage:
(let ((input-string "hello world")) ; Set the input string
  (format t "Original: ~a~%" input-string)
  (format t "Capitalized: ~a~%" (capitalize input-string)))


;;; Example 4:
(format t "~%Example 4:~%")

#| 
The opposite of the code above. 
|#

(defun cpl-char (c) ; Define a function called cpl-char that takes a character c as input

    ;Uppercase if: 
    ;(char>= c #\A): c is greater than or equal to the uppercase letter A.
    ;(char<= c #\Z): checks if c is less than or equal to the uppercase letter Z. 

    (if (and (char>= c #\A) (char<= c #\Z)) ; If c is an uppercase letter

            #|
            If c is an uppercase letter, it is converted to lowercase by adding the difference 
            between the ASCII codes of the uppercase and lowercase letters to the ASCII code of c.
            For example, the ASCII code of A is 65 and the ASCII code of a is 97, 
            so (code-char (+ (char-code #\A) 32)) returns the character #\a.
            |#

            (code-char (+ (char-code c) 32)) ; Convert c to lowercase and return it
            c)) ; Otherwise, return c as is

(defun lowercase (s) ; Define a function called lowercase that takes a string s as input
    "Converts the uppercase characters in string s to lowercase"
    (dotimes (i (length s) s) ; Loop over the characters in s
        (setf (aref s i) (cpl-char (aref s i))) ; Convert each character to lowercase using cpl-char and set it in s
        (format t "Iteration ~a: s = ~a, i = ~a (aref s i):  = ~a~%" (+ i 1) s i (aref s i))))

; Usage:
(let ((input-string "HELLO WORLD")) ; Set the input string
  (format t "Original: ~a~%" input-string)
  (format t "Lowercase: ~a~%" (lowercase input-string)))


; -----------------------------------------------------------------------
; Run code 
;(load "lec_1.lisp")