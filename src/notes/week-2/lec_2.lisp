;;; Example 1:~%
(format t "~%Example 1:~%")

#|

Use case:

In simple and short terms:
- This function provides a mechanism to repeatedly read lines from the user until they type "exit". 
- It also keeps track of the number of lines entered and the accumulated length of the lines. 
- It's helpful for interactive programs that require user input and monitoring input length.

Rules for input code:

- The loop starts with the counter "i" initialized to 0.
- The string "s" is read from the user each iteration.
- The accumulator "acc" keeps track of the total length of the strings input by the user.
- The loop continues until the user inputs the string "exit".

Steps:

1. Print the instruction "Type 'exit' to stop."
2. Initialize the counter "i" to 0.
3. Read a line from the user and assign it to "s".
4. Calculate the length of the string "s" and add it to the accumulator "acc".
5. Continue reading lines and accumulating their lengths until the user types "exit".
6. For each line entered, print the line number, the line itself, and the accumulated length.

Syntax:

(do* ((var init [step−form]))
     form∗ form
     [form] form
     s
     (end-test-form result-form [result-form ...]))

|#

; Usage:
(defun do*-example ()
    ; Print a message to the standard output stream
    (format t "Type 'exit' to stop.~%")
    ; Create a loop with three variables: i, s, and acc
    (do* ((i 0 (1+ i)) ; Initialize i to 0 and increment it by 1 on each iteration
                (s (read-line) (read-line)) ; Read a line of input and store it in s
                (acc (length s) (+ acc (length s)))) ; Initialize acc to the length of s and add the length of each subsequent s to acc
             ((equal s "exit") t) ; Terminate the loop when s is equal to "exit"
             ; Print a message to the standard output stream
             (format t "~a: ~a ~a~%" i s acc)))

(format t "Running do*-example function.~%")
(do*-example)



(format t "Example 2:~%")

#|

Use case:

In simple and short terms:
- This function counts the occurrences of a specific character in a string.
- It's helpful for analyzing text and determining the frequency of specific characters.

Rules for input code:

- The function accepts a character and a string as arguments.
- It initializes an accumulator "acc" to 0.
- It then iterates over each character in the string.
- For each character in the string that matches the input character, the accumulator is incremented.

Steps:

1. Initialize an accumulator "acc" to 0.
2. Iterate over each character in the string.
3. If the character matches the input character, increment the accumulator.
4. Return the accumulator's value.

Syntax:

(let ((var value))
  (dotimes (var limit result)
    (code ...)))

(when test-form
    body-form*)
|#

(defun count-chars (c s)
    "Counts the number of occurrences of character c in string s"
    ; Initialize an accumulator variable to 0
    (let ((acc 0))
        ; Loop over the characters in the string
        (dotimes (i (length s) acc)
            ; Check if the current character matches the input character
            (when (char= (aref s i) c)
                ; Increment the accumulator if the characters match
                (incf acc)
                ; Print a message to the standard output stream
                (format t "Found ~a at index ~a~%" c i)))))


(format t "Counting the number of occurrences of character ~a in string ~a~%" #\a "banana")
(count-chars #\a "banana")

;;; Example 3:~3
(format t "~%Example 3:~%")

#|

Use case:

- The purpose of this function is to count the occurrences of a specific character in a given string.
- It is helpful for analyzing text, such as counting the number of specific letters, digits, or symbols.

Rules for input code:

- The function 'count-char' takes two arguments: a character and a string.
- It iterates through the string, counting occurrences of the specified character.
- The count is returned as the result.

Steps:

1. Initialize a counter to zero.
2. Iterate through each character in the string.
3. Compare each character with the specified character.
4. If they match, increment the counter.
5. Return the final count after the iteration is complete.

Syntax:
defun: (defun function-name (parameter-list)
    "Optional documentation string"
    body-form*)

let: (let ((var1 value1)
      (var2 value2)
      ...)
  body-form*)
dotimes: (dotimes (var count result-form)
    body-form*)

when: (when test-form
        body-form*)
incf: (incf place &optional delta)

|#

; Define a function that counts the number of occurrences of a character in a string
(defun count-char (c s)
    ; Initialize an accumulator variable to 0
    (let ((acc 0))
        ; Loop over the characters in the string
        (dotimes (i (length s) acc)
            ; Check if the current character matches the input character
            (when (char= (aref s i) c)
                ; Increment the accumulator if the characters match
                (incf acc)))))

; Usage:
(format t "Count of character 'a' in string 'banana': ~A~%" 
        (count-char #\a "banana"))


;;; Example 4
(format t "~%Example 4:~%")

#|

Use case:

- The purpose of these functions is to determine if a given character is a vowel or a consonant.
- These functions are helpful in text processing, linguistics analysis, or any scenario where categorizing characters is needed.

Rules for input code:

- The 'vowelp' function returns true if the character represents a vowel, false otherwise.
- The 'consonantp' function returns true if the character represents a consonant, false otherwise.
- Both functions only consider alphabetical characters; digits and symbols are not evaluated.

Steps:

1. For 'vowelp', check if the character is one of the vowels (a, e, i, o, u) in either uppercase or lowercase.
2. For 'consonantp', check if the character is not a vowel and is an alphabetical character.

Syntax:

|#

(defun vowelp (c)
    (or(char= c #\a) (char= c #\A)
        (char= c #\e) (char= c #\E)
        (char= c #\i) (char= c #\I)
        (char= c #\o) (char= c #\O)
        (char= c #\u) (char= c #\U)))

(defun consonantp (c)
; Check if character c is a consonant
; Returns t if c is a consonant, nil otherwise
(and
    ; Check if c is not a vowel
    (not (vowelp c))
    ; Check if c is a lowercase or uppercase letter
    (or
        (and (char>= c #\a) (char<= c #\z))
        (and (char>= c #\A) (char<= c #\Z)))))

; Usage:
(format t "Is 'a' a vowel? ~A~%" (vowelp #\a))
(format t "Is 'b' a consonant? ~A~%" (consonantp #\b))


;;; Example 5
(format t "~%Example 5:~%")

#|

Use case:

- The purpose of this function is to count both the vowels and consonants in a given string.
- It is helpful for text analysis, linguistics, and other scenarios where understanding the composition of a string is needed.

Rules for input code:

- The function 'count-vc-do' takes a single argument: a string.
- It iterates through the string, counting both vowels and consonants.
- The counts of vowels and consonants are returned as multiple values.

Steps:

1. Initialize two counters to zero, one for vowels and another for consonants.
2. Iterate through each character in the string.
3. Use the 'vowelp' function to check if the character is a vowel. If true, increment the vowel counter.
4. If not a vowel, use the 'consonantp' function to check if the character is a consonant. If true, increment the consonant counter.
5. Return the final counts of vowels and consonants after the iteration is complete.

Syntax:

- (defun function-name (parameter-list)
  "Optional documentation string"
  body-form*)

- (do ((var1 init1 step1)
     (var2 init2 step2)
     ...)
    (end-test result-form*)
  body-form*)

- (if test-form
    then-form
    else-form)

- (incf place &optional delta) 
|#

(defun count-vc-do (s)
    ; Initialize loop variables i, accv, and accc
    (do ((i 0 (1+ i))
             (accv 0)
             (accc 0))
            ; Terminate loop when i reaches the length of s
            ((equal i (length s))
            (values accv accc)) ; Return the values of accv and accc
        ; Check if the current character is a vowel
        (if (vowelp (aref s i))
                ; Increment accv if the character is a vowel
                (incf accv)
                ; Check if the current character is a consonant
                (if (consonantp (aref s i))
                        ; Increment accc if the character is a consonant
                        (incf accc)))))

; Usage:
(multiple-value-bind (vowels consonants) 
    (count-vc-do "hello")
  (format t "Vowels: ~A, Consonants: ~A~%" vowels consonants))

; -----------------------------------------------------------------------
; Run code 
;(load "lec_2.lisp")
