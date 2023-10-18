# **What Are Lists?**
- Lists are a fundamental data structure found in many modern programming languages.
- In Lisp, lists are especially central and versatile.
- They are more flexible than arrays and can represent a wide range of data types, including sets, tables, graphs, and even text like English sentences.

**What Do Lists Look Like?**
- Lists in Lisp are collections of data enclosed in parentheses.
- Examples of lists in Lisp:
  - `(43 12 45 3 -1)` is a list of numbers.
  - `(RED GREEN BLUE)` is a list of color names.
  - `(3 FRENCH HENS 2 TURTLE DOVES 1 PARTRIDGE 1 PEAR TREE)` is a list that can represent a song lyric.
- Lists can hold various types of data, including numbers, words, or other lists.

**Example Lisp Program:**
- Lisp programs themselves are essentially lists, which is why it's called "LisP."
- Here's a simple Lisp program that calculates the sum of numbers from 1 to `n`:
```lisp
(defun sum-n1 (n)
  "Return the sum of the first n positive integers."

  ;; Initialize the loop variables i and sum
  (do ((i 1 (1+ i))  ; i starts at 1 and increments by 1 in each iteration
       (sum 0 (+ i sum)))  ; sum starts at 0 and adds i in each iteration

    ;; Check if i is greater than n
    ((> i n) sum)))  ; If yes, return the sum; otherwise, continue the loop
```
  - This program uses the `do` construct to iterate through numbers from 1 to `n` and calculate their sum.
  - `sum-n1` function takes an integer n as an argument and returns the sum of the first n positive integers. The function first initializes the loop variables i and sum using the do function. The i variable starts at 1 and increments by 1 in each iteration, and the sum variable starts at 0 and adds i in each iteration. The do function then checks if i is greater than n. If yes, it returns the sum. If no, it continues the loop.

**Internal Representation (Cons Cell Structure):**
- Internally, computers represent lists using a structure called a "cons cell."
- In Lisp, a cons cell has two parts: the "car" (short for Contents of the Address Register) and the "cdr" (short for Contents of the Decrement Register).
- Lists are formed by chaining cons cells together.
- A list is represented as a chain of cons cells, where the "car" contains the data, and the "cdr" points to the next cons cell.
- The chain of cons cells ends with a special value, `NIL`, indicating the end of the list.
- For example, `(RED GREEN BLUE)` is internally represented as a chain of cons cells, with the last one pointing to `NIL`.

**Mathematical Concepts:**
- Lists in Lisp have a recursive structure.
- They can be thought of as a linked data structure, where each element points to the next one.
- This recursive structure makes Lisp particularly well-suited for tasks that involve recursion and dynamic data structures.

**NIL: The Empty List**
- In Lisp, the empty list is represented as either `()` or `NIL`.
  - Example 1: `(A NIL B)` can also be written as `(A () B)`.
  - Example 2: `(length NIL)` and `(length ())` both return 0 because they represent empty lists.

**List Accessors**
- Lisp provides primitive functions for accessing elements in a list:
  - `(first '(a b c d))` or `(car '(a b c d))` returns `a`.
  - `(second '(a b c d))` returns `b`.
  - `(third '(a b c d))` returns `c`.
  - `REST` and `CDR` return a list containing everything except the first element:
    - `(rest '(a b c d))` or `(cdr '(a b c d))` returns `(b c d)`.
    - `(rest (cdr '(a b c d)))` returns `(c d)`.

**Example: Defining FORTH Using CAR and CDR**
- FORTH is a programming language known for its stack-based architecture.
- You can define a function to access the fourth element of a list using `CAR` and `CDR` combinations:
```lisp
(defun forth (a)
  "Return the fourth element of list a."

  ;; Get the fourth element of list a by taking the car of the cdr of the cdr of the cdr of a
  (car (cdr (cdr (cdr a)))))
```
  - This function retrieves the fourth element from the list `a`.
  -  forth function takes a list a as an argument and returns the fourth element of the list. The function first gets the fourth element of list a by taking the car of the cdr of the cdr of the cdr of a. The cdr function is used to get the rest of the list after the first element, and the car function is used to get the first element of the rest of the list.

Alternatively, you can use `CADDDR`:
```lisp
(defun forth (a)
  (cadddr a))
```
  - `CADDDR` is a built-in accessor in Lisp that directly accesses the fourth element of the list.
  - . The forth function takes a list a as an argument and returns the fourth element of the list. The function gets the fourth element of list a by taking the cadr of the cadr of the cadr of a. The cadr function is used to get the second element of the list, and the cadddr function is used to get the fourth element of the list.

**CONS Cell and CONS:**
- `CONS` is a fundamental function in Lisp that creates a cons cell.
- A cons cell has two parts: CAR and CDR.
- `CONS` takes two parameters and returns a new cons cell with its CAR pointing to the first parameter and its CDR pointing to the second parameter.
- Examples of `CONS`:
  - `(cons 'sink '(or swim))` creates a cons cell with CAR as 'sink and CDR as '(or swim).
  - `(cons 'hello '(there miss doolittle))` creates a cons cell with CAR as 'hello and CDR as '(there miss doolittle).

**List Creation Using CONS:**
- You can use `CONS` to build lists recursively or iteratively.
- Example: Creating a list of 'a's using recursion:
```lisp
(defun mymake-list-rec (n element &optional (acc nil))
  "Create a list of length n with each element equal to element."

  ;; Check if n is 0
  (if (= n 0)

      ;; If yes, return the accumulator
      acc

      ;; If no, recursively call the function with n decremented by 1, element, and the cons of element and the accumulator
      (mymake-list-rec (1- n) element (cons element acc))))
```
-  `mymake-list-rec` function takes an integer n, an element element, and an optional accumulator acc as arguments, and returns a list of length n with each element equal to element. The function first checks if n is 0 using the = function. If yes, it returns the accumulator. If no, it recursively calls the function with n decremented by 1, element, and the cons of element and the accumulator using the cons function.
  
- Example: Creating a list of 'a's using iteration:
```lisp
(defun mymake-list-it (n elem)
  "Create a list of length n with each element equal to elem."

  ;; Initialize the accumulator to NIL
  (let ((acc nil))

    ;; Iterate over the range 0 to n-1 using the dotimes function
    (dotimes (i n acc)

      ;; Set the accumulator to the cons of elem and the accumulator
      (setf acc (cons elem acc)))))
```
-  `mymake-list-it` function takes an integer n and an element elem as arguments, and returns a list of length n with each element equal to elem. The function first initializes the accumulator to NIL using the let function. It then iterates over the range 0 to n-1 using the dotimes function. In each iteration, it sets the accumulator to the cons of elem and the accumulator using the cons function, and updates the accumulator using the setf function. Finally, the function returns the accumulator.

**List Constructors (QUOTE, MAKE-LIST, LIST):**
- Lisp provides three ways to create lists:
  - `QUOTE`: Creates a literal (constant) list. Its contents should not be changed.
  - `MAKE-LIST`: Creates a list of specified length.
  - `LIST`: Creates a list from its arguments.
- Examples:
  - `(quote ("hello" world 111))` creates a constant list.
  - `(make-list 3)` creates a list with three `NIL` elements.
  - `(make-list 3 :initial-element 'a)` creates a list of 'a's.
  - `(make-list 3 :initial-contents '(a b c))` creates a list from specified elements.
  - `(list "hello" 'world 111)` creates a list from arguments.

**DOLIST Loop:**
- `DOLIST` is a looping construct in Lisp used to iterate across elements of a list.
- It evaluates a list-form once to produce a list and then evaluates a body-form for each item in the list, with a variable holding the item's value.
- Example:
```lisp
(dolist (x '(1 2 3))
  ;; Print the value of x
  (print x))
```
  - This code prints each element in the list (1, 2, 3) one by one and returns `NIL` when done.
- `RETURN` can be used to break out of a `DOLIST` loop prematurely.

**Example Function Using Iterative DOLIST:**
- Here's a function that returns a list of odd numbers from its input list, using `DOLIST` iteratively:
```lisp
(defun get-odds-it (alist)
  "Return a list of the odd elements in alist using an iterative approach."

  ;; Initialize the accumulator to NIL using the let function
  (let ((acc nil))

    ;; Iterate over the elements of alist using the dolist function
    (dolist (elem alist acc)

      ;; Check if the current element is odd using the oddp function
      (if (oddp elem)

          ;; If yes, add the element to the accumulator using the cons function
          (setf acc (cons elem acc)))))) ; alternatively (push elem acc)

```
- `get-odds-it` function takes a list alist as an argument and returns a list of the odd elements in alist using an iterative approach. The function first initializes the accumulator to NIL using the let function. It then iterates over the elements of alist using the dolist function. In each iteration, it checks if the current element is odd using the oddp function. If yes, it adds the element to the accumulator using the cons function and updates the accumulator using the setf function. Finally, the function returns the accumulator. Alternatively, the push function can be used to add the element to the accumulator.

**Example Function Using Recursive Approach:**
- Here's an alternative function that achieves the same using recursion:
```lisp
(defun get-odds (alist &optional (acc nil))
  (cond ((null alist) acc)
        ((oddp (car alist)) (get-odds (cdr alist) (cons (car alist) acc)))
        (t (get-odds (cdr alist) acc))))
```
- `get-odds` function takes a list alist and an optional accumulator acc as arguments, and returns a list of the odd elements in alist using a recursive approach. The function first checks if alist is null using the null function. If yes, it returns the accumulator. If no, it checks if the first element of alist is odd using the oddp function and the car function. If yes, it recursively calls the function with the rest of alist and the cons of the first element and the accumulator using the cons function. If no, it recursively calls the function with the rest of alist and the accumulator. Finally, the function returns the accumulator.