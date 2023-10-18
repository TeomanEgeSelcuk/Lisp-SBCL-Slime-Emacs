;;; Example 1:~1
(format t "~%Example 1:~%")

#|

Use case:

- This set of codes is used for defining a structure for movies, creating a movie instance, modifying its data, and checking if a particular movie is present in a database.
- It allows users to effectively organize, manage, and retrieve movie data.

Rules for input code:

- A structure named "movie" is defined with four attributes: title, director, year, and type.
- Instances of movies can be created and their attributes can be accessed or modified.
- A function to check the presence of a movie in a database by its title is provided.

Types:

- The "movie" structure has fields of type string for title and director, integer for the year, and string for type.
- The "in-db?" function takes a string as input (the title of the movie) and returns a movie structure if found, otherwise NIL.

Steps:

1. Define a structure named "movie" to hold information about movies.
2. Create an instance of a movie by providing values for some or all of its attributes.
3. Access or modify the attributes of a movie instance.
4. Define a function to check if a movie with a specific title is present in a database.

Syntax:

- (let ((var-1 val-1)
      (var-2 val-2)
      ...)
  body)
- (setf (slot-name struct) value)
- (defun function-name (parameter-1 parameter-2 ...)
    body)
- (dotimes (var count)
  body)
- (when condition
  body)
|#

;; Define a new structure type with named slots
(defstruct movie
    title
    director
    year
    type)

;; Create a new instance of the movie structure type with
;; specified slot values

(make-movie :title "Blade Runner" :director "Ridley Scott")
(defvar my-movie (make-movie :title "Blade Runner" :director "Ridley Scott"))
(format t "Movie: ~A~%" my-movie)


;; Creating a new instance of a movie structure type and binding it to
;; a variable named var. Then, it sets the value of the year slot in 
;; the movie structure to 2022. Finally, it returns the values of the title,
;; director, and year slots in the movie structure using the values function.

;; Bind a variable to a new instance
;; of the movie structure type
(let ((var (make-movie :title "Ufos" :director "Spielberg")))

    ;; Set the value of the year slot in the movie structure
    (setf (movie-year var) 2022)
    
    (print var)  ; This will print the value of var
    
    ;; Return the values of the title, director, and year slots in the movie structure
    (values (movie-title var) (movie-director var) (movie-year var)))


;; Defines a function named in-db? that takes a title parameter.
;; The function iterates over a specified number of times using
;; the dotimes function. For each iteration, it checks if the current 
;; element in the db array is a movie structure and if its title slot
;; matches the specified title. If it finds a match, it returns the
;; current element in the array. If it doesn't find a match, it continues 
;; to the next iteration until it has iterated over the entire array.

;; Define a new function that takes a title parameter
(defvar size 10)
(defvar db (make-array 10))

;; Store the "Ufos" movie in the db array at index 0
(setf (aref db 0) (
    make-movie :title "ET" :director "Jackson" :year 1966))

(defun in-db? (title)
    ;; Iterate over a specified number of times
    (dotimes (i size)
        ;; Check if the current element in the array is a 
        ;; movie structure and if its title slot matches the specified title
        (when (and (typep (aref db i) 'movie) (equal (movie-title (aref db i)) title))
            ;; Return the current element in the array if it
            ;; matches the specified title
            (return (aref db i)))))


;;Printing 
(print (in-db? "Ufos")) ;;-> NIl
(print (in-db? "ET")) ;; prints 


;;; Example 2:~2
(format t "~%Example 2:~%")

#|

Use case:

- The purpose of this code is to create an adjustable vector, 
    extend it dynamically, and describe its content after each addition.
- It's helpful because it shows how an adjustable vector can be
    dynamically increased in size, making it a flexible option for
    storing elements when the total count isn’t known beforehand.

Rules for input code:

- Initiate an adjustable vector with a fill pointer.
- Use a loop to add elements to the vector.
- The vector's size adjusts dynamically as elements are added.
- The 'describe' function is used to print the state of the vector after each addition.

Types:

- vec is an adjustable vector that can dynamically increase in size.
- i is an integer used in the loop iteration.

Steps:

1. Create an adjustable vector with a fill pointer.
2. Iterate 3 times using the dotimes loop, each time adding an 
element to the vector and describing the vector’s current state.
3. Utilize the vector-push-extend function to add elements to
 the vector, allowing it to grow dynamically.
4. Use the describe function to print out the vector's 
content and properties after each addition.

Syntax:

(let ((variable-name (make-array initial-size :fill-pointer t :adjustable t)))
  (dotimes (iterator max-count)
    (vector-push-extend element variable-name)
    (describe variable-name)))

|#

; Usage:
;; Create a new array with a fill pointer and adjustable size
(let ((vec (make-array 0 :fill-pointer t :adjustable t)))

    ;; Iterate over a specified number of times
    (dotimes (i 3)

        ;; Add the current iteration value to the array and 
        ;; extend its size if necessary
        (vector-push-extend i vec)

        ;; Print the description of the array after each iteration
        (print "Array Description:")
        (describe vec)))

; Usage:
; (load "/src/notes/week-3/lec_2.lisp")