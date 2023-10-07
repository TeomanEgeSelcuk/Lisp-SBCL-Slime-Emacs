; Defines the movie structure, which is a list of attributes that describe a movie: title, director, year, and type.
(defstruct movie
  title director year type)

; Defines the ADD-MOVIE function, which adds a movie to an array of movies if the title of the movie is not already present in the array.
; The function takes two arguments: the movie to add and the array of movies.
; The function returns either the updated array (if the move was added) or nil (if the movie was not added).
(defun add-movie (movie-to-add array)
  ; Gets the size of the array.
  (let ((array-size (length array))
        ; Defines a variable that will be used to store whether the title already exists in the array.
        (title-exists nil))
    
    ; Loops through the array to see if the title of the movie to add is already present.
    (loop for i from 0 below array-size do
          (let ((movie (aref array i)))
            
            ; If the title is already present, sets the title-exists variable to t.
            (if (and movie (string= (movie-title movie) (movie-title movie-to-add)))
                (setf title-exists t))))
    
    ; If the title exists, returns nil.
    (if title-exists
        nil
        ; Loops through the array to find an empty slot.
        (loop for i from 0 below array-size do
              (if (null (aref array i))
                  ; If an empty slot is found, adds the movie to the array and returns the updated array.
                  (progn
                    (setf (aref array i) movie-to-add)
                    (return array)))))))

; Defines the DELETE-MOVIE function, which deletes a movie from an array of movies if the title of the movie is present in the array.
; The function takes two arguments: the title of the movie to delete and the array of movies.
; The function returns either the updated array (if the move was deleted) or nil (if the movie was not deleted).

; Define the movie structure
(defstruct movie
  title director year type)

; Define the ADD-MOVIE function
(defun add-movie (movie-to-add array)
  ; Get the array size
  (let ((array-size (length array))
        (title-exists nil))
    
    ; Loop through the array
    (loop for i from 0 below array-size do
          ; Get the current movie
          (let ((movie (aref array i)))
            
            ; Check if movie title exists
            (if (and movie (string= (movie-title movie) (movie-title movie-to-add)))
                ; If it does, set the flag
                (setf title-exists t))))
    
    ; If title exists, return nil
    (if title-exists
        nil
        ; Loop again to find an empty slot
        (loop for i from 0 below array-size do
              (if (null (aref array i))
                  ; Add the movie and return the updated array
                  (progn
                    (setf (aref array i) movie-to-add)
                    (return array)))))))


; Define the DELETE-MOVIE function
(defun delete-movie (title-to-delete array)
  (let ((array-size (length array))
        (deleted nil))
    
    ; Loop through the array
    (loop for i from 0 below array-size do
          (let ((movie (aref array i)))
            
            ; Check if movie title matches the one to delete
            (if (and movie (string= (movie-title movie) title-to-delete))
                (progn
                  ; Set the slot to nil and mark as deleted
                  (setf (aref array i) nil)
                  (setf deleted t)))))
    
    ; If a movie was deleted, return the updated array
    (if deleted array nil)))

; Define the NUM-MOVIES function
(defun num-movies (array)
  (let ((count 0))
    
    ; Loop through the array
    (loop for movie across array do
          (if movie (incf count)))
    
    ; Return the count of non-nil elements
    count))

; Test code
(defvar a (make-array 4 :initial-element nil))
(print a)
(print (add-movie (make-movie :title "Blade Runner") a))
(print (delete-movie "Blade Runner" a))
(print (num-movies a))

; (cg:eval-solutions "" :Lab03)