; The FILTER-AGES function takes a list of PERSON 
; structures and an AGE-THRESHOLD and returns a list
; of strings containing the names of all people whose
; age is greater than the AGE-THRESHOLD.

; The ARRAY-GROUP function takes two arrays of equal
; length and returns an array of vectors. The vectors
; in the result array have two elements, one from each of the input arrays. If one of the input arrays is shorter than the other, the extra elements in the longer array are ignored.

; Define the PERSON structure with NAME (string)
; and AGE (integer) slots

(defstruct person ; Define a structure for a person
  name ; The name of the person
  age) ; The age of the person

; Define the FILTER-AGES function
(defun filter-ages (people age-threshold) ; Take a list of people and an age threshold
  (let ((result (make-array 0 :adjustable t :fill-pointer 0))) ; Create an empty array
    (loop for person across people do ; Loop over the people
      (when (> (person-age person) age-threshold) ; If their age is greater than the threshold
        (vector-push-extend (person-name person) result))) ; Add their name to the array
    result)) ; Return the array

; Define the ARRAY-GROUP function
(defun array-group (array1 array2)
  (let* ((len1 (length array1)) ;; get the length of array1
         (len2 (length array2)) ;; get the length of array2
         (result (make-array (max len1 len2) :adjustable t :fill-pointer 0))) ;; create result array
    (loop for i from 0 below (max len1 len2) do ;; loop through result array
      (vector-push-extend (vector (if (< i len1) (aref array1 i) nil) ;; push the first element
                                   (if (< i len2) (aref array2 i) nil)) ;; push the second element
                          result)) ;; into the result array
    result)) ;; return the result array

;;




