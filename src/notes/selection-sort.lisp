
#|
The selection-sort function takes a list as input and sorts it using the selection sort algorithm. 
The algorithm works by repeatedly finding the minimum element from the unsorted part of the list and moving it to the beginning of the list.

The implementation of the selection-sort function uses two nested dotimes loops to iterate over the list and find the minimum element. 
The rotatef function is used to swap the minimum element with the first element of the unsorted part of the list.

The function returns the sorted list as output.

Usage:
(selection-sort '(3 1 4 1 5 9 2 6 5 3 5))
Output: (1 1 2 3 3 4 5 5 5 6 9)

Steps: 
1. Define a function called selection-sort that takes a list lst as input.
2. Add a docstring to the function that describes what it does.
3. Create a local variable n that stores the length of the input list lst.
4. Use a dotimes loop to iterate over the indices of the list lst.
5. Create a local variable min-index that stores the index of the minimum element found so far.
6. Use another dotimes loop to iterate over the remaining unsorted part of the list.
7. Use a when conditional expression to check if the current element is less than the current minimum element.
8. If the current element is less than the current minimum element, update min-index to the index of the current element.
9. Use the rotatef function to swap the minimum element with the first element of the unsorted part of the list.
10. Repeat steps 5-9 until the entire list is sorted.
11. Return the sorted list as output.

|#

(defun selection-sort (lst) ; Define a function called selection-sort that takes a list lst as input
    "Sorts a list using selection sort" ; Add a docstring to the function that describes what it does
    (let ((n (length lst))) ; Create a local variable n that stores the length of the input list lst
        (dotimes (i n lst) ; Use a dotimes loop to iterate over the indices of the list lst
            (let ((min-index i)) ; Create a local variable min-index that stores the index of the minimum element found so far
                (dotimes (j (- n i 1)) ; Use another dotimes loop to iterate over the remaining unsorted part of the list
                    (when (< (nth j lst) (nth min-index lst)) ; Use a when conditional expression to check if the current element is less than the current minimum element
                        (setf min-index j))) ; If the current element is less than the current minimum element, update min-index to the index of the current element
                (rotatef (nth i lst) (nth min-index lst)))
            ;(format t "Iteration ~a: ~a~%" (+ i 1) lst)))) ; Use the format function to print the current iteration and the current state of the list

; Usage:
(selection-sort '(3 1 4 1 5 9 2 6 5 3 5))
; Output: (1 1 2 3 3 4 5 5 5 6 9)



(defun insertion-sort (lst)
    "Sorts a list using insertion sort"
    (let ((n (length lst)))
        (dotimes (i (1- n) lst)
            (let ((key (nth (1+ i) lst)))
                (loop for j downfrom i
                    while (and (>= j 0) (> (nth j lst) key))
                    do (rotatef (nth j lst) (nth (1+ j) lst))
                         (format t "Iteration ~a: ~a~%" (+ i 1) lst)))
            (format t "Iteration ~a: ~a~%" n lst))))

; Usage:
(insertion-sort '(3 1 4 1 5 9 2 6 5 3 5))
; Output: (1 1 2 3 3 4 5 5 5 6 9)


#|
Iterate over the list from the second element to the last element.
For each element, compare it to the elements before it until you find the correct position to insert it.
To insert the element, shift all the elements after the insertion point one position to the right.
Insert the element into the correct position.
Repeat steps 2-4 until the entire list is sorted.
#|

