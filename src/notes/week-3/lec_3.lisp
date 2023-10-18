;;; Example #:
(format t "~%Example 1:~%")

#|

Use Case:

In simple and short terms:
- This function performs a binary search on a sorted vector to find a given value.
- It is helpful as binary search is faster than linear search for sorted collections.

Rules for input code:

- `val` is the target value to be searched for.
- `vec` is the sorted vector in which the search is performed.
- `pos` is the optional argument which keeps track of the position in the original vector (used for recursive calls).

Steps:

1. Check if the length of the vector `vec` is greater than 1.
2. If yes, find the middle point and the middle element of the vector.
3. Compare the middle element with the target value `val`.
4. If the middle element is smaller than `val`, search in the right half of the vector.
5. If the middle element is larger than `val`, search in the left half of the vector.
6. If the middle element is equal to `val`, return the position.
7. If there's only one element left in the vector, check if it's equal to `val` and return the position if it is.

Syntax:

(defun bin-search (val vec &optional (pos 0))
    ... ; function body
)

|#

(format t "~15T~A~25T~A~35T~A~45T~A~%" "Index of MidP" "MidPoint" "Upper" "Lower")

;; val: the target value to search for in the vector
;; vec: the sorted vector to search in
;; pos: the starting position in the vector (default is 0)
(defun bin-search (val vec &optional (pos 0))
    "Search for val in sorted vector vec, return position if found, NIL otherwise."

    ;; Define a function to print the current iteration of the middle, upper, and lower bounds
    ;; midel refers to the value of the middle element in the vector.
    (defun print-iteration (midpt midel upper lower)
        (format t "~27T~A~36T~A~42T~A~49T~A~%" midpt midel upper lower))

    ;; Check if there are more than one element in the vector
    (if (> (length vec) 1)
        (let* ((midpt (floor (length vec) 2))  ; Calculate the index of the middle element in the vector
                 (midel (aref vec midpt))  ; Get the value of the middle element in the vector
                 (upper (if (> midpt 0) (aref vec (- midpt 1)) nil)) ; Get the value of the element immediately before the middle element, if it exists
                 (lower (if (< midpt (- (length vec) 1)) (aref vec (+ midpt 1)) nil))) ; Get the value of the element immediately after the middle element, if it existsCheck added here
            ;; Print the current iteration of the middle, upper, and lower bounds
            (print-iteration midpt midel upper lower)
            ;; Compare the middle element with the target value
            ;; Explanation : 
            ;; cond statement checks if the middle element midel is less than,
            ;; greater than, or equal to the target value val. If midel is less 
            ;; than val, the function recursively calls itself with the right half
            ;; of the vector as the new vector and the position adjusted 
            ;; accordingly. If midel is greater than val, the function 
            ;; recursively calls itself with the left half of the vector as
            ;; the new vector and the position kept the same. If midel is equal 
            ;; to val, the function returns the position of the middle element.
            (cond
                ((< midel val) 
                 (bin-search val 
                             (subseq vec midpt)
                             (+ pos midpt)))
                ((> midel val) 
                 (bin-search val 
                             (subseq vec 0 midpt)
                             pos))
                (t 
                 (+ pos midpt))))
        ;; Explanation :
        ;; checks if there is only one element left in the vector and if it
        ;; is equal to the target value val. If yes, it calls the print-iteration
        ;;  function to print the current iteration of the middle, upper, and lower bounds with the index of the middle element, the value of the middle element, and nil for both upper and lower bounds. If no, it returns nil by default.
        (when (= (aref vec 0) val)
            (print-iteration 0 (aref vec 0) nil nil) ; As it's the only element, both upper and lower are nil
            pos)))

; Usage:
(print (bin-search 5 #(1 2 3 4 5 6 7 8 9 10)))


;;; Example 2:
(format t "~%Example 2:~%")

#|

Use Case:

In simple and short terms:
- This function performs a selection sort on a given vector.
- The purpose is to sort the vector in a specific order defined by the comparison function `comp`.
- It is helpful as it provides a simple way to sort vectors based on custom criteria.

Rules for input code:

- `vec` is the vector to be sorted.
- `comp` is a comparison function that returns true if its first argument should come before its second argument in the sorted order.
- During each iteration of the outer loop, the function finds the best element from the remaining unsorted part of the vector and swaps it with the current element.

Steps:

1. Loop over the vector, excluding the last element.
2. For each iteration, initialize the best element and its index to the current element and index.
3. Loop over the remaining elements in the vector.
4. For each remaining element, if it's better than the best element (as per `comp`), update the best element and its index.
5. After finding the best element in the remaining unsorted part, swap it with the current element of the outer loop.
6. Continue this process until the entire vector is sorted.
7. Return the sorted vector.

Syntax:

(defun selection-sort (vec comp)
    ... ; function body
)

|#

(defun selection-sort (vec comp)
    ;; Define a helper function to print the table header
    (defun print-header ()
        (format t "~30T~A~45T~A~%" "Swapped Values" "Vector State"))
    
    ;; Define a helper function to print the details of each swap operation
    (defun print-swap-details (val1 val2 vec)
        (format t "~30T~A <-> ~A~45T~A~%" val1 val2 vec))
    
    ;; Print the table header
    (print-header)
    ;; Print the initial unmodified vector
    (format t "~30T~A~45T~A~%" "Initial State" vec)

    ;; Loop over the vector, excluding the last element
    (dotimes (cur (1- (length vec)))
        ;; Initialize the best element and its index
        (let ((best (aref vec cur)) 
              (idx cur))
            ;; Loop over the remaining elements in the vector
            (do ((j (1+ cur) (1+ j))) 
                ((> j (1- (length vec))))
                ;; Check if the current element is better than the best element
                (when (funcall comp (aref vec j) best)
                    ;; Update the best element and its index
                    (:= best (aref vec j)
                        idx j)))
            ;; Swap the current element with the best element
            (rotatef (aref vec cur) (aref vec idx))
            ;; Print the details of the swap operation
            (print-swap-details (aref vec idx) (aref vec cur) vec)))
    ;; Return the sorted vector
    vec)

;; Example usage:
(let ((data #(5 2 9 1 5 6)))
  (print (selection-sort data #'<)))


;; (load "/src/notes/week-3/lec_3.lisp") 