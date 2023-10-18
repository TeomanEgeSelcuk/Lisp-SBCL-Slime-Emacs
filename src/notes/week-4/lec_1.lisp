;;; Example 1:
(format t "~%Example 1:~%")

#|
Use case:

(In simple and short terms:
- Purpose: To sort a vector using a comparison function.
- Helpful: Sorting data in a specific order as per the comparison function.

Rules for input code:

- The input code defines a function 'insertion-sort' that takes two arguments: 'vec' (a vector) and 'comp' (a comparison function).
- It sorts the 'vec' in ascending order using the 'comp' function to compare elements.
- The function uses nested loops for sorting.
- It iterates over the vector from the second element to the last element.
- For each element, it compares it with the previous element using the 'comp' function and swaps them if needed.

Steps:

1. Iterate over the vector from the second element (index 1) to the last element (index (length vec) - 1).
2. For each element at index 'i', iterate over the vector from the current element (index 'i') to the first element (index 0).
3. For each inner loop iteration at index 'j', check if 'j' is negative. If it is, exit the loop.
4. Compare the current element 'vec[j+1]' with the previous element 'vec[j]' using the 'comp' function.
5. If 'vec[j+1]' is less than 'vec[j]', swap the elements using 'rotatef'.
6. If 'vec[j+1]' is greater than or equal to 'vec[j]', exit the inner loop.
7. Repeat steps 3 to 6 until the inner loop completes.
8. Repeat steps 2 to 7 for all elements in the vector, effectively sorting the vector in ascending order.
9. Return the sorted vector.

Syntax:

- (defun insertion-sort (vec comp) ...): Defines the 'insertion-sort' function that takes a vector 'vec' and a comparison function 'comp'.
- (dotimes (i (1- (length vec))) ...): Iterates over 'i' from 0 to (length vec - 2).
- (do ((j i (1- j))) ...): Iterates over 'j' from 'i' to 0 with a step of -1.
- (minusp j): Checks if 'j' is negative.
- (funcall comp ...): Calls the comparison function 'comp'.
- (aref vec ...): Accesses elements of the vector 'vec'.
- (rotatef ...): Swaps two elements.

|# 

(defun insertion-sort (vec comp)
  "Sort vector vec using the comparison function comp."

  ;; Initialize a counter for steps
  (let ((step-counter 0))

    ;; Iterate over the vector from the second element to the last element
    (dotimes (i (1- (length vec)))
      
      ;; Iterate over the vector from the current element to the first element
      (do ((j i (1- j)))
        
        ;; Check if j is negative
        ((minusp j))

        ;; Increment the step counter
        (incf step-counter)

        ;; Check if the current element is less than the previous element
        (if (funcall comp (aref vec (1+ j)) (aref vec j))
            
            ;; If yes, swap them
            (progn
              (rotatef (aref vec (1+ j)) (aref vec j))
              ;; Print the step, indices, comparison, swap status, and the vector
              (format t "Step ~a: Index i=~a, Index j=~a, Comparison: ~a, Swap: Yes, Vector: ~a~%"
                      step-counter i j (funcall comp (aref vec (1+ j)) (aref vec j)) vec))
            
            ;; If not, exit the loop
            (progn
              ;; Print the step, indices, comparison, swap status, and the vector
              (format t "Step ~a: Index i=~a, Index j=~a, Comparison: ~a, Swap: No, Vector: ~a~%"
                      step-counter i j (funcall comp (aref vec (1+ j)) (aref vec j)) vec)
              (return))))))

        ;; Return the sorted vector
        vec)


; Usage:
(let ((my-vector #(5 3 8 1 7 2))) ; Define a vector named my-vector
  (setq my-vector (insertion-sort my-vector #'<)) ; Sort the vector
  (format t "Sorted Vector: ~a~%" my-vector))

;; (load "/src/notes/week-4/lec_1.lisp")