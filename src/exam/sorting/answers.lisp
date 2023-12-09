;; MERGE SORT CODE
(defun merge-lists (list1 list2)
    (cond 
        ((null list1) list2) ; If list1 is empty, return list2
        ((null list2) list1) ; If list2 is empty, return list1
        ((< (first list1) (first list2)) ; If the first element of list1 is less than the first element of list2
         (cons (first list1) (merge-lists (rest list1) list2))) ; Add the first element of list1 to the merged list and recursively call merge-lists on the rest of list1 and list2
        (t ; Otherwise
         (cons (first list2) (merge-lists list1 (rest list2)))))) ; Add the first element of list2 to the merged list and recursively call merge-lists on list1 and the rest of list2

(defun merge-sort (list1)
  (if (<= (length list1) 1)
      list1
      (let ((mid (floor (/ (length list1) 2))))
        (merge-lists (merge-sort (subseq list1 0 mid)) (merge-sort (subseq list1 mid))))))

;; HEAP SORT CODE

(defun heapify (list n i)
  "Heapify the list by rearranging its elements in a way that satisfies the heap property.
  
  Args:
    list: The list to be heapified.
    n: The size of the list.
    i: The index of the current element being heapified."
    
    (let* ((largest i) ; Initialize largest as root
           (left (+ (* 2 i) 1)) ; left = 2*i + 1
           (right (+ (* 2 i) 2))) ; right = 2*i + 2
        (when (and (< left n) (> (nth left list) (nth largest list))) ; If left child is larger than root
            (setq largest left)) ; Make left child as largest
        (when (and (< right n) (> (nth right list) (nth largest list))) ; If right child is larger than largest so far
            (setq largest right)) ; Make right child as largest
        (when (not (= largest i)) ; If largest is not root
            (rotatef (nth i list) (nth largest list)) ; Swap
            (heapify list n largest)))) ; Recursively heapify the affected sub-tree

(defun heap-sort (list)
    (let* ((n (length list)) ; Get the length of the list
           (start (- (floor (/ n 2)) 1))) ; Start from last non-leaf node : floor(n / 2) - 1, finding the position of the last
                                          ; parent node in the heap, which is the node that is not a leaf (doesn't have children)

        ;; Building a max heap from the list.
        ;; Making sure that each parent node is larger than its children, property of a max heap.
        (dotimes (i (+ 1 start)) 
            (heapify list n (- start i 1))) ; Build heap. The (- start i 1) expression calculates the index of the current parent node.

        ;; Preparing for heap sort by determining the list's size and locating the last parent node.
        (dotimes (i (1- n))
            (rotatef (nth 0 list) (nth (- n i 1) list)) ; Move current root to end
            (heapify list (- n i 1) 0))) ; Call max heapify on the reduced heap, max element at top of the heap. (- n i 1) 
                                         ; expression calculates the new size of the heap, and 0 is the index of the new root
    list) ; Return the sorted list