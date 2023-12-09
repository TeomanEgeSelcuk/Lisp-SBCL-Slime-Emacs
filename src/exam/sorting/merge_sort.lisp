(defun merge-lists (list1 list2)
  (cond
    ((null list1) list2)
    ((null list2) list1)
    ((< (first list1) (first list2))
      (cons (first list1) (merge-lists (rest list1) list2)))
    (t
      (cons (first list2) (merge-lists list1 (rest list2))))
  )
)

(defun merge-sort (list1)
  (if (<= (length list1) 1)
    list1 
    (let ((mid (floor (/ (length list1) 2))))
      (merge-lists (merge-sort (subseq list1 0 mid)) (merge-sort (subseq list1 mid)))
    )
  )
)



(defun generate-random-list (size)
  (loop repeat size collect (random 1.0)))

(let ((size 9)) ; Change the size as needed
  (let ((list1 (generate-random-list size)))
    (print list1)
    (print (merge-sort list1))))


;; (load "/src/exam/sorting/merge_sort.lisp")