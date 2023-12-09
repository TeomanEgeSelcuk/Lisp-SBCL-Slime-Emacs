;; (load "/src/exam/sorting/heap_sort.lisp")

(defun heap-sort (list)
  "Perform heap sort on the list."
  (let* ((n (length list))
         (start (- (floor (/ n 2)) 1)))
    (dotimes (i (+ start 1))
      (heapify list n (- start i 1)))
    (dotimes (i (- n 1))
      (rotatef (nth 0 list) (nth (- i n 1) list))
      (heapify list (- i n 1) 0))
    list))

(defun heapify (list n i)
  "Heapify the list."
  (let* ((largest i)
         (left (+ (* i 2) 1))
         (right (+ (* i 2) 2)))
    (when (and (< left n) (< (nth largest list) (nth left list)))
      (setf largest left))
    (when (and (< right n) (< (nth largest list) (nth right list)))
      (setf largest right))
    (when (not (= largest i))
      (rotatef (nth i list) (nth largest list))
      (heapify list n largest))))


;; Testing 
(defun generate-random-list (size)
  "Generate a list of random numbers of given size."
  (loop repeat size collect (random 1.0)))

(defun is-sorted (list)
  "Check if the list is sorted."
  (or (null list) (every #'<= list (cdr list))))

(defun adjust-time-for-toronto (hour)
  "Adjust UTC hour to Toronto local time, considering UTC-5 offset."
  (mod (+ hour 19) 24)) ; Adding 19 handles the UTC-5 offset and wraps around 24 hours

(defun adjust-time-for-toronto (hour)
  "Adjust UTC hour to Toronto local time, considering UTC-5 offset."
  (mod (+ hour 19) 24)) ; Adding 19 handles the UTC-5 offset and wraps around 24 hours

(defun adjust-time-for-toronto (hour)
  "Adjust UTC hour to Toronto local time, considering UTC-5 offset."
  (mod (+ hour 19) 24)) ; Adding 19 handles the UTC-5 offset and wraps around 24 hours

(defun execute-sorting ()
  "Execute sorting after a delay and assert the list is sorted."
  ;; Define sleep duration in seconds
  (let ((sleep-duration 1)) ; seconds of delay

    ;; Capture start time in UTC
    (multiple-value-bind (start-second start-minute start-hour) (get-decoded-time)
      ;; Calculate total seconds at end time
      (let* ((total-end-seconds (+ start-second sleep-duration))
             ;; Convert total seconds to hours, minutes, seconds
             (end-hour (+ start-hour (/ total-end-seconds 3600)))
             (end-minute (+ start-minute (/ (mod total-end-seconds 3600) 60)))
             (end-second (mod total-end-seconds 60)))
        ;; Adjust for overflow in minutes
        (when (>= end-second 60)
          (incf end-minute)
          (decf end-second 60))
        ;; Adjust for overflow in hours
        (when (>= end-minute 60)
          (incf end-hour)
          (decf end-minute 60))
        ;; Apply timezone adjustment
        (let ((toronto-end-hour (adjust-time-for-toronto end-hour)))
          (format t "Start Time (Toronto): ~2,'0D:~2,'0D:~2,'0D~%" (adjust-time-for-toronto start-hour) start-minute start-second)
          (format t "Projected End Time (Toronto): ~2,'0D:~2,'0D:~2,'0D~%" toronto-end-hour end-minute end-second))))

    ;; Rest of the function
    (let ((size 9)) ; Set the size as needed
      (let ((list1 (generate-random-list size)))
        (print "Original list:")
        (print list1)
        
        (let ((sorted-list (heap-sort list1)))
          (print "Sorted list:")
          (print sorted-list)
          (assert (is-sorted sorted-list) (sorted-list) "The list is not sorted properly."))))))

(execute-sorting)