;; Array-based Queue Implementation in Lisp

;; Define a structure for the array-based queue
(defstruct queue
  (size 0)         ; current size of the queue
  (capacity 10)    ; maximum capacity of the queue
  (front 0)        ; index of the front element
  (rear -1)        ; index of the rear element
  (elements (make-array 10 :initial-element nil))) ; array to store elements

;; Helper function to check if the queue is empty
(defun queue-empty-p (q)
  (= (queue-size q) 0))

;; Enqueue operation
(defun enqueue (q item)
    ;; q: The queue where the item will be added.
    ;; item: The item to be added to the queue.
    ;; Check if the queue is full
    (if (= (queue-size q) (queue-capacity q))
            ;; If the queue is full, throw an error
            (error "Queue is full.")
            ;; If the queue is not full, add the item
            (progn
                ;; Calculate the new rear position
                (setf (queue-rear q) (mod (1+ (queue-rear q)) (queue-capacity q))) ; rear = (rear + 1) mod capacity
                ;; Add the item at the new rear position
                (setf (aref (queue-elements q) (queue-rear q)) item)
                ;; Increase the size of the queue by 1
                (incf (queue-size q)))))

;; Dequeue operation
(defun dequeue (q)
    ;; Check if the queue is empty
    (if (queue-empty-p q)
            ;; If the queue is empty, throw an error
            (error "Queue is empty.")
            ;; If the queue is not empty, remove the item
            (progn
                ;; Store the item at the front of the queue
                (let ((item (aref (queue-elements q) (queue-front q))))
                    ;; Remove the item from the queue
                    (setf (aref (queue-elements q) (queue-front q)) nil)
                    ;; Calculate the new front position
                    (setf (queue-front q) (mod (1+ (queue-front q)) (queue-capacity q)))
                    ;; Calculate the new rear position
                    (setf (queue-rear q) (mod (1+ (queue-rear q)) (queue-capacity q)))
                    ;; Decrease the size of the queue by 1
                    (decf (queue-size q))
                    ;; Return the removed item
                    item))))

(defun print-queue (q)
  "Convert the queue to a string for printing."
  (with-output-to-string (s)
    (loop for i from (queue-front q) to (queue-rear q)
          do (format s "~A " (aref (queue-elements q) i)))))

(defun run-queue-example ()
    (let ((q (make-queue)))
        (enqueue q 1)
        (enqueue q "yes")

        (format t "Queue after enqueuing: ~A~%" (print-queue q))

        (let ((dequeued-item (dequeue q)))
            (format t "Dequeued item: ~A~%" dequeued-item)
            (format t "Queue after dequeuing: ~A~%" (print-queue q)))

        (format t "Queue size after dequeuing: ~A~%" (queue-size q))
        (format t "Is the queue empty? ~A~%" (queue-empty-p q))

        ;; Dequeue again
        (let ((dequeued-item (dequeue q)))
            (format t "Dequeued item: ~A~%" dequeued-item)
            (format t "Queue after dequeuing: ~A~%" (print-queue q)))

        (format t "Queue size after dequeuing: ~A~%" (queue-size q))
        (format t "Is the queue empty? ~A~%" (queue-empty-p q))))

(run-queue-example)