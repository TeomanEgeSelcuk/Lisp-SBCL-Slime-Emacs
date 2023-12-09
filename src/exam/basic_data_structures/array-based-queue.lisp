;; (load "/src/exam/basic_data_structures/array-based-queue.lisp")

;; Define a structure for the array-based queue
(defstruct queue
    (size 0)
    (capacity 10)
    (front 0)
    (rear -1)
    (elements (make-array 10 :initial-element nil))
)

;; Helper function to check if the queue is empty
(defun queue-empty-p (q)
    (= (queue-size q) 0)
)

;; Enqueue operation
(defun enqueue (q item)
    (if (= (queue-size q) (queue-capacity q))
        (error "Queque is at its max")
        (progn 
            (setf (queue-rear q) (mod (+ (queue-rear q) 1) (queue-capacity q)))
            (setf (aref (queue-elements q) (queue-rear q)) item)
            (incf (queue-size q))
        )
    )
)

;; Dequeue operation
(defun dequeue (q)
    (if (queue-empty-p q)
        (error "Queque Already empty")
        (progn
            (let ((item (aref (queue-elements q) (queue-front q)))) 
                (setf (aref (queque-elements q) (queque-front q)) nil)
                (setf (queque-front q) (mod (+ (queque-front q) 1) (queque-capacity q)))
                (setf (queque-rear q) (mod (+ (queque-rear q) 1) (queque-capacity q)))
                (decf (queque-size q))
            item)
        )
    )
)

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