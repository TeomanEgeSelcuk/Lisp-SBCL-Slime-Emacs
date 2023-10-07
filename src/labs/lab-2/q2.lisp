;; Type this code, as is, in your lab02-ans.lisp file.
;; DO NOT MODIFY THIS CODE
(defparameter *events* (list))

(defun roll-dice ()
  (let* ((s1 (1+ (random 6)))
         (s2 (1+ (random 6)))
         (sum (+ s1 s2)))
    (push (list sum s1 s2) *events*)
    sum))

(defun execute-experiment (n)
  "This function implements a simple interface for analyzing the
   operation of function SIMULATE-DICE-ROLLS"
  (setf *events* (list))
  (let ((res (simulate-dice-rolls n)))
    (format t "simulate-dice-rolls returned: ~a~%(Sum Dice1 Dice2) values: ~a~%" res (reverse *events*))))

(defun SIMULATE-DICE-ROLLS (n)
  (if (< n 2)
      (error "n should be greater than or equal to 2")
      (let ((count 0)
            (prev-sum (roll-dice)))
        (dotimes (i (1- n))
          (let ((curr-sum (roll-dice)))
            (when (> curr-sum prev-sum)
              (incf count))
            (setf prev-sum curr-sum)))
        count)))

;; Test the SIMULATE-DICE-ROLLS function
(let ((n 4)) ; Change the value of n as needed
  (format t "Number of times the next sum is greater: ~a~%" (SIMULATE-DICE-ROLLS n)))

;; (cg:eval-solutions "" :lab02)