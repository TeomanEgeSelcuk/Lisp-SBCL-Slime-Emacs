(defun fact (n)
  (if (= n 0)
      1              ; Base case: factorial of 0 is 1
      (* n (fact (- n 1)))))
