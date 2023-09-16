(defun square (x)
  (* x x))

(defun main (arg)
  (format t "The square of ~a is ~a" arg (square arg)))

(main 5) ; Replace 5 with the value you want to pass
