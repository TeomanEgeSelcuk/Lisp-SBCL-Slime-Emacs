(defun GRADE (percentage)
  (cond ((>= percentage 90) "A+")
        ((>= percentage 85) "A")
        ((>= percentage 80) "A-")
        ((>= percentage 77) "B+")
        ((>= percentage 73) "B")
        ((>= percentage 70) "B-")
        ((>= percentage 67) "C+")
        ((>= percentage 63) "C")
        (t "F")))

; Example usage:
; (let ((percentage 44)) ; Change the percentage as needed
;   (format t "Grade: ~a~%" (GRADE percentage)))
