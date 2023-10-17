;; Load the FiveAM library
(asdf:test-system "fiveam")

;; Load the file with the functions that need testing
(load "/src/notes/week-2/lec_2.lisp")  ; Replace with the actual file path

;; Define a test suite
(defpackage :my-tests
  (:use :cl :fiveam))

(in-package :my-tests)

;; Begin defining tests
(test do*-example-test
      "A test for the do*-example function."
      ;; This function has side-effects (printing to stdout) and does not return
      ;; a value that can be easily tested. Therefore, we just run it to ensure
      ;; it does not signal an error.
      (not (signals-error? (do*-example))))

(test count-chars-test
      "A test for the count-chars function."
      ;; Ensure that it counts the occurrences of a character in a string correctly.
      (is (= (count-chars #\a "banana") 3)))

(test count-char-test
      "A test for the count-char function."
      ;; Similar to count-chars, just a different implementation.
      (is (= (count-char #\a "banana") 3)))

(test vowelp-test
      "A test for the vowelp function."
      ;; Ensure it identifies vowels correctly.
      (is (true? (vowelp #\a)))
      (is (false? (vowelp #\b))))

(test consonantp-test
      "A test for the consonantp function."
      ;; Ensure it identifies consonants correctly.
      (is (true? (consonantp #\b)))
      (is (false? (consonantp #\a))))

(test count-vc-do-test
      "A test for the count-vc-do function."
      ;; Ensure it counts vowels and consonants correctly.
      (multiple-value-bind (vowels consonants) 
      (is (= (count-vc-do "example") (3 4)))))

;; Run all tests
;; (run! 'do*-example-test)
(run! 'count-chars-test)
(run! 'count-char-test)
(run! 'vowelp-test)
(run! 'consonantp-test)
(run! 'count-vc-do-test)
