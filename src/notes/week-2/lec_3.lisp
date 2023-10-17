;; O(1) - Constant Time:
;; - Operations with constant runtime, independent of input size.
;; - Examples: Array element access, hash table lookup, simple arithmetic.

;; O(log n) - Logarithmic Time:
;; - Runtime increases logarithmically with input size.
;; - Example: Binary search in sorted data.

;; O(n) - Linear Time:
;; - Runtime is directly proportional to input size.
;; - Example: Iterating through a list or array with a single loop.

;; O(n log n) - Linearithmic Time:
;; - Runtime is a product of linear and logarithmic growth.
;; - Examples: Merge sort, heap sort.

;; O(n^2) - Quadratic Time:
;; - Runtime is proportional to the square of input size.
;; - Common with nested loops iterating over the same data.

;; O(2^n) - Exponential Time:
;; - Exponential growth in runtime with input size.
;; - Typically seen in recursive algorithms with multiple recursive calls.

;; O(n!) - Factorial Time:
;; - Runtime grows as the factorial of input size.
;; - Often encountered in problems involving permutations and combinations.

#|
Example 1
Given: T(n) = 5n^2 + 6n*log(n) + 7n + 8

1. The term that increases the fastest as n grows is 5n^2.
2. Removing the other terms, we get:
   O(T(n)) = O(5n^2)
   Simplified: O(n^2).

Example 2
Given: T(n) = 2^n + n^3 + 4n^2 + 5n*log(n)

1. The term that increases the fastest as n grows is 2^n.
2. Removing the other terms, we get:
   O(T(n)) = O(2^n)
   Simplified: O(2^n).

Example 3
Given: T(n) = 3n^4 + 2n^3 + 7n^2 + 6n

1. The term that increases the fastest as n grows is 3n^4.
2. Removing the other terms, we get:
   O(T(n)) = O(3n^4)
   Simplified: O(n^4).

Example 4
Given: T(n) = n*log(n) + 6n + 5*log(n) + 4

1. The term that increases the fastest as n grows is n*log(n).
2. Removing the other terms, we get:
   O(T(n)) = O(n*log(n))
   Simplified: O(n*log(n)).

Example 5
Given: T(n) = 4^n + 3n^5 + 2n^3 + n^2

1. The term that increases the fastest as n grows is 4^n.
2. Removing the other terms, we get:
   O(T(n)) = O(4^n)
   Simplified: O(4^n).
|#

