## Understanding Algorithms More

**What You'll Learn:**
After this lesson, you'll be able to:
1. Use Big-O notation to talk about how fast a program runs.

**Examples and Explanations:**

**A. Simple Assignments and Calculations**
- When we have simple assignments like `x = y + b`, the time it takes to run is constant. We don’t worry if there are loops or not.
- We often just say the running time is "O(1)" because it doesn't change even if we have more data.

**B. More Complex Assignments with LET Statements**
- For LET statements where multiple variables are set at once, like:
  ```lisp
  (let ((a 5) (b 6) ... )
    ...)
  ```
  We add up the time of each assignment to get the total time.
- But again, if there are no loops involved, we simply say the time is "O(1)".

**C. Loops and Running Time**
- When loops come into play, the time a program takes to run depends on how many times the loop runs.
- For a loop that repeats 'n' times, doing constant work each time, we say the time is "O(n)".
- For nested loops, where each loop runs 'n' times, the time becomes "O(n^2)" because every item is paired with every other item.

**D. Special Cases of Loops**
- Some loops don't just add 1 each time, they might double the counter each time. This changes how we calculate the time.
- We have to consider this different growth when talking about the running time.


**Summary:**
In this lesson, you’ve learned how to use Big-O notation to describe how quickly different types of code run. We looked at simple assignments, LET statements, and loops to understand how each impacts the time it takes for a code to execute.

### Example Revisions for Clarity:

**Original:**
```lisp
(setf x (+ y b))
```
**Explanation:** Assigns the sum of `y` and `b` to `x`. It takes constant time.

**Original:**
```lisp
(dotimes (i n)
  (:= x (+ i b))
  (:= y (* x 2)))
```
**Explanation:** A loop that runs 'n' times, updating `x` and `y` each time. The running time is proportional to 'n', making it "O(n)".

**Original:**
```lisp
(let ((a 5) (b 6) (c 10) (d 0) ...)
  ...)
```
**Explanation:** Sets multiple variables at once. Each assignment takes constant time, but we add them up to get the total time for this statement.

**Original:**
```lisp
(do ((i 1 (* i 2)))
  ((>= i n))
  (:= k (+ k 4)))
```
**Explanation:** A loop where the counter doubles each time, not increases by 1. We have to account for this when figuring out the running time.

## Lisp Code

1. **Function f:**
   ```lisp
   (defun f (a b)
     (let ((acc 0))
       (dotimes (i a)
         (incf acc))))
   ```
   - **Explanation:** This Lisp function `f` takes two arguments `a` and `b`. It initializes a variable `acc` to 0. Then, it loops `a` times, incrementing `acc` each time. The value of `b` is not used in this function.

2. **Function g:**
   ```lisp
   (defun g(n)
     (dotimes (i n)
       (dotimes (j n (f i j))
         (setf x (+ i j)))))
   ```
   - **Explanation:** This function `g` takes one argument `n`. It has a nested loop that runs `n` times for both `i` and `j`. Inside the inner loop, it calculates the sum of `i` and `j` and assigns it to `x`. After each inner loop, it calls function `f` with `i` and `j` as arguments.

### Mathematical Notation

It seems like the mathematical notation is trying to analyze the time complexity of the functions `f` and `g`. However, the notation is not clear, and there are missing parts. I'll try to interpret it as best as I can.

1. **Time Complexity of Function f (Tf(n)):**
   - **Tf(n) = 1 + ∑(from i=0 to a-1) 1**
   - **Explanation:** The time complexity of function `f` is linear with respect to `a`, because it loops `a` times. Each loop iteration takes constant time.

2. **Time Complexity of Function g (Tg(n)):**
   - **Tg(n) = (n^2) + n(n-1)/2**
   - **Explanation:** The time complexity of function `g` is quadratic because of the nested loops. Each loop runs `n` times, leading to `n^2` iterations. Additionally, there is a call to function `f` inside the loop, adding to the total time complexity.

3. **Big O Notation:**
   - **O(n^2)**
   - **Explanation:** The dominating term in the time complexity expression is `n^2`, so the Big O notation, which describes the upper bound of the time complexity, is O(n^2).

### Analyzing the `g` Function

The `g` function contains two nested loops, each running `n` times, and a call to the `f` function inside the inner loop. The `f` function runs in linear time with respect to its first parameter. Here’s the `g` function again for reference:

```lisp
(defun g(n)
 (dotimes (i n)
   (dotimes (j n (f i j))
     (setf x (+ i j)))))
```

### Counting the Operations

1. **Nested Loops:** The nested loops lead to `n * n = n^2` iterations. Each iteration involves a constant amount of work (setting `x` to `i + j`), so this part contributes `O(n^2)` to the time complexity.

2. **Call to `f`:** The `f` function is called `n^2` times because it’s inside the nested loops. However, the `f` function itself has a loop that runs `i` times (where `i` varies from `0` to `n-1`). So, we need to calculate the sum of the first `n-1` positive integers to find out how many times the `f` function’s loop is executed in total. This sum is given by the formula:
   
   \[
   \sum_{i=0}^{n-1} i = \frac{n(n - 1)}{2}
   \]

   This formula is derived from the sum of an arithmetic series, where you’re adding up all the numbers from `1` to `n-1`.

### Total Running Time

Adding up the contributions from the nested loops and the call to `f`, we get the total running time function:

\[
T_g(n) = n^2 + \frac{n(n - 1)}{2}
\]

### Big O Notation

The Big O notation is used to describe the upper bound of an algorithm’s running time, focusing on the term that grows the fastest as the input size increases. In this case, the `n^2` term dominates, so the Big O notation is:

\[
O(n^2)
\]

### Conclusion

The `n(n-1)/2` term comes from counting the total number of operations performed by the `f` function when it’s called inside the nested loops of the `g` function. The `n^2` term comes from the nested loops themselves. The Big O notation is derived by focusing on the term that grows the fastest as `n` increases, which is `n^2` in this case.


### Summary

- Function `f` has a linear time complexity because it has a single loop that runs `a` times.
- Function `g` has a quadratic time complexity because it has nested loops, each running `n` times, and it also calls function `f`.
- The overall time complexity of function `g` is O(n^2) because the quadratic term dominates as `n` becomes large.

