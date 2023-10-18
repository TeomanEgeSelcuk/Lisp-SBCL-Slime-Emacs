# **Sequential Search:**
   It means going through a list of items one by one until you find what you're looking for.

   - **Example:** You have a line of 10 people and you're looking for someone named John. You'll ask each person in line, one by one, if their name is John.

### Searching in an Unordered List:
   - **Given Vector:** `#(15 18 2 19 18 0 8 14 19 14)`
   - **Finding Key 18:** You would need 2 comparisons because 18 is the second element in the vector.
   - **Finding Key 3:** You would need 10 comparisons as 3 is not in the vector, so you would go through the entire vector.

### Searching in an Ordered List:
   - **Given Vector:** `#(3 5 6 8 11 12 14 15 17 18)`
   - **Finding Key 13:** You would need 7 comparisons because you would go sequentially until 14, which is the closest higher number to 13.

### 2. **Sequential Search Complexity:**
   - **Best Case:** You find the item on the first try.
   - **Worst Case:** You have to look through every item in the list.
   - **Example:** In a list of names, the best case is if John is the first person you ask, and the worst case is if John is the last person or isn’t in the line at all.

#  **Binary Search:**
   - **Simple Explanation:** This is like looking for a word in a dictionary. You don’t go page by page; you open it in the middle and decide whether the word is before or after that midpoint.
   - **Example:** Searching for the word “apple,” you open the dictionary in the middle, find “mango,” and know “apple” will be in the first half of the dictionary. You then divide that part and repeat the process until you find “apple.”

### Binary Search Implementation:
   - **Given Code Example:** It’s like having a list of numbers from 1 to 6 and you're looking for number 6. You start in the middle at number 4. Since 6 is greater than 4, you now look in the right half, and so on, until you find the number 6.

```lisp
    (defun bin-search (val vec &optional (pos 0))
        "Search for val in sorted vector vec, return position if found, NIL otherwise."
        
        ; Check if there are more than one element in the vector
        (if (> (length vec) 1)
            ; If yes, find the middle point and the middle element
            (let* ((midpt (floor (length vec) 2))
                (midel (aref vec midpt)))
                
                ; Compare the middle element with the target value
                (cond 
                    ; If middle element is smaller, search in the right half of the vector
                    ((< midel val) 
                    (bin-search val 
                                (subseq vec midpt) ; right half of the vector
                                (+ pos midpt))) ; adjust the position accordingly
                    
                    ; If middle element is larger, search in the left half of the vector
                    ((> midel val) 
                    (bin-search val 
                                (subseq vec 0 midpt) ; left half of the vector
                                pos)) ; keep the position the same
                    
                    ; If middle element is equal to the target value, return the position
                    (t 
                    (+ pos midpt))))
                
            ; If there is only one element left, check if it is equal to the target value
            (when (= (aref vec 0) val)
                pos))) ; if yes, return the position, otherwise it returns NIL by default
```

### Steps of Binary Search:

1. **Find the Middle:** Look at the middle item of the sorted list.
2. **Compare:** If it's the target value, great! If not, is it higher or lower?
3. **Half the List:** Depending on your answer, ignore one half of the list.
4. **Repeat:** Treat the remaining half as a new list and repeat the steps.

### **Binary Search Complexity:**
   - **Explanation:** Each step in a binary search reduces the number of elements you’re looking at by half. This makes it a very efficient way to search in a sorted list.
   - **Example:** If you have 1024 pages in a dictionary, with binary search, you’ll find the word you're looking for in about 10 steps (or fewer), because each step halves the number of pages you need to look at (1024, 512, 256, and so on).

### Caveat:
   - **Sorting Required:** Binary search is fast, but it only works effectively on a sorted list. If the list isn’t sorted, you’ll need to sort it first before doing a binary search, which can take up some time.
   - **Example:** It’s like having a shuffled deck of cards. Before you can use the binary search method to find a specific card, you need to sort the cards in order.

# Sorting
- **Objective:** Arrange elements in a specific order (ascending or descending).
- **Different Aspects:**
  - **In-place:** Sorting done within the original array, not using extra space.
  - **Stable:** Keeps the relative order of equal elements.
  - **Online:** Can sort even if not all elements are given at the start.

### Selection Sort
- **In-Place Sorting Algorithm:** Doesn't require extra space.
- **How It Works:**
  - Go through the list.
  - Find the smallest (or largest) element.
  - Swap it with the first unsorted element.
  - Repeat for the rest of the unsorted elements.

### Example:
- **Given Array:** `#(3 1 0 7 8 2)`
- **After Sorting (Ascending):** `#(0 1 2 3 7 8)`

### How Selection Sort Works (Step-by-Step):
1. Start with the entire array. The smallest element is 0. Swap 0 and 3: `#(0 1 3 7 8 2)`.
2. Consider the subarray from the second element. The smallest is 1 (it's already in place): `#(0 1 3 7 8 2)`.
3. And so on, until the array is sorted.

### Time Complexity Analysis of Selection Sort
The selection sort algorithm performs a series of comparisons and potentially a swap in each pass through the list.

#### 1. Comparisons
Each pass through the list requires a number of comparisons:

- First pass: \(n-1\) comparisons
- Second pass: \(n-2\) comparisons
- ...
- \(n-1\)-th pass: 1 comparison

The total number of comparisons is the sum of the first \(n-1\) positive integers:

\[
\text{Total Comparisons} = \sum_{i=1}^{n-1} i = \frac{n(n-1)}{2}
\]

So, the time complexity for comparisons in selection sort is \(O(n^2)\).

#### 2. Swaps
In the worst case, you perform a swap at each pass through the list, so there would be \(n-1\) swaps, giving a linear time complexity for swaps, \(O(n)\).

#### Total Time Complexity
The overall time complexity of selection sort is dominated by the comparisons, so it is \(O(n^2)\).

### Code

```lisp
(defun selection-sort (vec comp)
    ; Loop over the vector, excluding the last element
    (dotimes (cur (1- (length vec)))
        ; Initialize the best element and its index
        (let ((best (aref vec cur)) ; (aref vec cur) retrieves the element of vec at the current index cur and assigns it to the variable best.
                    (idx cur)) ; (idx cur) initializes the variable idx to the current index cur
            ; Loop over the remaining elements in the vector
            (do ((j (1+ cur) (1+ j))) ; j is initially cur+1
                    ((> j (1- (length vec)))) ; When j is greater than or equal to the length of vec minus 1, the loop stops.
                ; Check if the current element is better than the best element
                (when (funcall comp (aref vec j) best) ; Call the comparison function with the current element and the best element as arguments
                    ; Update the best element and its index
                    (:= best (aref vec j) ; Assign the current element to the best element
                        idx j))) ; Assign the current index to the index of the best element
            ; Swap the current element with the best element
            (rotatef (aref vec cur) (aref vec idx))))
    ; Return the sorted vector
    vec)
```

More readable and shorter:

```
(defun selection-sort (vec comp)
    (dotimes (i (1- (length vec)))
        (let ((best (aref vec i))
                    (idx i))
            (dotimes (j (- (length vec) i 1))
                (when (funcall comp (aref vec (+ i j 1)) best)
                    (:= best (aref vec (+ i j 1))
                             idx (+ i j 1))))
            (rotatef (aref vec i) (aref vec idx))))
    vec)
```


### Complexity:
- **Comparisons:** It has O(n^2) comparisons in total because each element is compared with every other element.
- **Exchanges:** O(n) exchanges in the worst case because at most, you might make an exchange during each pass.

### Practical Example:
Imagine you have a hand of playing cards and you want to sort them. 
- **Unsorted Cards:** 3, 1, 0, 7, 8, 2
- Using selection sort, you look at all the cards and pick the smallest one, 0, and swap it with the first card, 3.
- **After First Pass:** 0, 1, 3, 7, 8, 2
- Then you ignore the 0 (since it’s already sorted), look at the rest of the cards, pick the smallest one, 1, which is already in the correct position, so no swap needed.
- **After Second Pass:** 0, 1, 3, 7, 8, 2
- And you continue this process until all cards are sorted in ascending order.

### Summary:
- **Selection Sort:** A simple, in-place sorting algorithm.
- **Pros:** Easy to understand and implement.
- **Cons:** Not efficient for large datasets due to its O(n^2) time complexity for comparisons.
