## Questions 

**Mock Test: Sorting Algorithms and Heap Sort Implementation in Lisp**

**Short Answer Questions**
1. Explain the primary difference between merge sort and heap sort in terms of their algorithmic strategy.
2. What is the worst-case time complexity of heap sort and why?
3. In what scenario is heap sort more efficient than quicksort?
4. Describe the heap property in the context of heap sort.
5. How does a max heap differ from a min heap in heap sort?
6. Why is heap sort considered an in-place sorting algorithm?
7. What is the significance of a binary heap in implementing heap sort?

**Additional Mock Test: Heap Sort Algorithm Implementation in Lisp**

**Short Answers**
1. Define the term 'heap' in the context of sorting algorithms.
2. What is the significance of a 'complete binary tree' in heap sort?
3. How does the stability of heap sort compare to other sorting algorithms like merge sort?
4. Explain how heap sort can be optimized for a nearly sorted array.
5. Describe the role of the heapify function in heap sort.
6. What is the impact of using a min heap instead of a max heap in heap sort?
7. Can heap sort be implemented as a stable sort? Explain.

## Answers

**Mock Test: Sorting Algorithms and Heap Sort Implementation in Lisp**

**Short Answer Answers**
1. Merge vs. Heap Sort: Merge sort is a divide-and-conquer algorithm that divides the list into halves, sorts them, and then merges them. Heap sort, on the other hand, organizes the data into a heap structure and repeatedly extracts the maximum element to sort the array.
2. Heap Sort Complexity: The worst-case time complexity of heap sort is O(n log n). This is due to the heapify process for each of the n elements, which takes O(log n) time.
3. Heap Sort Efficiency: Heap sort is more efficient than quicksort in scenarios where the data set is very large and the cost of data movement is high since heap sort has better locality of reference.
4. Heap Property: In a heap, every parent node is greater than or equal to (in a max heap) or less than or equal to (in a min heap) its child nodes.
5. Max vs. Min Heap: In a max heap, the parent node is always larger than its children, whereas in a min heap, the parent node is always smaller.
6. Heap Sort In-Place: Heap sort is considered in-place because it requires a constant amount (O(1)) of additional memory space, beyond the input list.
7. Binary Heap in Heap Sort: A binary heap is a complete binary tree used in heap sort for efficiently finding and removing the highest (or lowest) priority element.

**Additional Mock Test: Heap Sort Algorithm Implementation in Lisp**

**Short Answer Answers**
1. Heap Definition: A heap is a specialized tree-based data structure that satisfies the heap property: in a max heap, for any given node I, the value of I is greater than or equal to the values of its children, and vice versa for a min heap.
2. Complete Binary Tree Significance: In heap sort, a complete binary tree is used to ensure that the heap can be efficiently represented in an array without any gaps, facilitating swift traversal and manipulation.
3. Heap Sort Stability: Heap sort is not a stable sort. This means it does not necessarily preserve the order of equal elements as they appeared in the input.
4. Optimizing for Nearly Sorted Arrays: Heap sort is not particularly optimized for nearly sorted arrays; however, the initial building of the heap can be faster if the array is already partially sorted.
5. Heapify Role: The heapify function maintains the heap property by comparing a node with its children and swapping them if necessary, working recursively towards the leaf nodes.
6. Impact of Min Heap vs Max Heap: Using a min heap sorts the elements in ascending order, while a max heap sorts in descending order. The choice affects the sorting order of the heap sort algorithm.
7. Heap Sort as Stable Sort: Heap sort cannot be easily made stable due to the heapify process, which involves swapping elements, potentially disrupting the original order of equal elements.