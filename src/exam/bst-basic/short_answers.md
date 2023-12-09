## Questions

**Mock Test on Binary Search Trees (BSTs) - Part 1**
Short Answer Questions:
1. Describe the ordering property of a BST.
   The keys that are less than the parent node are found in the left subtree and the keys that are greater are found on the right subtree.
2. Explain how the 'find' operation works in a BST.
   It starts from the root and compares the key to the roots value, if greater then the key will go down the right subtree and if less than it will go down the left subtree. This process will repeat until the key is found or there is NIL as the outcome. 
3. What is the significance of the 'maximum' operation in a BST and how is it determined?
   The significance is that it will just traverse the rightmost subtree since the value that is the greatest will always reside on the right. 
4. Discuss the complexity of the 'insert' operation in a BST.
5. How does the 'remove' operation affect the structure of a BST?
6. Describe a scenario where the 'minimum' operation in a BST would be used.
7. What are the limitations of using BSTs in data structure applications?
8. How do BSTs compare to other data structures in terms of efficiency and usability?

**Mock Test on Binary Search Trees (BSTs) - Part 2**
Short Answer Questions:
1. How does the structure of a BST ensure efficient searching?
2. Describe how a BST can be represented as a list of lists.
3. Explain the concept of tree rotations in BSTs. How do they affect the tree's balance?
4. Discuss the impact of tree height on the performance of BST operations.
5. What is the significance of the left and right children in a BST node?
6. How do BSTs facilitate the implementation of map and set abstractions?
7. Describe the challenges in removing a node with two children in a BST.
8. Explain the role of balancing in BSTs and its effect on operational complexity.


## Answers 

**Mock Test on Binary Search Trees (BSTs) - Part 1**
Short Answer Questions:
1. Describe the ordering property of a BST.
   - Answer: In a BST, keys that are less than the parent node are found in the left subtree, and keys that are greater than the parent node are found in the right subtree. This property ensures that the BST maintains a sorted collection of objects.
   - Reference: Page 195

2. Explain how the 'find' operation works in a BST.
   - Answer: The 'find' operation in a BST starts at the root and traverses down the tree, comparing the key to find with the current node's key. If the key is less, it moves to the left subtree; if greater, it moves to the right subtree. The process is repeated until the key is found or the subtree is NIL.
   - Reference: Page 68

3. What is the significance of the 'maximum' operation in a BST and how is it determined?
   - Answer: The 'maximum' operation in a BST is used to find the largest key. It is determined by traversing the tree to the rightmost node since, in a BST, all keys in the right subtree of a node are greater than the node's key.
   - Reference: Page 201

4. Discuss the complexity of the 'insert' operation in a BST.
   - Answer: The complexity of the 'insert' operation in a BST is generally O(log n), assuming that the tree is balanced. This is because the height of the tree determines the maximum number of comparisons and movements needed to insert a new node.
   - Reference: Page 202

5. How does the 'remove' operation affect the structure of a BST?
   - Answer: The 'remove' operation can affect the BST's structure by potentially removing a node with children. If the node to be removed has one child or no children, it's straightforward. If it has two children, a typical approach is to find the in-order successor (minimum key in the right subtree) and replace the node with it.
   - Reference: Page 202

6. Describe a scenario where the 'minimum' operation in a BST would be used.
   - Answer: The 'minimum' operation in a BST is used when the smallest key in the tree is needed, such as finding the lowest value in a set of numeric data. This operation involves traversing to the leftmost node of the tree.
   - Reference: Page 195

7. What are the limitations of using BSTs in data structure applications?
   - Answer: The main limitation of using BSTs is that their performance heavily depends on the tree's balance. If the tree becomes skewed (like in the case of inserting sorted data), the operations' complexity can degrade to O(n), similar to a linked list.
   - Reference: Page 206

8. How do BSTs compare to other data structures in terms of efficiency and usability?
   - Answer: BSTs offer efficient search, insertion, and deletion operations (O(log n) in balanced trees), making them more efficient than linear data structures like arrays or linked lists for these operations. However, they can be less efficient than hash tables for search operations, which offer O(1) complexity.
   - Reference: Page 170

**Mock Test on Binary Search Trees (BSTs) - Part 2**
Short Answer Questions:
1. How does the structure of a BST ensure efficient searching?
   - Answer: The structure of a BST, where each left child is less than its parent and each right child is greater, allows operations to disregard half of the tree at each step, leading to efficient O(log n) search times in balanced trees.
   - Reference: Page 68

2. Describe how a BST can be represented as a list of lists.
   - Answer: A BST can be represented as a list of lists where each node is a list containing its key and sub-lists representing its left and right children. This representation captures the hierarchical nature of BSTs in a simple data structure.
   - Reference: Page 170

3. Explain the concept of tree rotations in BSTs. How do they affect the tree's balance?
   - Answer: Tree rotations in BSTs are operations that change the structure without violating the BST properties. They are used in balancing operations to ensure that the tree maintains an O(log n) height, which is crucial for efficient BST operations.
   - Reference: Page 206

4. Discuss the impact of tree height on the performance of BST operations.
   - Answer: The height of a BST directly impacts the performance of its operations. The greater the height, the longer it takes to traverse the tree, reducing efficiency. Ideally, a balanced BST should have a height of O(log n) for optimal performance.
   - Reference: Page 201

5. What is the significance of the left and right children in a BST node?
   - Answer: In a BST, the left child of a node contains a key less than the node's key, and the right child contains a key greater. This ordering property is fundamental to the BST and enables efficient searching, insertion, and deletion operations.
   - Reference: Page 195

6. How do BSTs facilitate the implementation of map and set abstractions?
   - Answer: BSTs allow for efficient searching, insertion, and deletion of keys, making them suitable for implementing map and set abstractions, where quick lookup, addition, and removal of items are essential.
   - Reference: Page 196

7. Describe the challenges in removing a node with two children in a BST.
   - Answer: Removing a node with two children in a BST is challenging because it requires rearranging the tree to maintain the BST property. Typically, the node is replaced with its in-order successor (the smallest node in its right subtree) or predecessor.
   - Reference: Page 202

8. Explain the role of balancing in BSTs and its effect on operational complexity.
   - Answer: Balancing in BSTs ensures the tree remains at or close to its minimum possible height, avoiding skewed formations. This balancing maintains the O(log n) complexity for operations like search, insert, and delete.
   - Reference: Page 202


**Mock Test on Binary Search Trees (BSTs) - Part 1**
Short Answer Questions:
1. Describe the ordering property of a BST.
2. Explain how the 'find' operation works in a BST.
3. What is the significance of the 'maximum' operation in a BST and how is it determined?
4. Discuss the complexity of the 'insert' operation in a BST.
5. How does the 'remove' operation affect the structure of a BST?
6. Describe a scenario where the 'minimum' operation in a BST would be used.
7. What are the limitations of using BSTs in data structure applications?
8. How do BSTs compare to other data structures in terms of efficiency and usability?

**Mock Test on Binary Search Trees (BSTs) - Part 2**
Short Answer Questions:
1. How does the structure of a BST ensure efficient searching?
2. Describe how a BST can be represented as a list of lists.
3. Explain the concept of tree rotations in BSTs. How do they affect the tree's balance?
4. Discuss the impact of tree height on the performance of BST operations.
5. What is the significance of the left and right children in a BST node?
6. How do BSTs facilitate the implementation of map and set abstractions?
7. Describe the challenges in removing a node with two children in a BST.
8. Explain the role of balancing in BSTs and its effect on operational complexity.
