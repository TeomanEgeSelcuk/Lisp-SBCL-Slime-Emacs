## Questions

**Test 1: Trees in Data Structures and Lisp - Short Answer Questions**

1. Explain the significance of tree data structures in the realm of computing and algorithms. Provide examples of their applications.
   The Tree data structure creates a hierachial data structure with parent-child relationships where each node could be linked to another. Examples include file systems , HTL DOM and decision trees in machine learning
2. Describe the basic properties that define a tree data structure.
   Hierachial data structure where there is a root node, node a the very top and non root nodes which are linked with a parent as a parent child relationship. 
3. What distinguishes a binary tree from other forms of trees?
   A binary tree parent can at most have 2 childs, one on the right and one on the left. Whilst other tree nodes can have more childs per node.
4. How is the concept of trees applied in the evaluation of Lisp expressions?
   It can be made so that there is an hierarchial structure of parent child operators and operands, facilitating parsing and evaluating. 
5. Discuss the role of parse trees in representing the syntactical structure of programming languages.
   Show how a piece of code is constructed by splitting it into parts and shows how it fits together as a whole. Provides insight into the syntax 
6. Explain the term 'root node' in the context of tree data structures.
   The root node is the topmost node of a tree where it will have no parent and will be the starting point of the tree. 
7. Describe the significance of having a 'unique path' in a tree structure.
   A unique path is the path where the outcome will only be achieved if theres only one way to reach the outcome from another. Aiding in efficient dta retreival
8. Explain how trees are utilized in the syntax tree of a programming language.
   It is the hierarchial structure of a programming code where each node are programming language codes which helps the computer understand the code better.  

**Test 2: Additional Short Answer Questions**

1. Explain the role of 'leaf nodes' in a binary tree and their properties.
   A leaf node has no children but will have a parent. It indicates the end of a branch in the tree structure. 
2. Describe the differences between a 'full binary tree' and a 'complete binary tree'.
   A full binary tree has 0 or 2 children for each node except on every level. A complete binary tree will have 2 children for each node on all levels but maybe the last level. 
3. What is a 'balanced binary tree', and why is it important in algorithm efficiency?
   A balanced binary tree is a tree will have mostly equal heights of subtree, where the complexity of search is O(logn). 
4. How does a 'binary search tree' differ from a regular binary tree?
   It is an ordered binary tree where the subtrees on the right are greater than the left subtrees
5. Explain the concept of 'tree rotation' in balanced trees.
   It is re-balancing the tree for more efficient insertion and deletion operations.
6. Describe the process of 'tree traversal' and name its types.
   Tree rotations are searching for a value in each node of a tree in a specific order 
7. What is a 'binary heap', and how is it implemented in the context of trees?
   Used for binary queue where it either uses a max or min heap for the tree so parent nodes are either greater/less than the children
8. Explain the concept of 'path compression' in union-find trees.
   

## Answers

**Test 1: Trees in Data Structures and Lisp - Short Answer Questions - Answers**

1. Explain the significance of tree data structures in the realm of computing and algorithms. Provide examples of their applications.
   - Answer: Tree data structures are crucial in computing and algorithms for their hierarchical organization. Examples include file systems, HTML DOM, and decision trees in machine learning.

2. Describe the basic properties that define a tree data structure.
   - Answer: A tree is a hierarchical data structure with nodes connected by edges. It has a root node, and each non-root node has one parent, defining parent-child relationships.

3. What distinguishes a binary tree from other forms of trees?
   - Answer: A binary tree is a tree where each node can have at most two children: a left child and a right child. Other trees may have more than two children per node.

4. How is the concept of trees applied in the evaluation of Lisp expressions?
   - Answer: Trees represent the hierarchical structure of Lisp expressions, where nodes are operators or operands, facilitating parsing and evaluation.

5. Discuss the role of parse trees in representing the syntactical structure of programming languages.
   - Answer: Parse trees break down program code into hierarchical structures, showing how language constructs are nested and providing insights into syntax.

6. Explain the term 'root node' in the context of tree data structures.
   - Answer: The root node is the topmost node in a tree, serving as the starting point for traversing the tree.

7. Describe the significance of having a 'unique path' in a tree structure.
   - Answer: A unique path between any two nodes in a tree ensures that there's only one way to reach one from the other, aiding in efficient data retrieval.

8. Explain how trees are utilized in the syntax tree of a programming language.
   - Answer: Syntax trees represent the hierarchical structure of program code, where nodes correspond to language constructs, aiding in parsing and analysis.


**Test 2: Additional Short Answer Questions - Answers**

1. Explain the role of 'leaf nodes' in a binary tree and their properties.
   - Answer: Leaf nodes are terminal nodes in a binary tree with no children. They represent endpoints of branches and contain data.

2. Describe the differences between a 'full binary tree' and a 'complete binary tree'.
   - Answer: A full binary tree has every node with either 0 or 2 children. A complete binary tree has all levels filled except possibly the last level, which is filled from left to right.

3. What is a 'balanced binary tree', and why is it important in algorithm efficiency?
   - Answer: A balanced binary tree maintains roughly equal heights of subtrees, ensuring operations like insertion and search have O(log n) complexity, crucial for efficiency.

4. How does a 'binary search tree' differ from a regular binary tree?
   - Answer: A binary search tree (BST) follows an ordering property: for each node, elements in the left subtree are less, and in the right subtree are greater. A regular binary tree lacks this ordering.

5. Explain the concept of 'tree rotation' in balanced trees.
   - Answer: Tree rotation is a technique used to re-balance binary trees, preserving their properties during insertion and deletion operations.

6. Describe the process of 'tree traversal' and name its types.
   - Answer: Tree traversal is visiting nodes in a specific order. Types include Depth-First Search (pre-order, in-order, post-order) and Breadth-First Search (level order).

7. What is a 'binary heap', and how is it implemented in the context of trees?
   - Answer: A binary heap is a complete binary tree used for priority queues. It's implemented with the heap property, where parent nodes are less (min heap) or greater (max heap) than their children.

8. Explain the concept of 'path compression' in union-find trees.
   - Answer: Path compression is an optimization technique in union-find data structures. It shortens paths by linking nodes directly to the root, improving performance.

**Test 1: Trees in Data Structures and Lisp - Short Answer Questions**

1. Explain the significance of tree data structures in the realm of computing and algorithms. Provide examples of their applications.
2. Describe the basic properties that define a tree data structure.
3. What distinguishes a binary tree from other forms of trees?
4. How is the concept of trees applied in the evaluation of Lisp expressions?
5. Discuss the role of parse trees in representing the syntactical structure of programming languages.
6. Explain the term 'root node' in the context of tree data structures.
7. Describe the significance of having a 'unique path' in a tree structure.
8. Explain how trees are utilized in the syntax tree of a programming language.

**Test 2: Additional Short Answer Questions**

1. Explain the role of 'leaf nodes' in a binary tree and their properties.
2. Describe the differences between a 'full binary tree' and a 'complete binary tree'.
3. What is a 'balanced binary tree', and why is it important in algorithm efficiency?
4. How does a 'binary search tree' differ from a regular binary tree?
5. Explain the concept of 'tree rotation' in balanced trees.
6. Describe the process of 'tree traversal' and name its types.
7. What is a 'binary heap', and how is it implemented in the context of trees?
8. Explain the concept of 'path compression' in union-find trees.