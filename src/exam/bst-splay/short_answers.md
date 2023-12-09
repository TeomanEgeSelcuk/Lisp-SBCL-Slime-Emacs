## Questions 

**Mock Test on Splay Trees and Tree Rotations - Part 1**
Short Answer Questions:
1. What is a Splay Tree and how does it differ from other BSTs?
2. Explain the key properties of a Splay Tree.
3. Describe the process of splaying in a Splay Tree.
4. How do Splay Trees maintain their balance?
5. Discuss the benefits of using Splay Trees over other self-balancing trees.
6. Explain the role of recently accessed elements in Splay Trees.
7. What is the impact of splaying on tree operations like search and insert?
8. How does the Splay Tree handle duplicate keys?

**Mock Test on Splay Trees and Tree Rotations - Part 2**
Short Answer Questions:
1. Discuss the advantages of splay operations in terms of tree balancing.
2. Explain the impact of splay tree rotations on search efficiency.
3. How does a splay tree adapt to different access patterns?
4. What are the key differences between splay trees and AVL trees?
5. Describe the process of accessing a node in a splay tree and its effects.
6. How do splay trees ensure that frequently accessed elements are quickly reachable?
7. Discuss the time complexity of operations in a splay tree.
8. Explain the concept of 'amortized analysis' in the context of splay trees.

## Answers 

**Mock Test on Splay Trees and Tree Rotations - Part 1**
Short Answer Questions:
1. What is a Splay Tree and how does it differ from other BSTs?
   - Answer: A Splay Tree is a self-adjusting binary search tree with the additional property that recently accessed elements are quick to access again. It differs from other BSTs by performing a splaying operation, moving an accessed node closer to the root, which optimizes for frequent access patterns.
   - Reference: Page 201

2. Explain the key properties of a Splay Tree.
   - Answer: Key properties of Splay Trees include self-balancing through splaying operations, keeping recently accessed elements near the root, and maintaining efficient O(log n) average time complexity for basic operations like search, insert, and delete.
   - Reference: Page 205

3. Describe the process of splaying in a Splay Tree.
   - Answer: Splaying involves performing rotations on a node every time it is accessed. Depending on the node's position relative to its parent and grandparent, a series of zig, zig-zig, or zig-zag operations are performed to bring the node to the root, optimizing the tree for future accesses.
   - Reference: Page 206

4. How do Splay Trees maintain their balance?
   - Answer: Splay Trees maintain their balance by moving frequently accessed nodes closer to the root. This continuous adjustment ensures that the trees remain approximately balanced, maintaining efficient access times for their operations.
   - Reference: Page 201

5. Discuss the benefits of using Splay Trees over other self-balancing trees.
   - Answer: Splay Trees are simpler to implement than other self-balancing trees and provide better performance for access patterns with frequent re-accesses of the same elements. They also do not require storing extra information like balance factors or colors.
   - Reference: Page 205

6. Explain the role of recently accessed elements in Splay Trees.
   - Answer: In Splay Trees, recently accessed elements are moved closer to the root, making subsequent accesses to these elements faster. This adaptive property makes Splay Trees particularly efficient for workloads with locality of reference.
   - Reference: Page 206

7. What is the impact of splaying on tree operations like search and insert?
   - Answer: Splaying during search and insert operations ensures that the elements being accessed or inserted are moved to the root of the tree, thereby reducing the height of frequently accessed paths and improving the efficiency of subsequent operations.
   - Reference: Page 201

8. How does the Splay Tree handle duplicate keys?
   - Answer: Handling of duplicate keys in Splay Trees is typically implemented by always inserting duplicates in a specific direction (either always left or always right) and adjusting the splay operation accordingly to accommodate duplicates.
   - Reference: Page 206

**Mock Test on Splay Trees and Tree Rotations - Part 2**
Short Answer Questions:
1. Discuss the advantages of splay operations in terms of tree balancing.
   - Answer: Splay operations help maintain an approximate balance by moving frequently accessed nodes closer to the root, thereby ensuring shorter access paths for those nodes in future operations.
   - Reference: Page 201

2. Explain the impact of splay tree rotations on search efficiency.
   - Answer: Splay tree rotations improve search efficiency by moving the accessed nodes towards the root, which reduces the path length for future searches of the same node.
   - Reference: Page 206

3. How does a splay tree adapt to different access patterns?
   - Answer: Splay trees adapt to different access patterns by reorganizing themselves with each access, moving the recently accessed nodes closer to the root. This adaptive property makes them ideal for scenarios with non-uniform access patterns.
   - Reference: Page 205

4. What are the key differences between splay trees and AVL trees?
   - Answer: Splay trees self-balance by moving accessed nodes to the root, whereas AVL trees maintain strict balance by keeping track of the height of subtrees and performing rotations to maintain a balance factor.
   - Reference: Page 201

5. Describe the process of accessing a node in a splay tree and its effects.
   - Answer: Accessing a node in a splay tree involves performing rotations to bring the node to the root, thereby reducing its access time in subsequent operations and optimizing the tree's structure based on usage patterns.
   - Reference: Page 206

6. How do splay trees ensure that frequently accessed elements are quickly reachable?
   - Answer: Splay trees ensure fast access to frequently accessed elements by splaying, or moving these elements towards the root with each access, thus reducing their access path length in future operations.
   - Reference: Page 205

7. Discuss the time complexity of operations in a splay tree.
   - Answer: The time complexity of operations in a splay tree is O(log n) on average, but individual operations might take longer. The efficiency is due to the splay operation, which optimizes the tree structure over time based on access patterns.
   - Reference: Page 206

8. Explain the concept of 'amortized analysis' in the context of splay trees.
   - Answer: Amortized analysis in splay trees refers to averaging the time complexity of operations over a sequence of operations. While individual operations might be expensive, the average cost per operation remains efficient due to the tree's self-adjusting nature.
   - Reference: Page 206