
# Linear Probing

### Question 1: Initial Insertions
Given a hash table of size 10, and a hash function `hash(key) = key % 10`, insert the following keys in the order given: 12, 22, 32, 42. After each insertion, what does the hash table look like?

### Question 2: Handling Collisions
Continue with the hash table from Question 1. Insert the keys 2, 52, 62. Describe how each collision is resolved and the state of the hash table after each insertion.

### Question 3: Searching for Keys
Given the final hash table from Question 2, describe the process of searching for the following keys: 22, 52, 72. Specify if each key is found or not and the slots checked during the search.

### Question 4: Deletion and Reinsertion
Assuming the hash table from Question 3, remove the key 32 and then attempt to insert the key 72. Describe how the removal is handled and the process of inserting 72, detailing how linear probing is applied.

### Question 5: Full Table Scenario
Consider a new hash table of size 5 with the hash function `hash(key) = key % 5`. Insert the keys 5, 10, 15, 20, 25. What happens when you try to insert the key 30? Explain the process and the state of the hash table.

### Question 6: Complex Collision Resolution
Using the hash table from Question 5, attempt to insert the keys 1, 2, 3. Explain in detail how each insertion is handled, considering the collisions and the linear probing process. What is the final state of the hash table?

# Quadratic Probing

### Question 1: Basic Insertions
Given a hash table of size 10, and a hash function `hash(key) = key % 10`, insert the following keys using quadratic probing: 5, 15, 25, 35. Describe the state of the hash table after each insertion.

### Question 2: Collision Handling
Continue with the hash table from Question 1. Insert the keys 45, 55 using quadratic probing. Explain how each collision is resolved and the state of the hash table after each insertion.

### Question 3: Searching for Keys
Given the final hash table from Question 2, describe the process of searching for the following keys using quadratic probing: 15, 55, 75. State whether each key is found and the slots checked during the search.

### Question 4: Deletion and Reinsertion
Assuming the hash table from Question 3, remove the key 25 and then attempt to insert the key 65 using quadratic probing. Explain the process of deletion and insertion, including the resolution of any collisions.

### Question 5: Full Table Scenario
Consider a new hash table of size 5 with the hash function `hash(key) = key % 5`. Insert the keys 0, 1, 2, 3, 4 using quadratic probing. What happens when you try to insert the key 5? Explain the process and the state of the hash table.

### Question 6: Complex Collision Resolution
Using the hash table from Question 5, attempt to insert the keys 6, 7, 8 using quadratic probing. Explain in detail how each insertion is handled, considering the collisions and the quadratic probing process. What is the final state of the hash table?

# Linear Probing Answers

### Answer to Question 1: Initial Insertions

**Hash Function**: `hash(key) = key % 10`

**Insertions**: 12, 22, 32, 42

- **Insert 12**: `hash(12) = 2`. Place 12 in index 2.
- **Insert 22**: `hash(22) = 2`. Index 2 is occupied, move to index 3. Place 22 in index 3.
- **Insert 32**: `hash(32) = 2`. Indexes 2 and 3 are occupied, move to index 4. Place 32 in index 4.
- **Insert 42**: `hash(42) = 2`. Indexes 2, 3, and 4 are occupied, move to index 5. Place 42 in index 5.

**Final State of Hash Table**:
```
Index: 0 1 2  3  4  5  6 7 8 9
Value:   - - 12 22 32 42 - - - -
```

### Answer to Question 2: Handling Collisions

**Continuing from Question 1's Final State**

- **Insert 2**: `hash(2) = 2`. Indexes 2, 3, 4, and 5 are occupied, move to index 6. Place 2 in index 6.
- **Insert 52**: `hash(52) = 2`. All slots from 2 to 6 are occupied, move to index 7. Place 52 in index 7.
- **Insert 62**: `hash(62) = 2`. All slots from 2 to 7 are occupied, move to index 8. Place 62 in index 8.

**Final State of Hash Table**:
```
Index: 0 1 2  3  4  5  6 7  8  9
Value:   - - 12 22 32 42 2 52 62 -
```

### Answer to Question 3: Searching for Keys

**Searching in Final State from Question 2**

- **Search 22**: `hash(22) = 2`. Check index 2, then 3. Found at index 3.
- **Search 52**: `hash(52) = 2`. Check indexes 2, 3, 4, 5, 6, 7. Found at index 7.
- **Search 72**: `hash(72) = 2`. Check indexes 2, 3, 4, 5, 6, 7, 8, 9, 0, 1. Not found.

### Answer to Question 4: Deletion and Reinsertion

**Starting with Final State from Question 3**

- **Remove 32**: Set the value at index 4 (where 32 is stored) to null or a special marker indicating a deleted item.
- **Insert 72**: `hash(72) = 2`. Indexes 2, 3, and 5 are occupied, index 4 is marked deleted, move to index 6. Indexes 6, 7, and 8 are occupied, place 72 in index 9.

**Final State of Hash Table**:
```
Index: 0 1 2  3  4  5  6 7  8  9
Value: - - 12 22 - 42 2 52 62 72
```

### Answer to Question 5: Full Table Scenario

**Hash Function**: `hash(key) = key % 5`

**Insertions**: 5, 10, 15, 20, 25

- **Insert 5, 10, 15, 20, 25**: All placed without collisions as they hash to different indices.

**Attempting to Insert 30**: 
- `hash(30) = 0`. All indices from 0 to 4 are occupied. The table is full and cannot accommodate the new key.

**Final State of Hash Table**:
```
Index: 0  1  2  3  4
Value: 5 10 15 20 25
```

### Answer to Question 6: Complex Collision Resolution

**Starting with Final State from Question 5**

- **Insert 1**: `hash(1) = 1`. Index 1 is occupied, move to index 2. Index 2 is also occupied, continue to 3, then 4, and finally place 1 in index

 0 (as it's circular).
- **Insert 2**: `hash(2) = 2`. Similar to above, after checking all indices, place 2 in index 1.
- **Insert 3**: `hash(3) = 3`. Again, following the same pattern, place 3 in index 2.

**Final State of Hash Table**:
```
Index: 0  1  2  3  4
Value: 1  2  3 20 25
```

# Quadratic Probing Answers 


### Answer to Question 1: Basic Insertions

**Hash Function**: `hash(key) = key % 10`

**Quadratic Probing Formula**: `new_index = (hash(key) + i^2) % table_size`, where `i` is the attempt number (starting from 0).

**Insertions**: 5, 15, 25, 35

- **Insert 5**: `hash(5) = 5`. Place 5 in index 5.
- **Insert 15**: `hash(15) = 5`. Collision at index 5. Try `i = 1`, so `new_index = (5 + 1^2) % 10 = 6`. Place 15 in index 6.
- **Insert 25**: `hash(25) = 5`. Collisions at indices 5 and 6. Try `i = 1`, then `i = 2`, so `new_index = (5 + 2^2) % 10 = 9`. Place 25 in index 9.
- **Insert 35**: `hash(35) = 5`. Collisions at 5, 6, and 9. Try `i = 1`, `i = 2`, then `i = 3`, so `new_index = (5 + 3^2) % 10 = 4`. Place 35 in index 4.

**Final State of Hash Table**:
```
Index: 0 1 2 3 4  5  6  7 8 9
Value:   - - - - 35 5 15 - - 25
```

### Answer to Question 2: Collision Handling

**Continuing from Question 1's Final State**

- **Insert 45**: `hash(45) = 5`. Collisions at 5, 6, 9, 4. Try `i = 1`, `i = 2`, `i = 3`, then `i = 4`, so `new_index = (5 + 4^2) % 10 = 1`. Place 45 in index 1.
- **Insert 55**: `hash(55) = 5`. Collisions at 5, 6, 9, 4, 1. Try `i = 1`, `i = 2`, `i = 3`, `i = 4`, then `i = 5`, so `new_index = (5 + 5^2) % 10 = 0`. Place 55 in index 0.

**Final State of Hash Table**:
```
Index: 0  1  2 3 4  5  6  7 8 9
Value: 55 45 - - 35 5 15 - - 25
```

### Answer to Question 3: Searching for Keys

**Searching in Final State from Question 2**

- **Search 15**: `hash(15) = 5`. Check index 5, found at index 5.
- **Search 55**: `hash(55) = 5`. Check index 5, then follow quadratic probing: 6, 9, 4, 1, and find 55 at index 0.
- **Search 75**: `hash(75) = 5`. Follow quadratic probing sequence: 5, 6, 9, 4, 1, 0, 3, 2, 7, 8. Not found.

### Answer to Question 4: Deletion and Reinsertion

**Starting with Final State from Question 3**

- **Remove 25**: Set the value at index 9 (where 25 is stored) to null or a special marker indicating a deleted item.
- **Insert 65**: `hash(65) = 5`. Collisions at 5, 6, 4, 1, 0. Try `i = 1`, `i = 2`, `i = 3`, `i = 4`, then `i = 5`, so `new_index = (5 + 5^2) % 10 = 3`. Place 65 in index 3.

**Final State of Hash Table**:
```
Index: 0  1  2 3  4  5  6  7 8 9
Value: 55 45 - 65 35 5 15 - - -
```

### Answer to Question 5: Full Table Scenario

**Hash Function**: `hash(key) = key % 5`

**Quadratic Probing Formula**: `new_index = (hash

(key) + i^2) % 5`.

**Insertions**: 0, 1, 2, 3, 4

- All keys 0 to 4 are placed in their respective indices without collision.

**Attempting to Insert 5**: 
- `hash(5) = 0`. Collisions at 0, 1, 4, 4 (again), and 1 (again). The table is full and unable to accommodate the new key.

**Final State of Hash Table**:
```
Index: 0 1 2 3 4
Value: 0 1 2 3 4
```

### Answer to Question 6: Complex Collision Resolution

**Starting with Final State from Question 5**

- **Insert 6**: `hash(6) = 1`. Collisions at 1, 2, 0, 4, 3. Unable to insert.
- **Insert 7**: `hash(7) = 2`. Collisions at 2, 4, 1, 1 (again), and 4 (again). Unable to insert.
- **Insert 8**: `hash(8) = 3`. Collisions at 3, 0, 2, 1, 1 (again). Unable to insert.

**Final State of Hash Table**:
```
Index: 0 1 2 3 4
Value: 0 1 2 3 4
```
