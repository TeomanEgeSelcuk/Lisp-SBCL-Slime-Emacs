## Questions

**Mock Test: Hash-Tables - Linear Probing**
* Short Answer Questions
1. (Page 145) Describe the primary concept of open addressing with linear probing in hash tables.
2. (Page 146) Explain the issue of clustering in linear probing and how it affects performance.
3. (Page 148) What are the key differences between linear probing and other collision resolution techniques?
4. (Page 149) How does the choice of hash function impact the efficiency of linear probing?
5. (Page 150) Discuss the advantages of linear probing over quadratic probing.
6. (Page 151) Describe a scenario where linear probing might lead to poor performance.
7. (Page 157) Explain how linear probing can be implemented in Lisp code.

**Additional Mock Test: Hash-Tables - Linear Probing**
* Short Answer Questions
1. (Page 145) How does linear probing handle the deletion of elements in a hash table?
2. (Page 146) What are the potential downsides of using linear probing in a hash table with a high load factor?
3. (Page 148) Compare the cache efficiency of linear probing with chaining in hash tables.
4. (Page 149) How does the size of the hash table affect the performance of linear probing?
5. (Page 150) Explain how linear probing can lead to primary clustering.
6. (Page 151) Discuss the trade-offs between linear probing and double hashing for collision resolution.
7. (Page 157) How does linear probing differ from quadratic probing in handling collisions.

## Answers 

**Mock Test: Hash-Tables - Linear Probing**
* Short Answer Questions
1. (Page 145) Open addressing with linear probing involves addressing collisions by sequentially probing the hash table until an empty slot is found. This method reduces the need for additional data structures, but can lead to clustering.
2. (Page 146) Clustering in linear probing refers to the tendency for contiguous groups of occupied slots to form, leading to longer search times. It can be mitigated by rehashing or using quadratic probing.
3. (Page 148) Linear probing differs from other techniques like chaining by storing all keys within the hash table array itself. It simplifies the data structure but can lead to increased clustering and longer search times.
4. (Page 149) The efficiency of linear probing is significantly influenced by the choice of the hash function. A good hash function reduces clustering and evenly distributes keys.
5. (Page 150) Linear probing has a simpler implementation and better cache performance compared to quadratic probing. However, it's more prone to clustering.
6. (Page 151) Linear probing can perform poorly in scenarios where the hash table is nearly full or when there are long sequences of occupied slots, leading to longer probing sequences.
7. (Page 157) In Lisp, linear probing can be implemented using functions that iterate over the hash table's array, updating the index based on a hash function until an empty slot or the desired key is found.

**Additional Mock Test: Hash-Tables - Linear Probing**
* Short Answer Questions
1. (Page 145) Linear probing handles the deletion of elements by marking the slot as deleted (often called a 'tombstone') and continuing probing during subsequent insertions or searches.
2. (Page 146) High load factors lead to increased collisions, longer search times, and reduced efficiency in linear probing.
3. (Page 148) Linear probing tends to have better cache efficiency than chaining due to data locality, as elements are stored contiguously in memory.
4. (Page 149) A larger hash table size generally improves the performance of linear probing by reducing the likelihood of collisions.
5. (Page 150) Linear probing leads to primary clustering as continuous blocks of filled slots tend to form, causing longer search times for subsequent insertions.
6. (Page 151) Linear probing is simpler and has better cache performance, but double hashing reduces clustering and provides a more uniform distribution.
7. (Page 157) Linear probing uses a fixed step size (usually 1) for probing, whereas quadratic probing uses a variable step size, potentially reducing clustering.


