## Short Answer Questions 

* (Page 145): Explain the primary motivation behind using quadratic probing in hash tables.
* (Page 149): Describe how quadratic probing addresses the issue of clustering compared to linear probing.
* (Page 150): What is the initial probing formula for quadratic probing, and how does it differ from linear probing?
* (Page 151): Explain how the insertion process works in quadratic probing with the example of inserting 682 into a hash table.
* (Page 149): Discuss the advantages and disadvantages of using quadratic probing in hash tables.
* (Page 150): How does the quadratic probing formula prevent primary clustering?
* (Page 157): Describe how quadratic probing can be implemented in the Lisp programming language.
* (Page 158): In the context of quadratic probing, explain the significance of the hash table size selection.
* (Page 158): How does quadratic probing compare to other collision resolution techniques in terms of performance and complexity?

## Additional Short Answer Questions 

* (Page 145): Discuss the effect of table size on the efficiency of quadratic probing in hash tables.
* (Page 149): How does quadratic probing mitigate the problem of secondary clustering?
* (Page 150): Explain the concept of 'wrap-around' in quadratic probing.
* (Page 151): Compare and contrast quadratic probing with double hashing.
* (Page 157): Describe how the load factor of a hash table impacts the performance of quadratic probing.
* (Page 158): Explain why quadratic probing is not used with hash tables that allow deletion of elements.
* (Page 145): How does the choice of the auxiliary hash function, ℎ′(k)affect the performance of quadratic probing?
* (Page 149): Discuss the role of the quadratic term in the probing formula of quadratic probing.

## Short Answer Questions Answers 

* (Page 145): The primary motivation behind using quadratic probing in hash tables is to address the issue of clustering, which is a common problem in linear probing. Quadratic probing reduces the likelihood of clustering by using a quadratic function to calculate probe sequences, thereby distributing keys more evenly.
* (Page 149): Quadratic probing addresses the issue of clustering by using a quadratic function for the probe sequence, which spreads out the keys more evenly compared to the linear approach of linear probing.
* (Page 150): The initial probing formula for quadratic probing is h(k,i) = (h’(k) + i^2) typically mod M where h’k is an auxiliary hash function, i is the probe number, and M is the size of the hash table. This formula differs from linear probing, which uses a linear function for the probe sequence is the size of the hash table. This formula differs from linear probing, which uses a linear function for the probe sequence.
* (Page 151): In quadratic probing, the insertion process involves finding the next available slot using the quadratic probe sequence. For example, when inserting 682, the probing sequence might start at index h'(682), and subsequent probes are calculated using the quadratic formula until an empty slot is found.
* (Page 149): Advantages of using quadratic probing include reduced clustering and better distribution of keys. Disadvantages include the possibility of secondary clustering and the need for more complex calculations for probe sequences.
* (Page 150): The quadratic probing formula prevents primary clustering by using a quadratic function to calculate probe sequences, thereby ensuring that consecutive insertions do not cluster around the same area.
* (Page 157): Quadratic probing can be implemented in Lisp by modifying the hash function in the hash table implementation. The function would use a quadratic formula to calculate the next probe position, instead of a linear one.
* (Page 158): The hash table size is significant in quadratic probing as it affects the efficiency of the probing process. A prime number close to the desired table size is often chosen to reduce the likelihood of patterns that can lead to clustering.
* (Page 158): Quadratic probing can offer better performance compared to linear probing due to reduced clustering. However, it can be more complex and might still suffer from secondary clustering. It generally performs better than chaining for tables that are not too full.


## Additional Short Answer Questions Answers

* (Page 145): The table size significantly affects the efficiency of quadratic probing. Larger table sizes can reduce the likelihood of collisions and improve the overall performance of the hash table.
* (Page 149): Quadratic probing reduces secondary clustering by using a quadratic function to compute probe sequences, which helps in evenly distributing keys across the hash table.
* (Page 150): 'Wrap-around' in quadratic probing refers to the process of returning to the beginning of the hash table when the end is reached during the probing sequence.
* (Page 151): Quadratic probing differs from double hashing in that the former uses a quadratic function to calculate the probe sequence, while the latter uses a second hash function to determine the probe sequence.
* (Page 157): The load factor, defined as the number of entries divided by the number of slots in the hash table, impacts the performance of quadratic probing. A higher load factor increases the likelihood of collisions and reduces the efficiency of the probing process.
* (Page 158): Quadratic probing is not ideal for hash tables that allow deletion of elements because it can create gaps in the probe sequence, leading to inefficiencies in searching for elements.
* (Page 145): The choice of the auxiliary hash function in quadratic probing affects the initial slot choice and the subsequent probing sequence. A poor choice can lead to increased collisions and reduced performance.
* (Page 149): The quadratic term in the probing formula ensures that each subsequent probe is further away from the previous one, reducing the chance of clustering and improving the distribution of keys.

