# The von Neumann Bottleneck
This term refers to a limitation in a computer architecture where the speed of a computer is restricted by the rate at which data can travel between the computer's processor (CPU) and its memory.

## Examples and Explanations

### 1. CPU only has access to data stored in registers:
   - **Simple Explanation:** Think of the CPU as a chef in a kitchen, and the registers as the countertop where they prep ingredients. The chef can only work with what’s on the countertop.
   - **Example:** When making a sandwich, the chef can only use the ingredients that are currently on the countertop.

### 2. Types of Data in Registers:
   - **Numbers (integers, floats, and characters)**
       - **Simple Explanation:** Different ingredients like bread, lettuce, and tomatoes.
       - **Example:** The chef has slices of bread, lettuce leaves, and tomato slices on the countertop.
   - **Pointers (memory addresses)**
       - **Simple Explanation:** The pointers are like labels telling where each type of ingredient is stored in the pantry.
       - **Example:** There’s a label on the shelf indicating where to find more bread, lettuce, or tomatoes.

### 3. Bulk of the data stored in memory:
   - **Simple Explanation:** The pantry where all ingredients are stored. The chef must go to the pantry to get anything that’s not on the countertop.
   - **Example:** If the chef needs cheese, they have to go to the pantry because it’s not on the countertop.

### 4. Two Ways to Store Data in Memory:
   - **Contiguous Structure**
       - **Simple Explanation:** Ingredients are stored side by side on the same shelf.
       - **Example:** All types of bread are placed together on one shelf, making it easy to grab.
   - **Linked Structure**
       - **Simple Explanation:** Ingredients are scattered in different places and a note tells where to find each item.
       - **Example:** The white bread is on one shelf, but there’s a note pointing to another shelf where the whole wheat bread is located.

### 5. Comparison:
   - **Contiguous Structure (like arrays and structs)**
       - **More Efficient Data Access:** The chef can quickly grab what they need because everything is organized and close by.
       - **Takes Constant Time:** It always takes the same amount of time for the chef to grab an item because they know exactly where it is.
       - **Example:** The chef reaches directly to the bread shelf to get a specific type of bread.
   - **Linked Structure (like lists, trees, graphs)**
       - **Less Efficient Data Access:** The chef takes more time because they have to follow notes to find each item.
       - **Cost Depends on the Size:** The bigger the pantry (more items), the longer it takes to find things.
       - **Example:** To get to the whole wheat bread, the chef first finds the white bread, then reads the note, and then goes to the shelf where the whole wheat bread is kept.

In the context of computers:
- **Contiguous Structures:** Like an array where elements are stored side by side in memory, so it’s quick to access.
- **Linked Structures:** Like a linked list where each element points to the next, so it takes more time to traverse and access specific elements.

# Contiguous Structure: Tuple/Record/Object
A contiguous structure, like a tuple, is a collection of elements stored in a continuous memory location. Each element or "slot" in this structure can hold data.

### Example: Movie Information
In the example provided, a structure (or tuple) is created to store information about a movie. This structure has named slots to hold different pieces of data, such as the title, director, year, and type of movie.

### Using Lisp Programming Language
- **Creating a Structure:** The `defstruct` macro in Lisp is used to define a new structure. In this case, a "movie" structure is defined with slots for title, director, year, and type.
- **Using the Structure:** Once the structure is defined, Lisp automatically provides tools to create instances of the structure and access or modify the data within its slots.

### Working Example
1. **Defining a Movie Structure:** 
   ```lisp
   (defstruct movie title director year type)
   ```
   - **Simple Explanation:** A structure called "movie" is created to hold information about movies, with specific spots for the title, director, year, and type.

2. **Creating a Movie Instance:** 
   ```lisp
   (make-movie :title "Blade Runner" :director "Ridley Scott")
   ```
   - **Simple Explanation:** A specific movie, "Blade Runner" directed by "Ridley Scott", is being recorded using the previously defined structure. However, the year and type are not provided yet, so they are left empty (NIL).

3. **Accessing and Modifying Movie Data:**
   ```lisp
   (let ((movie (make-movie :title "Blade Runner" :director "Ridley Scott")))
     (setf (movie-year movie) 1982)
     (values (movie-title movie) (movie-director movie) (movie-year movie)))
   ```
   - **Simple Explanation:** The year "1982" is added to the movie’s information. Now, all the filled information about the movie, including the title, director, and year, can be accessed and displayed.

4. **Checking if a Movie is in Database:**
   ```lisp
   (defun in-db? (title)
     "Returns true if movie title is in the database; otherwise returns NIL"
     (dotimes (i *size*)
       (when (and (typep (aref *db* i) 'movie)
                  (equal (movie-title (aref *db* i)) title))
         (return (aref *db* i)))))
   ```
   - **Simple Explanation:** This is a function to check if a movie with a specific title is in the database. If it finds a movie with the given title, it returns that movie's information; otherwise, it returns NIL (meaning not found).

### Visual Explanation
Imagine a shelf (the structure) with different compartments (slots) named for each type of information (title, director, etc.). Each compartment can only contain the type of information it’s labeled for. You can quickly check each compartment to find out all about the movie since they are stored next to each other - that's the advantage of a contiguous structure!

# Array operations: algorithmic complexity

### 1. **Memory Space (O(1)):**
   - **Simple Explanation:** An array uses the least amount of memory needed to store its elements. There is a little extra memory used to keep track of the array's size.
   - **Example:** Think of an array like a shoe rack with specific spots for each pair of shoes. There's also a small tag on the side telling you how many pairs of shoes are on the rack.

### 2. **Access to i-th Element (O(1)):**
   - **Simple Explanation:** You can quickly and directly get to any pair of shoes on the rack without looking through every pair.
   - **Example:** If you want the 3rd pair of shoes, you go directly to the 3rd spot on the rack and pick them up.

### 3. **Search (O(log n)):**
   - **Simple Explanation:** If the shoes are sorted by size, you can quickly find a specific size by dividing the rack into sections and knowing which section to look in.
   - **Example:** Looking for size 8 shoes in a sorted rack, you can skip the smaller sizes and only look at the section with sizes close to 8.

### 4. **Insertion/Deletion (O(n)):**
   - **Simple Explanation:** Adding or removing a pair of shoes can take time because you might need to shift the other shoes to make room or close the gap.
   - **Example:** To add a new pair, you might need to move some pairs to the next spot to open up a space, or to remove a pair, you might shift others to fill the empty spot.

### 5. **Dynamic Arrays:**
   - **Simple Explanation:** Imagine a magical shoe rack that expands itself when it's full. So, you don’t need to know in advance how many pairs of shoes you'll have.
   - **Example:** You start with a small rack. As you add more shoes, the rack grows, creating more spots for the new pairs.

### Example Code in the Explanation:
This code snippet is creating a dynamic array (vector) in Lisp. This dynamic array automatically adjusts its size as you add more elements to it.

   ```lisp
   (let ((vec (make-array 0 :fill-pointer t :adjustable t)))
     (dotimes (i 10)
       (vector-push-extend i vec)
       (describe vec)))
   ```

   - **Simple Explanation:** It’s like having a shoe rack (vec) that starts with no spots. Each time you get a new pair of shoes (i from 0 to 9), the rack magically creates a spot for them and can even expand beyond its current size to accommodate more shoes.

### Expanded Vector Example:
As elements are added, the array grows, not just to fit the new element, but it expands a bit more to have room for additional elements.

   - **Example:** You have a magical shoe rack with 6 spots filled (from 0 to 5). Even though only 6 spots are filled, the rack expands to have a total of 8 spots ready for more shoes.

### Key Points:
   - **O(1) Memory Space:** Arrays are memory-efficient.
   - **O(1) Access Time:** Quick direct access to any element.
   - **O(log n) Search:** Fast search if the array is sorted.
   - **O(n) Insertion/Deletion:** Can be slow because elements might need to be shifted.
   - **Dynamic Arrays:** Automatically expand in size to accommodate more elements, helpful when you’re unsure how many elements you’ll have in total.