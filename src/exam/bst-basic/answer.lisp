;; Define the node structure for a BST
(defstruct (node (:conc-name nil))
    key  ; object stored in the node
    (lc nil)  ; left child of the node
    (rc nil)  ; right child of the node
)

;; Function to insert a node in the BST
(defun bst-insert (obj  ; The object or value that you are working with or manipulating in some way.
                   bst  ; Binary Search Tree (BST) - a type of data structure that organizes data in a hierarchical manner.
                   comp) ; Comparison function - used to determine the placement of 'obj' within the 'bst'. It's used to maintain the order of the binary search tree.
    "Inserts obj in bst destructively, i.e., modifying the tree"
    (if (null bst) 
            (make-node :key obj) ; if bst is null, create a new node
        (let ((root (key bst)) ; root is the key of the current node in the Binary Search Tree (bst)
            (left (lc bst)) ; left is the left child of the current node in the bst
            (right (rc bst))) ; right is the right child of the current node in the bst
            (cond ((funcall comp obj root) ; If the comparison function returns true when comparing the object and the root, execute the following code
                         (setf (lc bst) (bst-insert obj left comp))) ; if obj < root, insert in left subtree
                        ((funcall comp root obj) 
                         (setf (rc bst) (bst-insert obj right comp))) ; if obj > root, insert in right subtree
                        (t bst)) ; if obj == root, do nothing
            bst))) ; return the bst

;; Function to find a node in the BST
(defun bst-find (obj bst comp)
    (when bst ; Check if bst (Binary Search Tree) is not null
        (let ((key (key bst)) ; Get the key of the root node in the bst
            (cond 
                ((funcall comp obj key) ;  if obj < key
                    (bst-find obj (lc bst) comp)) ;search in left subtree
                ((funcall comp key obj) ; if obj > key
                    (bst-find obj (rc bst) comp)) ; search in right subtree
                    (t bst)))))) ; if obj == key, return the node

(defun bst-remove (obj bst comp)
        (when bst ; when bst is not null
                (let ((key (key bst)) ; get the key of the root node
                            (l (lc bst)) ; get the left child of the root node
                            (r (rc bst))) ; get the right child of the root node
                        (cond ((funcall comp obj key) ; if obj < key
                                     (make-node :key key :lc (bst-remove obj l comp) :rc r)) ; remove obj from the left subtree
                                    ((funcall comp key obj) ; if obj > key
                                     (make-node :key key :lc l :rc (bst-remove obj r comp))) ; remove obj from the right subtree
                                    (t ; if obj == key
                                     (bst-join l r comp)))))) ; join the left and right subtrees

(defun bst-join (l r comp)
    "Joins 2 binary search trees.
    Precondition: (bst-max l) <= (bst-min r)"
    (cond ((null l) r) ; no left subtree
                ((null r) l) ; no right subtree
                ((zerop (random 2)) ;; random choice for new root
                (let ((root (key (bst-max l)))) ; Get the maximum key from the left subtree
                    (make-node :key root :lc (bst-remove root l comp) :rc r)) ; Create a new node with the maximum key, 
                                                                              ; remove the node with the maximum key from the left subtree, 
                                                                              ; and set the right child to the original right subtree

                    (t (let ((root (key (bst-min r)))) ; Get the minimum key from the right subtree
                        (make-node :key root :lc l :rc (bst-remove root r comp))))))) ; Create a new node with the minimum key, 
                                                                                      ; remove the node with the minimum key from the right subtree,
                                                                                      ; and set the left child to the original left subtree

;; Function to find the maximum node in a BST
(defun bst-max (bst)
    (and bst ; if bst is not null
        (or (bst-max (rc bst)) bst))) ; recursively find the maximum in the right subtree, if right subtree is null, return the current node

;; Function to find the minimum node in a BST
(defun bst-min (bst)
    (and bst ; if bst is not null
        (or (bst-min (lc bst)) bst))) ; recursively find the minimum in the left subtree, if left subtree is null, return the current node 

;; Example Usage
(defvar my-bst nil) ; Initialize an empty BST

;; Insert elements
(format t "Inserting 10, 5, 15, 8 into the BST:")
(setq my-bst (bst-insert 10 my-bst #'<))
(setq my-bst (bst-insert 5 my-bst #'<))
(setq my-bst (bst-insert 15 my-bst #'<))
(setq my-bst (bst-insert 8 my-bst #'<))

;; Find an element
(format t "~%~%Finding 8 in the BST:")
(let ((found-node (bst-find 8 my-bst #'<)))
  (if found-node
      (format t "Found node with key: ~A" (key found-node))
      (format t "Key not found")))

;; Remove an element
(format t "~%~%Removing 5 from the BST:")
(setq my-bst (bst-remove 5 my-bst #'<))

;; Find minimum and maximum
(format t "~%~%Finding minimum in the BST:")
(format t "Minimum key: ~A" (key (bst-min my-bst)))

(format t "~%~%Finding maximum in the BST:")
(format t "Maximum key: ~A" (key (bst-max my-bst)))

;; (load "/src/exam/bst-basic/bst.lisp")
