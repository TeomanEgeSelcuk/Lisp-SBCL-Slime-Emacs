;; (load "/src/exam/trees/tree.lisp")

;; Define a new data structure called 'tree-node'
(defstruct tree-node
  key       ;; This field stores the value or identifier for the node.
  children) ;; This field stores a list of this node's children.

;; Define a function to create a new 'tree-node' instance
(defun create-tree-node (key children)
  (make-tree-node :key key :children children))

;; Define a function to add a child node to a given node
(defun add-child (node child)
  (push child (tree-node-children node)))

;; Define a function to print the tree structure
(defun print-tree (node indent)
  ;; Print the current node with indentation
  ;; ; "~%~V@T~A" is a format directive. "~%" prints a newline. 
  ;; "~V@T" prints spaces for indentation. The number of spaces is given by (* 2 indent). 
  ;; "~A" prints the value of (tree-node-key node).
  (format t "~%~V@T~A" (* 2 indent) (tree-node-key node)) 
  ;; Iterate over each child and recursively print them
  (dolist (child (tree-node-children node))
    (print-tree child (+ indent 1))))


;; Example usage
(let ((root (create-tree-node 'root nil)))

  ;; Creating first level children
  (let ((child1 (create-tree-node 'child1 nil))
        (child2 (create-tree-node 'child2 nil)))
    (add-child root child1)
    (add-child root child2)

    ;; Adding second level children to child1
    (let ((child1a (create-tree-node 'child1a nil))
          (child1b (create-tree-node 'child1b nil)))
      (add-child child1 child1a)
      (add-child child1 child1b))

    ;; Adding second level children to child2
    (let ((child2a (create-tree-node 'child2a nil))
          (child2b (create-tree-node 'child2b nil)))
      (add-child child2 child2a)
      (add-child child2 child2b)

      ;; Adding third level children to child2a
      (let ((child2a1 (create-tree-node 'child2a1 nil)))
        (add-child child2a child2a1))))

  ;; Print the entire tree
  (print-tree root 0))
