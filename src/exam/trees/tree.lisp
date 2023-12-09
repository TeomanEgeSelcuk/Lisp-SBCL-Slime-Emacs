;; (load "/src/exam/trees/tree.lisp")

;; Define the tree structure
(defstruct tree-node
  key     ; The 'key' is the value or data that the node holds
  children ; 'children' is a list of all child nodes of this node
)

;; Function to create a new tree node
(defun create-tree-node (key children)
  (make-tree-node :key key :children children)
)

;; Function to add a child to a node
(defun add-child (node child)
  (push child (tree-node-children node))
)

;; Function to print a tree node
(defun print-tree (node indent)
  (format t "~%~V@T~A" (* indent 2) (tree-node-key node))
  (dolist (child (tree-node-children node))
  (print-tree child (+ indent 1)))
)

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
