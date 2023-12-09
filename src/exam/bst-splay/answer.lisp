(defun tree-rotate (node parent grandparent)
    (cond                           ; update structure of parent node
        ((eql node (lc parent)) (setf (lc parent) (rc node) ; If node is the left child of parent, swap their positions
                      (rc node) parent))
        ((eql node (rc parent)) (setf (rc parent) (lc node) ; If node is the right child of parent, swap their positions
                      (lc node) parent))
        (t (error "NODE (~A) is not the child of PARENT (~A)”   node parent)))

    (cond                          ; update structure of grandparent node
          ((null grandparent) node) ; If grandparent is null, set node as the root
          ((eql parent (lc grandparent))     (setf (lc grandparent) node)) ; If parent is the left child of grandparent, set node as the new left child
          ((eql parent (rc grandparent))     (setf (rc grandparent) node)) ; If parent is the right child of grandparent, set node as the new right child
          (t (error "PARENT (~A) is not the child of GRANDPARENT (~A)" ; If none of the above conditions are met, raise an error
              parent grandparent))))

(defun node-chain (item root &optional chain)
  "Returns the node containing ITEM (if found) and the chain of nodes leading to it."
  (if root
      (let ((key (key root))
            (lc (lc root))
            (rc (rc root))
            (chain (cons root chain)))
        (cond ((< item key) (node-chain item lc chain)) ; If item is less than the key, traverse left
              ((> item key) (node-chain item rc chain)) ; If item is greater than the key, traverse right
              (t (values root chain)))) ; If item is equal to the key, return the node and chain
      (values nil chain))) ; If root is nil, return nil node and chain


(defun st-search (item root)
    (multiple-value-bind (node chain) (node-chain item root) ; Get the node and chain of nodes leading to it
        (when node (splay (car chain) (cdr chain))))) ; Splay the node if found

(defun splay (node chain)
    (loop :for (parent grandparent) :on chain :do ; Iterate over the chain of nodes
        (tree-rotate node parent grandparent)) ; Rotate the nodes in the chain
    node) ; Return the splayed node


