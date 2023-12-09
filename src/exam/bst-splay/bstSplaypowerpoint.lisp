(defun tree-rotate (node parent grandparent)

    (cond                              ; update structure of parent node
        ((eql node (lc parent)) (setf (lc parent) (rc node) ; EQL compares pointers
                                  (rc node) parent))
        ((eql node (rc parent)) (setf (rc parent) (lc node)
                                  (lc node) parent))
        (t (error "NODE (~A) is not the child of PARENT (~A)”   node parent)))

    (cond                          ; update structure of grandparent node
        ((null grandparent) node)
        ((eql parent (lc grandparent))     (setf (lc grandparent) node))
        ((eql parent (rc grandparent))     (setf (rc grandparent) node))
        (t (error "PARENT (~A) is not the child of GRANDPARENT (~A)"
              parent grandparent))))



(defun node-chain (item root &optional chain)
  "Returns the node containing ITEM (if found) and the chain of nodes leading to it."
  (if root
      (let ((key (key root))
            (lc (lc root))
            (rc (rc root))
            (chain (cons root chain)))
        (cond ((< item key) (node-chain item lc chain))
              ((> item key) (node-chain item rc chain))
              (t (values root chain))))
      (values nil chain)))


(defun st-search (item root)
    (multiple-value-bind (node chain) (node-chain item root)
        (when node (splay (car chain) (cdr chain)))))

(defun splay (node chain)
    (loop :for (parent grandparent) :on chain :do
        (tree-rotate node parent grandparent))
    node)

