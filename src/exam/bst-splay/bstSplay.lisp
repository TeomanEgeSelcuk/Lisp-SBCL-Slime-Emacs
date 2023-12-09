;; Define the node structure
(defstruct (node (:conc-name nil) ; Automatic prefix for access functions
                 (:print-object   ; Defines how the structure is printed
                  (lambda (node out)
                    (format out "[~a-~@[~a~]-~@[~a~]]" ; Prints argument if non-NIL
                            (key node)
                            (lc node)
                            (rc node)))))
  key  ; For storing the key
  lc   ; Left child
  rc)  ; Right child

;; Tree rotation function
(defun tree-rotate (node parent grandparent)
  (cond
    ((eql node (lc parent))
     (setf (lc parent) (rc node)
           (rc node) parent))
    ((eql node (rc parent))
     (setf (rc parent) (lc node)
           (lc node) parent))
    (t (error "NODE (~A) is not the child of PARENT (~A)" node parent)))
  (cond
    ((null grandparent) node)
    ((eql parent (lc grandparent))
     (setf (lc grandparent) node))
    ((eql parent (rc grandparent))
     (setf (rc grandparent) node))
    (t (error "PARENT (~A) is not the child of GRANDPARENT (~A)" parent grandparent))))

;; Building a chain of nodes
(defun node-chain (item root &optional chain)
  (if root
      (let ((key (key root))
            (lc (lc root))
            (rc (rc root))
            (chain (cons root chain)))
        (cond
          ((< item key) (node-chain item lc chain))
          ((> item key) (node-chain item rc chain))
          (t (values root chain))))
    (values nil chain)))

;; SPLAY function
(defun splay (node chain)
  (loop :for (parent grandparent) :on chain :do
        (tree-rotate node parent grandparent))
  node)

;; ST-SEARCH function
(defun st-search (item root)
  (multiple-value-bind (node chain) (node-chain item root)
    (when node
      (splay (car chain) (cdr chain)))))

;; ST-INSERT function
(defun st-insert (obj root)
  (if (null root)
      (make-node :key obj)
    (multiple-value-bind (node chain) (node-chain obj root)
      (unless node
        (let ((new (make-node :key obj))
              (parent (car chain)))
          (if (< obj (key parent))
              (setf (lc parent) new)
              (setf (rc parent) new))
          (splay new chain))))))

;; Example Usage
(defvar *my-tree* nil)  ; Initialize an empty tree

;; Insert elements
(format t "Inserting elements into the tree:~%")
(setq *my-tree* (st-insert 10 *my-tree*))
(setq *my-tree* (st-insert 5 *my-tree*))
(setq *my-tree* (st-insert 15 *my-tree*))
(setq *my-tree* (st-insert 8 *my-tree*))

;; Perform a search (splay) operation
(format t "Performing search for the key 8:~%")
(st-search 8 *my-tree*)

;; Print the tree
(format t "Current tree structure: ~a~%" *my-tree*)
