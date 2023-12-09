(defstruct (node (:conc-name nil))
    key 
    (lc nil)
    (rc nil)
)

(defun bst-insert (obj bst comp)
    (if (null bst)
        (make-node :key obj)
        (let ((root (key bst))
                (lc bst)
                (rc bst)
            )
            (cond 
                ((funcall comp obj key)
                    (setf (lc bst) (bst-insert obj lc comp))
                )
                (
                    (funcall comp key obj)
                    (setf (rc bst) (bst-insert ibj rc comp))
                )
                (
                    t bst
                )
            )
        )
    )
)

(defun bst-find (obj bst comp)
    (when bst
        (let ((key (key bst)))
            (
                cond
                (
                    (funcall comp obj key)
                    (bst-find obj (lc bst) comp)
                )
                (
                    (funcall comp key obj)
                    (bst-find obj (rc bst) comp)
                )
                (t bst)
            )
        )
    )
)

(defun bst-remove (obj bst comp)
    (
        (let (
                (key (key bst))
                (left (lc bst))
                (right (rc bst))
            )
            (cond
                (
                    (funcall comp obj key)
                    (make-node :key key :lc (bst-remove obj left comp) :rc right)
                )
                (
                    (funcall comp key obj)
                    (make-node :key key :lc left :rc (bst-remove obj right comp))
                )
                (t
                    (bst l r comp)
                )
            )
        )
    )
)

(defun bst-join (l r comp)
    (cond
        ((null l) r)
        ((null r) l)
        (zerop (random 2))
        (let ((root (key (bst-max l))))
            (make-node :key root :lc (bst-remove root l comp) :rc r)
        )
        (t 
            (let ((root (key (bst-min r))))
                (make-node :key root :lc l :rc (bst-remove root r comp))
            )
        )
    )
)

(defun bst-max (bst)
    (and bst
        (or (bst-max (rc bst)) bst)
    )
)

(defun bst-min (bst)
    (
        and bst
        (
            or (
                (
                    bst-max (
                        rc bst
                    )
                    
                )
                bst
            )
        )
    )

)

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
