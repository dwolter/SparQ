;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;


;;
;; RED/BLACK TREES
;;
;;
;; The code is based on the explanation found at (refer to it for naming and documentation)
;; "Introduction to Algorithms" by Cormen, Leiserson, Rivest, and Stein (MIT press), as well as
;; http://lcm.csa.iisc.ernet.in/dsa/node116.html 
;; Additional 'inspiration', esp. regarding the deletion has been taken from a MIT
;; implementation for a CL repository
;; http://home.comcast.net/~prunesquallor/random/rbtree.lsp
;; Note that either one of the original sources has proven wrong.


;; Change history (most recent first):
;; 2004-11-11 DW	added package definition to this file
;; 2004-10-13 DW	added r/b-tree-search to find objects when the key suffers numerical instability
;; 2004-08-18 DW	fixed that objects are also found when multiple keys are used
;; 2004-08-18 DW	bug fix in pop-min, pop-max, and r/b-tree->list
;; 2004-07-23 DW	added 2nd return value for insertion (newly inserted node)
;; 2004-07-22 DW	added push & pop for trees
;; 2004-07-21 DW	initial release

#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
(defpackage DATA-STRUCTURES
  (:use :common-lisp)
  (:nicknames :data)
  (:export 
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#   ;; Accessing the node objects and traversing the tree #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
   :r/b-node 
   :r/b-node-data 
   :r/b-node-parent 
   :r/b-node-data 
   :r/b-node-left-child 
   :r/b-node-right-child
   :r/b-node-successor 
   :r/b-node-predecessor
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#   ;; Basic tree operation #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
   :r/b-tree-insert 
   :r/b-tree-find 
   :r/b-tree-min-node 
   :r/b-tree-max-node 
   :r/b-tree-delete 
   :r/b-tree-delete-node
   :r/b-tree->list 
   :list->r/b-tree 
   :r/b-tree-push 
   :r/b-tree-pop-min 
   :r/b-tree-pop-max 
   :r/b-tree-search
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#   ;; Tree utilities #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
   :r/b-tree-depth 
   :r/b-tree-count))

(in-package :data)

#|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#;;
;; Struct to represent a tree's node
;;
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
(defstruct r/b-node
  data
  (color       :red) 
  (parent      nil) 
  (left-child  nil) 
  (right-child nil))

#|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#;; 
;; Retrieve the tree's minimum or maximum
;; 
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
(defun r/b-tree-min-node (tree)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Retrieves the tree's minimum node."
#|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#  (declare (type (or null r/b-node) tree)
           (optimize (safety 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (if tree
    (or (r/b-tree-min-node (r/b-node-left-child tree))
        tree)))

(defun r/b-tree-max-node (tree)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Retrieves the tree's maximum node." #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) tree)
           (optimize (safety 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (if tree
    (or (r/b-tree-max-node (r/b-node-right-child tree))
        tree)))

#|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#;; 
;; Traverse a tree
;; 
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#

(defun r/b-node-predecessor (node)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Returns the predecessor of a node" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type r/b-node node)
           (optimize (speed 0) (safety 3) (debug 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  ;(check-type node r/b-node)
  (let ((left (r/b-node-left-child node)))
    (if left
      (r/b-tree-max-node left)
      (let ((y (r/b-node-parent node))
            (x node))
        (declare (type (or null r/b-node) y)
                 (type r/b-node x))
        (loop while (and y (eq x (r/b-node-left-child y))) do
              (setq x y
                    y (r/b-node-parent y)))
        y))))


(defun r/b-node-successor (node)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Returns the successor of a node" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type r/b-node node)
           (optimize (speed 0) (safety 3) (debug 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  ;(check-type node r/b-node)
  (let ((right (r/b-node-right-child node)))
    #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) right))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
    (if right
      (r/b-tree-min-node right)
      (let ((y (r/b-node-parent node))
            (x node))
        #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) y)
                 (type r/b-node x))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
        (loop while (and y (eq x (r/b-node-right-child y))) do
              (setq x y
                    y (r/b-node-parent y)))
        y))))

(defun r/b-tree->list (tree)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Returns the flattened list of objects contained by tree" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (let (list
        (node (r/b-tree-max-node tree)))
    (loop while node do
          (push (r/b-node-data node) list)
          (setq node (r/b-node-predecessor node)))
    list))



#|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#;;
;; Searching
;; obj  : object to find
;; tree : tree in which to find
;; <    : tree's order
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
(defun r/b-tree-find (tree obj < &key (test #'eql))
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Finds obj in tree, returns the node when found, nil otherwise." #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) tree)
           (type function < test)
           (optimize (speed 0) (debug 3) (safety 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  ;(check-type tree (or null r/b-node))
  ;(check-type < function)
  ;(check-type test function)
  (labels ((find-with-same-key (node)
             #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) node)
                      (optimize (speed 0) (safety 3) (debug 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
             (and node
                  (let ((data (r/b-node-data node)))
                    (and (not (funcall < data obj))
                         (or (if (funcall test obj data)
                               tree)
                             (find-with-same-key (r/b-node-predecessor node)))))))

           (r/b-find (tree)
             #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (optimize (speed 3) (debug 0) (safety 0))
                      (type (or null r/b-node) tree))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
             (if (null tree)
               nil
               (let ((data (r/b-node-data tree)))
                 (if (funcall test obj data)
                   tree
                   (let ((branch-left? (funcall < obj data)))
                     (if (not (or (r/b-node-left-child tree)
                                  (r/b-node-right-child tree)))
                       (find-with-same-key (r/b-node-predecessor tree))
                       (if branch-left?
                         (r/b-find (r/b-node-left-child tree))
                         (r/b-find (r/b-node-right-child tree))))))))))
    (r/b-find tree)))

(defun r/b-tree-search (tree obj < &key (test #'eql))
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#"Finds obj in tree by searching the complete tree starting at a position
   as defined by the object's key order. Use this function when numeric
   instability of key values is an issue." #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#

  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) tree)
           (type function < test)
           (optimize (speed 0) (debug 3) (safety 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#  
  (labels ((find-start-node (node)
             #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type r/b-node node)
                      (optimize (speed 0) (debug 3) (safety 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
             (let ((data (r/b-node-data node)))
               (if (funcall test obj data)
                 (values node t)
                 (if (funcall < obj data)
                   (if (r/b-node-left-child node)
                     (find-start-node (r/b-node-left-child node))
                     node)
                   (if (r/b-node-right-child node)
                     (find-start-node (r/b-node-right-child node))
                     node))))))

    (if tree
      (multiple-value-bind (start-node found?) (find-start-node tree)
        (if found?
          start-node
          (let ((found? nil)
                (expand-right (r/b-node-successor start-node))
                (expand-left (r/b-node-predecessor start-node)))
            (loop while (and (not found?)
                             (or expand-left expand-right)) do
                  (when expand-right
                    (when (funcall test obj (r/b-node-data expand-right))
                      (setq found? expand-right))
                    (setq expand-right (r/b-node-successor expand-right)))

                  (when (and expand-left (not found?))
                    (when (funcall test obj (r/b-node-data expand-left))
                      (setq found? expand-left))
                    (setq expand-left (r/b-node-predecessor expand-left))))
            found?))))))

#|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#;;
;; Rotation (needed for re-balancing)
;;
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
(defun r/b-rotate-right (x)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Rotates a r/b-tree to the left, returning the new tree's root if that has been changed." #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type r/b-node x)
           (optimize (speed 0) (safety 3) (debug 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (let ((y (r/b-node-left-child x))
        new-root)
    (declare (type (or null r/b-node) y y new-root))
    (progn 
      (setf (r/b-node-left-child x) (r/b-node-right-child y))
      (when (r/b-node-right-child y)
        (setf (r/b-node-parent (r/b-node-right-child y)) x))
      (setf (r/b-node-parent y) (r/b-node-parent x)))
    (if (null (r/b-node-parent x))
      (setq new-root y)
      (if (eq x (r/b-node-right-child (r/b-node-parent x)))
        (setf (r/b-node-right-child (r/b-node-parent x)) y)
        (setf (r/b-node-left-child (r/b-node-parent x)) y)))
    (setf (r/b-node-right-child y) x
          (r/b-node-parent x) y)
    new-root))

(defun r/b-rotate-left (x)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Rotates a r/b-tree to the right, returning the new tree's root if that has been changed" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type r/b-node x)
           (optimize (speed 0) (safety 3) (debug 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (let ((y (r/b-node-right-child x))
        new-root)
    #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) y y new-root))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
    (progn
      (setf (r/b-node-right-child x) (r/b-node-left-child y))
      (when (r/b-node-left-child y)
        (setf (r/b-node-parent (r/b-node-left-child y)) x))
      (setf (r/b-node-parent y) (r/b-node-parent x)))
    (if (null (r/b-node-parent x))
      (setq new-root y)
      (if (eq x (r/b-node-left-child (r/b-node-parent x)))
        (setf (r/b-node-left-child (r/b-node-parent x)) y)
        (setf (r/b-node-right-child (r/b-node-parent x)) y)))
    (setf (r/b-node-left-child y) x
          (r/b-node-parent x) y)
    new-root))


(defun r/b-insert-fixup (tree node)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Rebalancing & Recoloring of a red/black-tree after insertion of node" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type r/b-node tree) (type (or null r/b-node) node)
           (optimize (speed 0) (debug 3) (safety 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (macrolet ((left (node)
               `(r/b-node-left-child ,node))
             (right (node)
               `(r/b-node-right-child ,node))
             (parent (node)
               `(r/b-node-parent ,node))
             (color (node)
               `(r/b-node-color ,node)))
    (let ((z node)
          (root tree))
      #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type r/b-node root)
               (type (or null r/b-node) z))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
      (loop while (and z 
                       (parent z)
                       (eq :red (color (parent z)))) do
            (if (eq (parent z) (left (parent (parent z))))
              (let ((y (right (parent (parent z)))))
                (declare (type (or null r/b-node) y))
                (if (and y (eq :red (color y)))
                  (setf (color (parent z)) :black
                        (color y) :black
                        (color (parent (parent z))) :red
                        z (parent (parent z)))
                  (progn
                    (when (eq z (right (parent z)))
                      (setf z (parent z))
                      (let ((new-root (r/b-rotate-left z)))
                        (declare (type (or null r/b-node) new-root))
                        ; keep track of a changing tree's root
                        (when new-root
                          (setq root new-root))))
                    (setf (color (parent z)) :black
                          (color (parent (parent z))) :red)
                    (let ((new-root (r/b-rotate-right (parent (parent z)))))
		      (declare (type (or null r/b-node) new-root))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#                      ; keep track of a changing tree's root
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#                      (when new-root
                        (setq root new-root))))))
              (let ((y (left (parent (parent z)))))
                (declare (type (or null r/b-node) y))
                (if (and y (eq :red (color y)))
                  (setf (color (parent z)) :black
                        (color y) :black
                        (color (parent (parent z))) :red
                        z (parent (parent z)))
                  (progn
                    (when (eq z (left (parent z)))
                      (setf z (parent z))
                      (let ((new-root (r/b-rotate-right z)))
                        (declare (type (or null r/b-node) new-root))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#                        ; keep track of a changing tree's root
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#                        (when new-root
                          (setq root new-root))))
                    (setf (color (parent z)) :black
                          (color (parent (parent z))) :red)
                    (let ((new-root (r/b-rotate-left (parent (parent z)))))
                      (declare (type (or null r/b-node) new-root))
                      ; keep track of a changing tree's root
                      (when new-root
                        (setq root new-root))))))))
      (setf (r/b-node-color root) :black)
      root)))


#|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#;; 
;; Insertion
;;
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
(defun r/b-tree-insert (tree obj <)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Inserts obj into a red/black tree" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) tree)
           (type function <)
           (optimize (speed 0) (safety 3) (debug 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  ;(check-type tree (or null r/b-node))
  ;(check-type < function)
  (if (null tree)
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#    ; special case: empty tree
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#    (let ((node (make-r/b-node :data obj :color :black)))
      (values node node))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#    ; search for insertion point
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#    (let (parent
          branch-left?
          (child tree))
      (declare (type (or null r/b-node) parent child))
      (loop until (null child) do
            (setq branch-left? (funcall < obj (r/b-node-data child))
                  parent child)
            (if branch-left?
              (setq child (r/b-node-left-child child))
              (setq child (r/b-node-right-child child))))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#      ; now parent is the place where a new child node needs to be added
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#      (if branch-left?
        (let ((new-node (make-r/b-node :data obj
                                       :parent parent)))
          (setf (r/b-node-left-child parent) new-node)
          (values (r/b-insert-fixup tree new-node)
                  new-node))
        (let ((new-node (make-r/b-node :data obj
                                       :parent parent)))
          (setf (r/b-node-right-child parent) new-node)
          (values (r/b-insert-fixup tree new-node)
                  new-node))))))

(defun list->r/b-tree (lst <)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Constructs a red/black-tree from a list of objects; the tree's order is given by <" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (let (tree)
    (dolist (obj lst)
      (setq tree (r/b-tree-insert tree obj <)))
    tree))


(defun r/b-tree-depth (tree)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Returns the depth of tree" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (if (null tree)
    0
    (1+ (max (r/b-tree-depth (r/b-node-left-child tree))  
             (r/b-tree-depth (r/b-node-right-child tree))))))


(defun r/b-tree-count (tree)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Returns the number of nodes in tree" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (if (null tree)
    0
    (+ 1
       (r/b-tree-count (r/b-node-left-child tree))
       (r/b-tree-count (r/b-node-right-child tree)))))


#|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#;;
;; The following macros & function are used by the MIT version of delete-fixup
;;   
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#

(defmacro b->d (left?)
  `(IF ,left? :LEFT :RIGHT))

(defmacro -d (d)
  `(IF (EQ :LEFT ,d) :RIGHT :LEFT))

(defmacro get-link+ (p d)
  `(IF (EQ :LEFT ,d)
       (R/B-NODE-LEFT-CHILD ,p)
       (R/B-NODE-RIGHT-CHILD ,p)))

(defmacro set-link+ (p d l)
  `(IF (EQ :LEFT ,d)
       (SETF (R/B-NODE-LEFT-CHILD ,p) ,l)
       (SETF (R/B-NODE-RIGHT-CHILD ,p) ,l)))

(defmacro get-link- (p d)
  `(IF (EQ :RIGHT ,d)
       (R/B-NODE-LEFT-CHILD ,p)
       (R/B-NODE-RIGHT-CHILD ,p)))

(defmacro set-link- (p d l)
  `(IF (EQ :RIGHT ,d)
       (SETF (R/B-NODE-LEFT-CHILD ,p) ,l)
       (SETF (R/B-NODE-RIGHT-CHILD ,p) ,l)))

(defun rotate+ (tree x d)
  (let* ((root tree)
         (y    (get-link- x d))
         (beta (get-link+ y d)))
    (set-link- x d beta)
    (and beta (setf (r/b-node-parent beta) x))
    (let ((u (r/b-node-parent x)))
      (setf (r/b-node-parent y) u)
      (cond ((null u) (setq root y))
            ((eq x (get-link+ u d)) (set-link+ u d y))
            (t (set-link- u d y))))
    (set-link+ y d x)
    (setf (r/b-node-parent x) y)
    root))

(defmacro rotate- (tree x d)
  `(ROTATE+ ,tree ,x (-D ,d)))

(defun delete-fixup-case-4 (tree w u d)  
  (setf (r/b-node-color w) (r/b-node-color u))
  (setf (r/b-node-color u) :BLACK)
  (setf (r/b-node-color (get-link- w d)) :BLACK)
  (let ((root (rotate+ tree u d)))
    (setf (r/b-node-color tree) :BLACK)
    root))

(defun r/b-delete-fixup (tree x u)
  (if (or (not u)
          (and x (eq (r/b-node-color x) :RED)))
    (if u						; added (if u (progn ...) tree)
      (progn
        (when x (setf (r/b-node-color x) :BLACK))
        (let* ((root tree)
               (d  (b->d (eq x (r/b-node-left-child u))))
               (w  (get-link- u d)))
          (when (and w (eq (r/b-node-color w) :RED))	; added (and w ...)
            (setf (r/b-node-color w) :BLACK
                  (r/b-node-color u) :RED
                  root (rotate+ root u d)
                  w (get-link- u d)))
          (let ((n- (and w (get-link- w d))))		; added (and w ...)
            (if w					; added (if w (progn ...) root)
              (progn
                (if (and n-
                         (eq (r/b-node-color n-) :red))
                  (delete-fixup-case-4 root w u d)
                  (let ((n+ (get-link+ w d)))
                    (if (and n+
                             (eq (r/b-node-color (get-link+ w d)) :RED))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#                      ;; case 3
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#                      (progn
                        (setf (r/b-node-color n+) :BLACK
                              (r/b-node-color w) :RED
                              root (rotate- root w d))
                        (delete-fixup-case-4 root (get-link- u d) u d))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#                      ;; case 2
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#                      (progn
                        (setf (r/b-node-color w) :RED)
                        (r/b-delete-fixup root u (r/b-node-parent u)))))))
              root))))
      (progn						; added fixup case for deleting root
        (setf (r/b-node-color tree) :black)
        tree))
    tree))


(defun r/b-tree-delete-node (tree node%)
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#"Deletes a node from a tree" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#(declare (type (or null r/b-node) tree)
           (type r/b-node node%))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  ;(check-type node% r/b-node)
  ;(check-type tree (or null r/b-node))
  (unless (and (eq tree node%)
               (null (r/b-node-left-child tree))
               (null (r/b-node-right-child tree)))
    (let* ((node (cond ((r/b-node-left-child node%)
                        (let ((prev (r/b-node-predecessor node%)))
                          (setf (r/b-node-data node%) (r/b-node-data prev))
                          prev))
                       ((r/b-node-right-child node%)
                        (let ((next (r/b-node-successor node%)))
                          (setf (r/b-node-data node%) (r/b-node-data next))
                          next))
                       (t
                        node%)))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#           ; node is non-nil; it stands for the node actually removed from tree
           ; which can be different from 'node' to avoid some re-linking; in this case,
           ; the data originally stored in node%'s successor or predecessor is saved to node%
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#           (child (or (r/b-node-left-child node)
                      (r/b-node-right-child node)))
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#           ; child is node's child or nil - note: there exists atmost 1 child of node
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#           (parent (r/b-node-parent node))
           (root tree))
      (declare (type (or null r/b-node) child)
               (type r/b-node root node))
      
      
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#      ; link child (if that exists) to node's parent (splicing out node direction child->parent)
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#      (when child
        (setf (r/b-node-parent child) parent))
      
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#      ; link node's parent (if that exists) to child (splicing out node in direction parent->child)
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#      (if (null parent)
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#        ; if no parent exists child is the new root; the old root 'node' is, hence, black. 
        ; Therefore, the fixup gets invoked, coloring the new root black
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#        (setq root child)	
        (if (eq node (r/b-node-left-child parent))
          (setf (r/b-node-left-child parent) child)
          (setf (r/b-node-right-child parent) child)))
      
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.8))_|#      ; only when removing a black node, r/b tree properties may have been violated,
      ; requiring a fixup. 
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#      (if (eq :black (r/b-node-color node))
        (r/b-delete-fixup root child parent)
        root))))

(defun r/b-tree-delete (tree obj < &key (test #'eq))
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Deletes an obj from tree" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) tree)
           (type function < test)
           (optimize (speed 0) (safety 3) (debug 3)))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  ;(check-type tree r/b-node)
  ;(check-type < function)
  ;(check-type test function)
  (let ((node (r/b-tree-find tree obj < :test test)))
    #|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|#(declare (type (or null r/b-node) node))#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
    (if node
      (r/b-tree-delete-node (the r/b-node tree) (the r/b-node node))
      tree)))

(defmacro r/b-tree-push (tree obj <)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Pushes obj onto tree, returning the modified tree. equivalent to (setf tree (r/b-tree-insert tree obj <))" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  `(setf ,tree (r/b-tree-insert ,tree ,obj ,<)))

(defmacro r/b-tree-pop-min (tree)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Pops the tree's minimum and returns it" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (let ((min (gensym))
        (data (gensym)))
    `(let* ((,min (r/b-tree-min-node ,tree))
            (,data (and ,min (r/b-node-data ,min))))
       (when ,min
	 (setf ,tree (r/b-tree-delete-node ,tree ,min)))
       ,data)))

(defmacro r/b-tree-pop-max (tree)
  #|_("Monaco" 9 :PLAIN  (0.0 0.4 0.0))_|#"Pops the tree's maximum and returns it" #|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#
  (let ((max (gensym))
        (data (gensym)))
    `(let* ((,max (r/b-tree-max-node ,tree))
	    (,data (r/b-node-data ,max)))
       (setf ,tree (r/b-tree-delete-node ,tree ,max))
       ,data)))



#|_("Monaco" 9 :PLAIN  (0.5 0.5 0.5))_|##|
;;;
;;; TEST CASES FOR R/B-TREES
;;;


;; Eval either on of the *item* definitions

; For unique keys
(defparameter *items* (let (items)
                        (dotimes (i 5000)
                          (let ((new-item (* 0.001 (random 1000000))))
                            (unless (find new-item items :test #'=)
                              (push new-item items))))
                        items))

; For *lots* of equal keys
(defparameter *items* (let (items)
                        (dotimes (i 50000)
                          (push (random 50) items))
                        items))

;; 
;;  TIMING TEST
;; 
(let (tree)
  (print "Tree construction")
  (time (dolist (i *items*)
          (setq tree (r/b-tree-insert tree i #'<))))
  
  (format t "~%The tree contains ~a objects out of ~a inserted." (r/b-tree-count tree) (length *items*))
  

  (print "Searching")
  (time (print (every #'(lambda (i)
                          (r/b-tree-find tree i #'<))
                      *items*)))


  (print "Removal")
    (time (dolist (i *items*)
            (setq tree (r/b-tree-delete tree i #'<))))
  nil)

;;;
;;; TEST FOR SOUNDNESS
;;;

(defun delete-1 (item lst)
  (labels ((del (rst-lst accu)
             (if (eq (car rst-lst) item)
               (nconc (cdr rst-lst) accu)
               (del (cdr rst-lst) (cons (car rst-lst) accu)))))
    (del lst nil)))

;;
;; Eval this block (infinite loop) to test soundness 
;;
(let (objects-in-tree
      tree
      (action-count 0)
      (num-of-objects 0))
  (loop
    (let ((random (random 10)))
      ; pick out some action by random
      (cond ((< random (if (< num-of-objects 1000) 5 3))
             ; insert a new element
             (let ((new-elem (random 10000)))
               (push new-elem objects-in-tree)
               (incf num-of-objects)
               (setq tree (r/b-tree-insert tree new-elem #'<))))
	    
            ((< random 7)
             ; search for some element
             (when objects-in-tree
               (let ((elem (nth (random num-of-objects) objects-in-tree)))
                 (unless elem (break "Haehh?!"))
                 (unless (r/b-tree-find tree elem #'< :test #'=)
                   (error "Once inserted object not found!")))))
             
            (t
             ; delete some element
             (when objects-in-tree
               (let ((elem (nth (random num-of-objects) objects-in-tree)))
                 (setq tree (r/b-tree-delete tree elem #'<)
                       objects-in-tree (delete-1 elem objects-in-tree)
                       num-of-objects (1- num-of-objects))))))
      (incf action-count)
      (when (= 0 (mod action-count 10000))
        (format t ".(~a)." num-of-objects)))))

;; Object finding when keys occur multiply

(defparameter *tree* nil)
(defparameter *objects* nil)

(defun order (x y)
  (< (car x) (car y)))

(progn
  (print "Construction")
  (dotimes (i 500)
    (let ((obj (cons (random 50) (random 1000000))))
      (push obj *objects*)
      (setq *tree* (r/b-tree-insert *tree* obj #'order)))))
  
(progn
  (print "Searching")
  (setf *searched-nodes* 0 *keys* nil)
  (time (every #'(lambda (obj)
                   (r/b-tree-find *tree* obj #'order))
               *objects*))
  (print *searched-nodes*))

(progn
  (print "Searching non-existing object")
  ;(setf *searched-nodes* 0 *keys* nil)
  (time (r/b-tree-find *tree* '(49 . -1) #'order))
  ;(print *searched-nodes*)
  ;(pprint *keys*)
  nil)


|#