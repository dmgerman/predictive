
;;; Copyright (C) 2004 Toby Cubitt
;;;
;;; This file is part of the Emacs Predictive Completion package.
;;;
;;; The Emacs Predicive Completion package is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software
;;; Foundation; either version 2 of the License, or (at your option)
;;; any later version.
;;;
;;; The Emacs Predicive Completion package is distributed in the hope
;;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with the Emacs Predicive Completion package; if not, write
;;; to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;; Boston, MA 02111-1307 USA


;;; Commentary:
;;;
;;; A ternary search tree consists of two cons cells, the first one
;;; holding the tag 'TSTREE in the car cell and the second one having
;;; the tree in the car and the compare function in the cdr cell. The
;;; compare function must take two arguments of the type which is to
;;; be stored in the tree and must return a negative value if the
;;; first argument is "less than" the second, a positive value if the
;;; first argument is "greater than" the second, and zero if the two
;;; arguments are "equal".
;;;
;;;
;;; This package uses the elib LIFO stack package, stack-m.el, and the
;;; ternary heap package heap.el.
;;;


;;; Code:

(provide 'tstree)

(require 'stack-m)
(require 'heap)



;;; ================================================================
;;;  Internal functions for use in the ternary search tree package


(defmacro tst-tree-root (tree)  ; INTERNAL USE ONLY.
  ;; Return the root node for a ternary search tree.
  `(tst-node-equal (car (cdr ,tree)))
)



(defmacro tst-tree-dummyroot (tree)  ; INTERNAL USE ONLY.
  ;; Return the dummy node of a ternary search tree.
  `(car (cdr ,tree))
)



(defmacro tst-tree-cmpfun (tree)  ; INTERNAL USE ONLY.
  ;; Return the compare function of ternary search tree TREE.
  `(car (cdr (cdr ,tree)))
)



(defmacro tst-tree-insfun (tree)  ; INTERNAL USE ONLY.
  ;; Return the insert function of ternary search tree TREE.
  `(car (cdr (cdr (cdr ,tree))))
)


(defmacro tst-tree-rankfun (tree)  ; INTERNAL USE ONLY
  ;; Return the rank function of ternary search tree TREE.
  `(cdr (cdr (cdr (cdr ,tree))))
)


(defmacro tst-node-create (low equal high split)  ; INTERNAL USE ONLY.
  ;; Create a TST node from LOW, EQUAL, HIGH and SPLIT.
  ;; Note: If SPLIT is nil, EQUAL stores data rather than a pointer
  `(vector ,low ,equal ,high ,split)
)



(defmacro tst-node-p (obj)  ; INTERNAL USE ONLY
  ;; Return t if OBJ is a valid ternary search tree node, nil
  ;; otherwise.
  `(and (vectorp ,obj) (= (length ,obj) 4))
)



(defmacro tst-node-low (node)  ; INTERNAL USE ONLY.
  ;; Return the low pointer of NODE.
  `(aref ,node 0)
)



(defmacro tst-node-equal (node)  ; INTERNAL USE ONLY.
  ;; Return the equal pointer of NODE.
  `(aref ,node 1)
)



(defmacro tst-node-high (node)  ; INTERNAL USE ONLY.
  ;; Return the high pointer of NODE.
  `(aref ,node 2)
)



(defmacro tst-node-split (node)  ; INTERNAL USE ONLY.
  ;; Return the split value of NODE.
  `(aref ,node 3)
)




(defmacro tst-node-branch (node d)  ; INTERNAL USE ONLY.
  ;; For D negative, zero, or positive, return the low, equal or high
  ;; pointer of NODE respectively.
  `(aref ,node (1+ (signum ,d)))
)



(defmacro tst-node-set-high (node newhigh)  ; INTERNAL USE ONLY.
  ;; Set the high pointer of NODE to NEWHIGH
  `(aset ,node 0 ,newhigh)
)



(defmacro tst-node-set-equal (node newequal)  ; INTERNAL USE ONLY.
  ;; Set the equal pointer of NODE to NEWEQUAL
  `(aset ,node 1 ,newequal)
)



(defmacro tst-node-set-low (node newlow)  ; INTERNAL USE ONLY.
  ;; Set the low pointer of NODE to NEWLOW
  `(aset ,node 2 ,newlow)
)



(defmacro tst-node-set-split (node newsplit)  ; INTERNAL USE ONLY.
  ;; Set the split value of NODE to NEWSPLIT
  `(aset ,node 3 ,newsplit)
)



(defmacro tst-node-set-branch (node d newbranch)  ; INTERNAL USE ONLY.
  ;; If D is negative, zero or positive, set the high, equal or low
  ;; value respectively of NODE to NEWBRANCH.
  `(aset ,node (1+ (signum ,d)) ,newbranch)
)




(defun tst-node-find (tree string)  ; INTERNAL USE ONLY
  ;; Returns the node corresponding to STRING, or nil if none found.
  
  (cond
   ;; don't search for nil!
   ((null string) nil)
   ;; return root node if searching for an empty string
   ((= 0 (length string)) (tst-tree-root tree))
   ;; otherwise search for node corresponding to string
   (t (let ((cmpfun (tst-tree-cmpfun tree))
	    (node (tst-tree-root tree))
	    (c 0) (chr (aref string 0)) (d 0)
	    (len (length string)))
	
        ;; as long as we keep finding nodes, keep descending the tree
	(while (and node (< c len))
	  (setq d (funcall cmpfun chr (tst-node-split node)))
	  (if (= 0 d)
	      (when (< (setq c (1+ c)) len) (setq chr (aref string c))))
	  (setq node (tst-node-branch node d)))
	node))
  )
)




;;; ================================================================
;;;    The public functions which operate on ternary search trees.


(defun tstree-create (&optional compare-function insert-function
				rank-function)
  "Create an empty ternary search tree. If no arguments are supplied, it
creates a tree suitable for storing strings with numerical data.

The optional COMPARE-FUNCTION sets the comparison function for the
tree. COMPARE-FUNCTION takes two arguments, A and B, and returns a negative
value if A is less than B, zero if A is equal to B, and a positive value if A
is greater than B. It defaults to subtraction.

The optional INSERT-FUNCTION takes two arguments of the type stored as data in
the tree or nil, and returns the same type. It defaults to \"replace\". See
`tstree-insert'.

The optional RANK-FUNCTION takes two arguments, each a cons whose car is a
vector referencing data in the tree, and whose cdr is the data at that
reference. It should return non-nil if the first argument is \"better than\"
the second, nil otherwise. It defaults numerical comparison of the data using
\"greater than\". Used by `tstree-complete-ordered' to rank completions."

         ;; comparison-function defaults to -
  (let* ((cmpfun (when compare-function compare-function '-))
	 ;; the lambda expression redefines the compare funtion to ensure that
	 ;; all values other than nil are "greater" than nil
	 (cmpfun `(lambda (a b)
		    (cond ((and (null a) (null b)) 0) ((null a) -1)
			  ((null b) 1) (t (,cmpfun a b)))))
	 ;; insert-function defaults to "replace".
	 (insfun (if insert-function insert-function '(lambda (a b) a)))
	 ;; rank function defaults to >
	 (rankfun (if rank-function rank-function
		    '(lambda (a b) (> (cdr a) (cdr b))))))
  
    (cons 'TSTREE
	  (cons (tst-node-create nil nil nil t)
		(cons cmpfun
		      (cons insfun rankfun))))
  )
)




(defun tstree-p (obj)
  "Return t if OBJ is a ternary search tree, nil otherwise."
  (eq (car-safe obj) 'TSTREE)
)



(defun tstree-compare-function (tree)
  "Return the comparison function for the ternary search tree TREE."
  (tst-tree-cmpfun tree)
)



(defun tstree-insert-function (tree)
  "Return the insertion function for the ternary search tree TREE."
  (tst-tree-insfun tree)
)



(defun tstree-rank-function (tree)
  "Return the rank function for the ternary seach tree TREE."
  (tst-tree-rankfun tree)
)



(defun tstree-empty (tree)
  "Return t if the ternary search tree TREE is empty, nil otherwise."
  (null (tst-tree-root tree))
)



(defun tstree-insert (tree string &optional data insert-function)
  "Calculate the result of applying the tree TREE's insetion function to DATA
and the existing data at position STRING in the tree (or nil if empty), and
insert the result into the ternary search tree at the position referenced by
STRING. STRING must be a vector containing the type used to reference data
in the tree.

The optional INSERT-FUNCTION over-rides the tree's own insertion function. It
should take two arguments of the type stored as data in the tree, or nil. The
first is the data DATA, the second is the data stored at position STRING in
the tree, or nil if STRING doesn't yet exist. It should return the same
type. The return value is stored in the tree."
  
  ;; don't add empty strings to the tree
  (if (= 0 (length string)) nil
    
    (let ((cmpfun (tst-tree-cmpfun tree))
	  (insfun (if insert-function insert-function (tst-tree-insfun tree)))
	  (node (tst-tree-dummyroot tree))
	  (c 0) (chr (aref string 0)) (d 0)
	  (len (length string)) newdata)
      
      ;; as long as we keep finding nodes, keep descending the tree
      (while (and node (tst-node-branch node d))
	(setq node (tst-node-branch node d))
	(setq d (funcall cmpfun chr (tst-node-split node)))
	(if (= 0 d)
	    (if (< (setq c (1+ c)) len)
		(setq chr (aref string c))
	      ;; if complete string already exists in the tree and
	      ;; we've found the data node, insert new data
	      (if (tst-node-split node)
		  (setq chr nil)  ; not at data node so keep descending
		(tst-node-set-equal
		 node (setq newdata
			    (funcall insfun data (tst-node-equal node))))
		(setq node nil)))))  ; forces loop to exit
      
      ;; once we've found one node that doesn't exist, must create all others
      (while node
	;; create nodes for remainder of string, if any
	(if (< c len)
	    (progn
	      (setq chr (aref string c))
	      (tst-node-set-branch node d (tst-node-create nil nil nil chr))
	      (setq node (tst-node-branch node d))
	      (setq d 0)
	      (setq c (1+ c)))
	  ;; if we've reached end of string, create data node and exit
	  (tst-node-set-branch
	   node d (tst-node-create
		   nil (setq newdata (funcall insfun data nil)) nil nil))
	  (setq node nil)))  ; fores loop to exit
      
      ;; return the newly inserted data
      newdata)
  )
)




(defun tstree-member (tree string)
  "Return the data referenced by STRING from the tree TREE, or nil if
STRING does not exist in the tree. Note: this will not distinguish
between a non-existant STRING and a STRING whose data is nil. Use
`tstree-member-p' instead."

  ;; Find first node corresponding to STRING
  (let ((node (tst-node-find tree string)))
    
    ;; Keep following the low branch until we find the data node, or
    ;; can't go any further.
    (while (tst-node-p node)
      (setq node (if (tst-node-split node) (tst-node-low node)
		   (tst-node-equal node))))
    node)
)



(defun tstree-member-p (tree string)
  "Return t if STRING is in tree TREE, nil otherwise."
  
  (let ((node (tst-node-find tree string)))
    
    (while (tst-node-p node)
      (setq node (if (tst-node-split node) (tst-node-low node)
		   (setq node t))))
    node)
)



;; Deleting strings from a ternary search tree is a messy
;; operation. Basically, either the tree has to be left with redundant
;; nodes and probably nodes with nil equal children, or the sub-tree
;; below the string needs to be restructured.
;;
;; Possible solutions are either to leave the tree in a mess, or delete
;; the entire sub-tree then add the strings it contained back
;; again. Both are undesirable: the former because it leaves the tree
;; with redundant nodes that apart from making the tree slightly
;; inefficient, might even cause errors when running functions;; the
;; latter because it could potentially be very inefficient.
;;
;; The best option is probably to make sure you never need to delete
;; strings from the tree! Therefore I haven't bothered writing the
;; following function:
;;
;; (defun tstree-delete (tree string)
;;   "Delete string STRING from tree TREE."
;; )



(defun tstree-complete (tree string &optional maxnum all)
  "Return a vector containing all completions of STRING found in ternary searh
tree TREE, in \"alphabetial\" order (i.e. the order defined by the tree's
comparison function). Each element of the returned vector is a cons containing
the completed string and its associated data. If no completions are found,
return nil.

STRING must either be a vector containing elements of the type used to
reference data in the tree, or a list of such vectors. (If the tree stores
real strings, STRING can be a string or a list of strings, since strings are
treated internally as vectors of numbers.) If a list is supplied, completions
of all elements of the list are included in the returned vector.

The optional numerical argument MAXNUM limits the results to the first
MAXNUM completions. If it is absent or nil, all completions are
returned.

Normally, only the remaining characters needed to complete STRING are
returned. If the optional argument ALL is non-nil, the entire completion is
returned."
  
  (let ((stack (stack-create))
	(completions []))
    
    ;; ----- initialise the stack -----
    (let ((strlist (if (listp string) (reverse (sort string 'string<))
		     (list string)))
	  str)
      ;; add initial nodes for each string in the string list
      (while strlist
	(setq str (pop strlist))
	;; if completions exist, add initial node to the stack
	(if (car (stack-push stack (tst-node-find tree str)))
	    ;; force entire completion to be returned if arg ALL was set
	    (if all (stack-push stack str)
	      (if (stringp str) (stack-push stack "") (stack-push stack [])))
	  (stack-pop stack)))
    )
    
    ;; ----- search the tree -----
    (let ((num 0) str node)
      ;; Keep going until we've searched all nodes (node stack is empty), or
      ;; have found enough completions.
      (while (and (not (stack-empty stack)) (or (null maxnum) (< num maxnum)))
	(setq str (stack-pop stack))
	(setq node (stack-pop stack))
        ;; add the high child to the stack, if it exists
	(when (tst-node-high node)
	  (stack-push stack (tst-node-high node))
	  (stack-push stack str))
        ;; If we're at a data node, we've found a completion. Otherwise, add
        ;; the equal child to the stack.
	(if (tst-node-split node)
	    (progn
	      (stack-push stack (tst-node-equal node))
	      (stack-push stack
			  (if (stringp str)
			      (concat str (string (tst-node-split node)))
			    (vconcat str (vector (tst-node-split node))))))
	  (setq completions
		(vconcat completions
			 (vector (cons str (tst-node-equal node)))))
	  (setq num (1+ num)))
        ;; add the low child to the stack, if it exists
	(when (tst-node-low node)
	  (stack-push stack (tst-node-low node))
	  (stack-push stack str)))
    )
    
    ;; return nil if no completions were found, otherwise return the vector of
    ;; completions
    (if (= 0 (length completions)) nil completions))
)




(defun tstree-complete-ordered (tree string
				     &optional maxnum all rank-function)
  "Return a vector containing all completions of STRING found in ternary
search tree TREE. Each element of the returned vector is a cons containing the
completed string and its associated data. If no completions are found, return
nil.

Note that `tstree-complete' is significantly more efficient than
`tstree-complete-ordered', especially when a maximum number of completions is
specified. Always use `tstree-complete' when you don't care about the ordering
of the completions, or you need the completions ordered \"alphabetically\".

TREE can also be a list of ternary search trees, in which case completions are
sought in all trees in the list.

STRING must either be a vector containing elements of the type used to
reference data in the tree, or a list of such vectors. (If the tree stores
real strings, STRING can be a string or a list of strings, since strings are
treated internally as vectors of numbers.) If a list is supplied, completions
of all elements of the list are included in the returned vector.

The optional numerical argument MAXNUM limits the results to the \"best\"
MAXNUM completions. If nil, all completions are returned.

Normally, only the remaining characters needed to complete STRING are
returned. If the optional argument ALL is non-nil, the entire completion is
returned.

The optional argument RANK-FUNCTION over-rides the tree's default rank
function. It should take two arguments, each a cons whose car is a vector
referencing data in the tree, and whose cdr is the data at that reference. It
should return non-nil if the first argument is \"better than\" the second, nil
otherwise. The elements of the returned vector are sorted according to this
rank-function, in descending order."
  
  (let* ((stack (stack-create))
	 (completions [])
	 heap)
    
    
    ;; ----- initialise the stack and heap -----
    (let ((treelist (if (tstree-p tree) (list tree) tree))
	  strlist tmptree tmpstr rankfun)
      
      ;; create the heap using the rank-function from the first tree in the
      ;; list
      (setq rankfun (if rank-function rank-function
		      (tst-tree-rankfun (car treelist))))
      (setq heap (heap-create `(lambda (a b) (not (,rankfun a b)))))
      
      ;; add initial nodes from each tree.in the tree list
      (while treelist
	(setq tmptree (pop treelist))
	(setq strlist (if (listp string) string (list string)))
	;; ...and for each string in the string list
	(while strlist
	  (setq tmpstr (pop strlist))
	  ;; if completions exist, add initial node to the stack
	  (if (car (stack-push stack (tst-node-find tmptree tmpstr)))
	      ;; force entire completion to be returned if arg ALL was set
	      (if all (stack-push stack tmpstr)
		(stack-push stack (if (stringp tmpstr) "" [])))
	    (stack-pop stack))))
    )
    
    
    ;; ------ search the tree -----
    (let ((num 0) str node)
      ;; keep going until we've searched all nodes (node stack is empty)
      (while (not (stack-empty stack))
	(setq str (stack-pop stack))
	(setq node (stack-pop stack))
        ;; add the high child to the stack, if it exists
	(when (tst-node-high node)
	  (stack-push stack (tst-node-high node))
	  (stack-push stack str))
        ;; If we're at a data node, we've found a completion. Otherwise, add
        ;; the equal child to the stack.
	(if (tst-node-split node)
	    (progn
	      (stack-push stack (tst-node-equal node))
	      (stack-push stack
			  (if (stringp str)
			      (concat str (string (tst-node-split node)))
			    (vconcat str (vector (tst-node-split node))))))
	  ;; Add completions to the heap. If we already have enough
	  ;; completions, delete the worst one from the heap.
	  (heap-add heap (cons str (tst-node-equal node)))
	  (setq num (1+ num))
	  (when (and maxnum (> num maxnum)) (heap-delete-root heap)))
        ;; add the low child to the stack, if it exists
	(when (tst-node-low node)
	  (stack-push stack (tst-node-low node))
	  (stack-push stack str)))
    )
      
      
    ;; ----- create the completions vector -----
    (setq completions [])
    ;; repeatedly transfer the worst completion left in the heap to the
    ;; front of the completions vector
    (while (not (heap-empty heap))
      (setq completions
	    (vconcat (vector (heap-delete-root heap)) completions)))
    
    ;; return nil if no completions were found, otherwise return the vector of
    ;; completions
    (if (= 0 (length completions)) nil completions))
)
