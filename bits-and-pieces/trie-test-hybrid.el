
(require 'avl-tree)

(defun test-hybrid-create (cmpfun seq)
  (declare (special trie-depth))
  (if (>= (length seq) 2)
      (avl-tree-create cmpfun)
    (let ((vec (make-vector 28 nil)))
      (aset vec 26 (avl-tree-create cmpfun))    ; underflow tree
      (aset vec 27 (avl-tree-create cmpfun))))) ; overlflow tree

(defun test-hybrid-insert (array node &optional updatefun)
  (if (avl-tree-p array)
      (avl-tree-enter array node updatefun)
    (let ((idx (- (trie--node-split node) ?a)))
      (cond
       ((< idx 0) (avl-tree-enter (aref array 26) node updatefun))
       ((> idx 25) (avl-tree-enter (aref array 27) node updatefun))
       (t
	(let ((old (aref array idx)))
	  (aset array idx
		(if old
		    (funcall updatefun node old)
		  node))))))))

(defun test-hybrid-delete (array node &optional test nilflag)
  (if (avl-tree-p array)
      (avl-tree-delete array node test nilflag)
    (let ((idx (- (trie--node-split node) ?a)))
      (cond
       ((< idx 0)  (avl-tree-delete (aref array 26) node test nilflag))
       ((> idx 25) (avl-tree-delete (aref array 27) node test nilflag))
       (t
	(let ((element (aref array idx)))
	  (if (or (null element)
		  (and test (not (funcall test element))))
	      nilflag
	    (aset array idx nil)
	    element)))))))

(defun test-hybrid-member (array node &optional nilflag)
  (if (avl-tree-p array)
      (avl-tree-member array node nilflag)
    (let ((idx (- (trie--node-split node) ?a)))
      (cond
       ((< idx 0)  (avl-tree-member (aref array 26) node nilflag))
       ((> idx 25) (avl-tree-member (aref array 27) node nilflag))
       (t
	(let ((element (aref array idx)))
	  (if element element nilflag)))))))

(defun test-hybrid-mapc (function array &optional reverse)
  (if (avl-tree-p array)
      (avl-tree-mapc function array reverse)
    (avl-tree-mapc function (aref array (if reverse 27 26)) reverse)
    (let ((len (- (length array) 2)))
      (dotimes (i len)
	(funcall function (aref array (if reverse (- len i 1) i)))))
    (avl-tree-mapc function (aref array (if reverse 26 27)) reverse)))

(defun test-hybrid-empty (array)
  (if (avl-tree-p array)
      (avl-tree-empty array)
    (catch 'not-empty
      (unless (and (avl-tree-empty (aref array 26))
		   (avl-tree-empty (aref array 27)))
	(throw 'not-empty nil))
      (dotimes (i (length array))
	(unless (null (aref array i)) (throw 'not-empty nil)))
      t)))




(setq trie-test-hybrid
      (trie-create-custom
       '<
       :createfun 'test-hybrid-create
       :insertfun 'test-hybrid-insert
       :deletefun 'test-hybrid-delete
       :lookupfun 'test-hybrid-member
       :mapfun 'test-hybrid-mapc
       :emptyfun 'test-hybrid-empty))


(dictree-create-custom 'dict-test-hybrid nil nil t
		       :createfun 'test-hybrid-create
		       :insertfun 'test-hybrid-insert
		       :deletefun 'test-hybrid-delete
		       :lookupfun 'test-hybrid-member
		       :mapfun 'test-hybrid-mapc
		       :emptyfun 'test-hybrid-empty)


(dictree-populate-from-file
 dict-test
 "~/programming/predictive/dict-english.word-list")
