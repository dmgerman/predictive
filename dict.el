
;;; dict.el --- dictionary package

;; Copyright (C) 2004 2005 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.4
;; Keywords: dictionary

;; This file is part of the Emacs Predictive Completion package.
;;
;; The Emacs Predicive Completion package is free software; you can
;; redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; The Emacs Predicive Completion package is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the Emacs Predicive Completion package; if not, write
;; to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA


;;; Commentary:
;;
;; A dictionary consists of a list containing either 5 or 10 elements
;; (see the dict-create function for details).
;;
;; A dictionary is used to store strings, along with arbitrary data
;; associated with each string. The 'string' can be a sequence of any
;; data type, not just a string of characters. As well as basic data
;; insertion, manipulation and retrieval, a dictionary can perform
;; advanced searches on those strings (see the dict-complete and
;; dict-complete-ordered functions), and is able to cache results in
;; order to speed up those searches.
;;
;; This package uses the ternary search tree package, tstree.el.


;;; Change log:
;;
;; Version 0.4
;; * fixed bug in dict-read-line
;;
;; Version 0.3
;; * added dict-map function
;;
;; Version 0.2
;; * added dictionary autosave flag and related functions;
;; * fixed bug preventing dict caches being loaded properly;
;; * explicitly require cl.el;
;;
;; Note: version 0.1 dictionaries not compatible with version 0.2 and above!
;;
;; Version 0.1
;; * initial release



;;; Code:

(provide 'dict)
(require 'tstree)
;; the only required common-lisp functions are `subseq', `map' and `merge'
(require 'cl)




;;; ====================================================================
;;;  Internal functions and variables for use in the dictionary package


(defvar dict-loaded-list nil
  "Stores list of loaded dictionaries.")


(defmacro dic-name (dict)  ; INTERNAL USE ONLY
  ;; Return the name of dictonary DICT
  `(nth 1 ,dict)
)


(defmacro dic-set-name (dict name)  ; INTERBAL USE ONLY
  ;; Set the name of dictionary DICT
  `(setcar (cdr ,dict) ,name)
)


(defmacro dic-filename (dict)  ; INTERNAL USE ONLY.
  ;; Return the filename of dictionary DICT
  `(nth 2 ,dict)
)


(defmacro dic-set-filename (dict filename)  ; INTERNAL USE ONLY.
  ;; Set the filename of dictionary DICT
  `(setcar (cdr (cdr ,dict)) ,filename)
)


(defmacro dic-autosave (dict)  ; INTERNAL USE ONLY
  ;; Return the autosave flag of dictionary DICT
  `(nth 3 ,dict)
)


(defmacro dic-set-autosave (dict flag)  ; INTERNAL USE ONLY
  ;; Set the autosave flag of dictionary DICT
  `(setcar (cdr (cdr (cdr ,dict))) ,flag)
)


(defmacro dic-modified (dict)  ; INTERNAL USE ONLY
  ;; Return the modified flag of dictionary DICT
  `(nth 4 ,dict)
)


(defmacro dic-set-modified (dict flag)  ; INTERNAL USE ONLY
  ;; Set the modified flag of dictionary DICT
  `(setcar (cdr (cdr (cdr (cdr ,dict)))) ,flag)
)


(defmacro dic-tstree (dict)  ; INTERNAL USE ONLY.
  ;; Return the ternary search tree of dictionary DICT
  `(nth 5 ,dict)
)


(defmacro dic-lookup-only (dict)  ; INTERNAL USE ONLY.
  ;; Return the lookup-only setting of dictionary DICT
  `(nth 6 ,dict)
)


(defmacro dic-lookup-hash (dict)  ; INTERNAL USE ONLY
  ;; Return the lookup hash table of dictionary DICT
  `(nth 7 ,dict)
)


(defmacro dic-set-lookup-hash (dict hash)  ; INTERNAL USE ONLY
  ;; Set the completion hash for dictionary DICT
  `(setcar (cdr (cdr (cdr (cdr (cdr (cdr (cdr ,dict))))))) ,hash)
)


(defmacro dic-lookup-speed (dict)  ; INTERNAL USE ONLY
  ;; Return the lookup speed of dictionary DICT
  `(nth 8 ,dict)
)


(defmacro dic-completion-hash (dict)  ; INTERNAL USE ONLY
  ;; Return the completion hash table of dictionary DICT
  `(nth 9 ,dict)
)


(defmacro dic-set-completion-hash (dict hash)  ; INTERNAL USE ONLY
  ;; Set the completion hash for dictionary DICT
  `(setcar (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr ,dict))))))))) ,hash)
)


(defmacro dic-completion-speed (dict)  ; INTERNAL USE ONLY
  ;; Return the completion speed of dictionary DICT
  `(nth 10 ,dict)
)


(defmacro dic-ordered-hash (dict)  ; INTERNAL USE ONLY
  ;; Return the ordered completion hash table of dictionary DICT
  `(nth 11 ,dict)
)


(defmacro dic-set-ordered-hash (dict hash)  ; INTERNAL USE ONLY
  ;; Set the completion hash for dictionary DICT
  `(setcar (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr ,dict)
							     ))))))))))
	   ,hash)
)


(defmacro dic-ordered-speed (dict)  ; INTERNAL USE ONLY
  ;; Return the ordered completion speed of dictionary DICT
  `(nth 12 ,dict)
)


(defmacro dic-insfun (dict)  ; INTERNAL USE ONLY.
  ;; Return the insert function of dictionary DICT.
  `(if (dic-lookup-only ,dict)
       (nth 2 ,dict)
     (tst-tree-insfun (dic-tstree ,dict)))
)


(defmacro dic-rankfun (dict)  ; INTERNAL USE ONLY
  ;; Return the rank function of dictionary DICT.
  `(if (dic-lookup-only ,dict)
       nil
     (tst-tree-rankfun (dic-tstree ,dict)))
)



(defmacro cache-create (vect maxnum)  ; INTERNAL USE ONLY
  ;; Return a completion cache entry
  `(cons ,vect ,maxnum)
)


(defmacro cache-vect  (cache)  ; INTERNAL USE ONLY
  ;; Return the completions vector for cache entry CACHE
  `(car ,cache)
)


(defmacro cache-num (cache)  ; INTERNAL USE ONLY
  ;; Return the number of completions in cache entry CACHE
  `(cdr ,cache)
)





;;; ================================================================
;;;      The public functions which operate on dictionaries


(defun dict-p (obj)
  "Return t if OBJ is a dictionary, nil otherwise."
  (eq (car-safe obj) 'DICT)
)


(defun dict-name (dict)
  "Return dictionary DICT's name."
  (dic-name dict)
)


(defun dict-create (name &optional filename autosave
			      lookup-speed complete-speed
			      ordered-speed lookup-only
			      insert-function rank-function)
  "Create an empty dictionary stored in variable NAME, and return it.

Optional argument FILENAME supplies a directory and file name to use when
saving the dictionary. If the AUTOSAVE flag is non-nil, then the
dictionary will automatically be saved to this file when it is unloaded or
when exiting emacs.

The SPEED settings set the desired speed for the corresponding dictionary
search operations (lookup, completion, ordered completion), in seconds. If a
particular instance of the operation \(e.g. looking up the word \"cat\"\)
takes longer than this, the results will be cached in a hash table. If exactly
the same operation is requested subsequently, it should perform significantly
faster. \(Note \"should\": there's no guarantee!\) The down side is that the
memory or disk space required to store the dictionary grows, and inserting
words into the dictionary becomes slower, since the cache has to be
synchronized.

All SPEED's default to nil. The values nil and t are special. If a SPEED is
set to nil, no caching is done for that operation. If it is set to t,
everything is cached for that operation \(similar behaviour can be obtained by
setting the SPEED to 0, but it is better to use t\).

If LOOKUP-ONLY is non-nil, it disables all advanced search features for the
dictionary \(currently, completion\). All the SPEED settings are ignored, as
is the RANK-FUNCTION, and everything is stored in the lookup cache, even when
inserting data. This is appropriate when a dictionary is only going to be used
for lookup, since it speeds up lookups *and* decreases the memory required.


Optional argument INSERT-FUNCTION sets the function used to insert data into
the dictionary. It should take two arguments: the new data, and the data
already in the dictionary (or nil if none exists yet). It should return the
data to insert. It defaults to replacing any existing data with the new data.

Optional argument RANK-FUNCTION sets the function used to rank the results of
the `dict-complete-ordered' function. It should take two arguments, each a
cons whose car is a word in the dictionary and whose cdr is the data
associated with that word. It should return non-nil if the first argument is
\"better\" than the second, nil otherwise. It defaults to string comparison of
the words, ignoring the data \(which is not very useful, since the
`dict-complete' function already returns completions in alphabetical order
much more efficiently, but at least will never cause any errors, whatever data
is stored!\)"

  ;; a dictionary is a list containing:
  ;; ('DICT
  ;;  name
  ;;  filename
  ;;  autosave flag
  ;;  modified flag
  ;;  tstree/insert-function
  ;   lookup-only
  ;;  lookup-hash
  ;;  --- rest only if not lookup-only ---
  ;;  lookup-speed
  ;;  complete-hash
  ;;  complete-speed
  ;;  ordered-hash
  ;;  ordered-speed)
  (let ((dict
	 (if lookup-only
	     (list 'DICT name filename autosave t insert-function t
		   (make-hash-table :test 'equal))
	   (list 'DICT name filename autosave t
		 (tstree-create '- insert-function rank-function) nil
		 (if lookup-speed (make-hash-table :test 'equal) nil)
		 lookup-speed
		 (if complete-speed (make-hash-table :test 'equal) nil)
		 complete-speed
		 (if ordered-speed (make-hash-table :test 'equal) nil)
		 ordered-speed))
	 ))
    (push dict dict-loaded-list)
    dict)
)




(defun dict-create-type (name type &optional filename autosave
			      lookup-speed complete-speed ordered-speed)
  "Create an empty dictionary of type TYPE stored in variable NAME, and return
it. Type can be one of dictionary, spell-check, lookup, or
frequency. `dict-create-type' is a simplified interface to `dict-create'.

The \"dictionary\" type is exactly like a normal, paper-based dictionary: it
can associate arbitrary data with any word in the dictionary. Inserting data
for a word will replace any existing data for that word. All SPEED arguments
default to nil.

A \"spell-check\" dictionary stores words, but can not associate any data with
the words. It is appropriate when the dictionary will only be used for
checking if a word is in the dictionary (e.g. for spell-checking). All SPEED
arguments default to nil.

A \"lookup\" dictionary is like a dictionary-type dictionary, but can only be
used to look up words, not for more advanced searches (e.g. word
completion). This has both speed and memory benefits. It is appropriate when
the more advanced searches are not required. Any SPEED arguments are ignored.

A \"frequency\" dictionary associates a number with each word in the
dictionary. Inserting new data adds it to the existing data. It is
appropriate, for instance, when storing word-frequencies\; the
`dict-complete-ordered' function can then be used to return the most likely
completions. All SPEED arguments default to nil.

See `dict-create' for more details.


Technicalities:

For the \"dictionary\" type, INSERT-FUNCTION is set to \"replace\", and
RANK-FUNCTION to string comparison of the words (not very useful, since the
`dict-complete' function already returns completions sorted alphabetically,
and does it much more efficiently than `dict-complete-ordered', but at least
it will not cause errors!).

For the \"spell-check\" type, INSERT-FUNCTION is set to a function that always
returns t. RANK-FUNCTION is set to string comparison of the words.

For the \"lookup\" type, INSERT-FUNCTION is set to \"replace\", and
LOOKUP-ONLY is set to t.

For the \"frequency\" type, INSERT-FUNCTION sums the new and existing
data. Nil is treated as 0. The RANK-FUNCTION is set to numerical
\"greater-than\" comparison of the data."
  
  (let (insfun rankfun lookup-only)
    ;; set arguments based on type
    (cond
     ;; dictionary type
     ((eq type 'dictionary)
      (setq insfun (lambda (a b) a))
      (setq rankfun (lambda (a b) (string< (car a) (car b)))))
     
     ;; spell-check type
     ((eq type 'spell-check)
      (setq insfun (lambda (a b) t))
      (setq rankfun (lambda (a b) (string< (car a) (car b)))))
     
     ;; lookup type
     ((eq type 'lookup)
      (setq insfun (lambda (a b) a))
      (setq rankfun (lambda (a b) (string< (car a) (car b))))
      (setq lookup-only t))
     
     ;; frequency type
     ((eq type 'frequency)
      (setq insfun '(lambda (new old)
		      (cond ((and (nil new) (nil old)) 0)
			    ((nil new) old)
			    ((nil old) new)
			    (t (+ old new)))))
      (setq rankfun '(lambda (a b) (> (cdr a) (cdr b)))))
     )
    
    (dict-create name filename autosave
		 lookup-speed complete-speed ordered-speed
		 lookup-only insfun rankfun))
)




(defun dict-insert-function (dict)
  "Return the insertion function for dictionary DICT."
  (dic-insfun dict)
)



(defun dict-rank-function (dict)
  "Return the rank function for the dictionary DICT (note: returns nil if
lookup-only is set for the dictionary)."
  (dic-rankfun dict)
)



(defun dict-empty (dict)
  "Return t if the dictionary DICT is empty, nil otherwise."
  (if (dic-lookup-only dict)
      (= 0 (hash-table-count (dic-lookup-hash dict)))
    (tstree-empty (dic-tstree dict)))
)




(defun dict-insert (dict word &optional data insert-function)
  "Insert WORD and DATA into dictionary DICT.
If WORD does not already exist, this creates it. How the data is inserted
depends on the dictionary's insertion function (see `dict-create').

The optional INSERT-FUNCTION over-rides the dictionary's own insertion
function. It should take two arguments: the data DATA, and the data associated
with WORD in the dictionary (nil if none already exists). It should return the
data to insert."
  ;; make sure WORD is a string
  (when (not (stringp word))
    (error "Wrong argument type stringp, %s" (prin1-to-string word)))
  
  
  (let ((insfun (if insert-function insert-function (dic-insfun dict))))
    ;; set the dictionary's modified flag
    (dic-set-modified dict t)
    
    ;; if dictionary is lookup-only, just insert the data in the lookup cache
    (if (dic-lookup-only dict)
	(let ((lookup-hash (dic-lookup-hash dict)))
	  (puthash
	   word (funcall insfun data (gethash word lookup-hash))
	   lookup-hash))
      
      ;; otherwise...
      (let ((tstree (dic-tstree dict))
	    (lookup-hash (dic-lookup-hash dict))
	    (completion-hash (dic-completion-hash dict))
	    (ordered-hash (dic-ordered-hash dict))
	    newdata)
	
        ;; insert word in dictionary's ternary search tree
	(setq newdata (tstree-insert tstree word data insfun))
	
        ;; synchronize the lookup hash table
	(when (and (dic-lookup-speed dict) (gethash word lookup-hash))
	  (puthash word newdata lookup-hash))
	
        ;; synchronize the completion caches
	(when (or (dic-completion-speed dict) (dic-ordered-speed dict))
	  (let (str cache)
	    ;; have to check every possible substring that could be cached!
	    (dotimes (i (1+ (length word)))
	      (setq str (substring word 0 i))
	      
	      ;; synchronize the completion hash, if it exists
	      (when (and (dic-completion-speed dict)
			 (setq cache (gethash str completion-hash)))
		(puthash str (cons (tstree-complete
				    tstree str (cache-num cache))
				   (cache-num cache)) completion-hash))
	      
	      ;; synchronize the ordered completion hash, if it exists
	      (when (and (dic-ordered-speed dict)
			 (setq cache (gethash str ordered-hash)))
		(puthash str (cons (tstree-complete-ordered
				    tstree str (cache-num cache))
				   (cache-num cache)) ordered-hash)))))
	)))
)




(defun dict-lookup (dict word)
  "Return the data associated with WORD in dictionary DICT, or nil if WORD is
not in the dictionary.

Note: this will not distinguish between a non-existant WORD and a WORD whose
data is nil. \(\"spell-check\" type dictionaries created using
`dict-create-type' store t as the data for every word to avoid this problem)
Use `dict-member-p' to distinguish non-existant words from nil data."
  
  ;; first check the lookup hash for the word
  (let ((data (if (dic-lookup-speed dict)
		  (gethash word (dic-lookup-hash dict)) nil))
	time)
    
    ;; if it wasn't in the lookup hash and the dictionary isn't lookup-only,
    ;; search in the ternary search tree
    (unless (or data (dic-lookup-only dict))
      ;; time the lookup
      (let (time)
	(setq time (float-time))
	(setq data (tstree-member (dic-tstree dict) word))
	(setq time (- (float-time) time))
	
        ;; if the lookup was slower than the dictionary's lookup speed, add it
        ;; to the lookup hash and set the modified flag
	(when (and (dic-lookup-speed dict)
		   (or (eq (dic-lookup-speed dict) t)
		       (> time (dic-lookup-speed dict))))
	  (dic-set-modified dict t)
	  (puthash word data (dic-lookup-hash dict)))))
    
    ;; return the data
    data)
)




(defun dict-member-p (dict word)
  "Return t if WORD is in dictionary DICT, nil otherwise."
  
  ;; if dictionary is lookup-only, look in lookup hash and use dummy variable
  ;; to distinguish non-existant words from those with nil data
  (if (dic-lookup-only dict)
      (let (not-in-here)
	(if (eq (gethash word (dic-lookup-hash dict)) 'not-in-here) nil t))
    ;; otherwise look in the ternary search tree
    (tstree-member-p (dic-tstree dict) word))
)



;; (defun dict-delete (tree string)
;;   "Delete string STRING from dict TREE."
;; )



(defmacro dict-map (function dict)
  "Apply FUNCTION to all entries in dictionary DICT.

FUNCTION will be passed two arguments: a word from the dictionary, and the
data associated with that word. It is safe to assume the dictionary entries
will be traversed in alphabetical order."
  `(tstree-map ,function (dic-tstree ,dict) t)
)



(defun dict-complete (dict string &optional maxnum all)
  "Return a vector containing all completions of STRING found in dictionary
DICT, in alphabetial order. Each element of the returned vector is a cons
containing the completed string and its associated data. If no completions are
found, return nil.

DICT can also be a list of dictionaries, in which case completions are sought
in all dictionaries in the list, as though they were one large dictionary.

STRING can be a single string or a list of strings. If a list is supplied,
completions of all elements of the list are included in the returned vector.

The optional numerical argument MAXNUM limits the results to the first
MAXNUM completions. If it is absent or nil, all completions are
returned.

Normally, only the remaining characters needed to complete STRING are
returned. If the optional argument ALL is non-nil, the entire completion is
returned."
  
  (let* ((dictlist (if (dict-p dict) (list dict) dict)) dic
	 (rankfun (dic-rankfun (car dictlist)))
	 (completions [])
	 strlist str
	 cache vect cachenum
	 time speed)
    
    ;; search each dictionary in the list
    (while dictlist
      (setq dic (pop dictlist))
      
      ;; throw a wobbly if dictionary is lookup-only
      (when (dic-lookup-only dic)
	(error "Dictionary is lookup-only. Completion disabled."))
      
      ;; search each string in the list
      (setq strlist (if (stringp string) (list string) string))
      (while strlist
	(setq str (pop strlist))
	
	
        ;; look in completion cache first
	(setq cache (if (dic-completion-speed dic)
			(gethash str (dic-completion-hash dic))
		      nil))
	
	;; if we've found a cached result with enough completions...
	(if (and cache (or (null (setq cachenum (cache-num cache)))
			   (and (not (null maxnum)) (<= maxnum cachenum))))
	    (progn
	      (setq vect (cache-vect cache))
	      ;; drop any excess cached completions
	      (when (and maxnum (> (length vect) maxnum))
		(setq vect (subseq vect 0 maxnum))))
	  
	  ;; if nothing was in the cache or the cached result contained fewer
	  ;; completions than asked for, look in the ternary search tree and
	  ;; time it
	  (setq time (float-time))
	  (setq vect (tstree-complete (dic-tstree dic) str maxnum))
	  (setq time (- (float-time) time))
	  ;; if the completion function was slower than the dictionary's
	  ;; completion speed, add the results to the completion hash and set
	  ;; the dictionary's modified flag
	  (when (and (setq speed (dic-completion-speed dic))
		     (or (eq speed t) (> time speed)))
	    (dic-set-modified dic t)
	    (puthash str (cache-create vect maxnum)
		     (dic-completion-hash dic)))
	  )
	
	;; if ALL is set, add string to the fronts of the completions
	(when all
	  (setq vect (map 'vector
			  (lambda (s) (cons (concat str (car s)) (cdr s)))
			  vect)))
	;; merge the cached completions with those already found
	(setq completions (merge 'vector completions vect rankfun))
	;; drop any excess completions
	(when (and maxnum (> (length completions) maxnum))
	  (setq completions (subseq completions 0 maxnum)))
	))
    ;; return the completions vector, or nil if none were found
    (if (= 0 (length completions)) nil completions))
)





(defun dict-complete-ordered (dict string
				   &optional maxnum all rank-function)
  "Return a vector containing all completions of STRING found in dictionary
DICT. Each element of the returned vector is a cons containing the completed
string and its associated data. If no completions are found, return nil.

Note that `dict-complete' is significantly more efficient than
`dict-complete-ordered', especially when a maximum number of completions is
specified. Always use `dict-complete' when you don't care about the ordering
of the completions, or you need the completions ordered alphabetically.

DICT can also be a list of dictionaries, in which case completions are sought
in all trees in the list. If RANK-FUCTION is ot specified, the rank function
of the first dictionary in the list is used. All the dictionaries' rank
functions had better be compatible, otherwise at best you will get unexpected
results, at worst errors.

STRING must either be a single string, or a list of strings. If a list is
supplied, completions of all elements of the list are included in the returned
vector.

The optional numerical argument MAXNUM limits the results to the \"best\"
MAXNUM completions. If nil, all completions are returned.

Normally, only the remaining characters needed to complete STRING are
returned. If the optional argument ALL is non-nil, the entire completion is
returned.

The optional argument RANK-FUNCTION over-rides the dictionary's default rank
function. It should take two arguments, each a cons whose car is a vector
referencing data in the tree, and whose cdr is the data at that reference. It
should return non-nil if the first argument is \"better than\" the second, nil
otherwise. The elements of the returned vector are sorted according to this
rank-function, in descending order."
  
  (let ((dictlist (if (dict-p dict) (list dict) dict)) dic)
    
    ;; if the default rank function has been over-ridden, look in the ternary
    ;; search tree since we don't cache non-default rank functions
    (if rank-function
	(let (treelist)
	  (while dictlist
	    (setq dic (pop dictlist))
            ;; better check that none of the dictionaries in the list are
	    ;; lookup-only
	    (when (dic-lookup-only dic)
	      (error "Dictionary is lookup-only. Completion disabled."))
	    (setq treelist (append (dic-tstree dic) treelist)))
	  ;; search the ternary search tree
	  (tstree-complete-ordered treelist string maxnum all rank-function))
      
      
      ;; if we're using the dictionary's default rank-function...
      (let* ((rankfun (dic-rankfun (car dictlist)))
	     (completions [])
	     strlist str
	     cache vect cachenum
	     time speed)
    
        ;; search each dictionary in the list
	(while dictlist
	  (setq dic (pop dictlist))
	  
          ;; throw a wobbly if dictionary is lookup-only
	  (when (dic-lookup-only dic)
	    (error "Dictionary is lookup-only. Completion disabled."))
	  
          ;; search each string in the list
	  (setq strlist (if (stringp string) (list string) string))
	  (while strlist
	    (setq str (pop strlist))
	    
	    
            ;; look in completion cache first
	    (setq cache (if (dic-ordered-speed dic)
			    (gethash str (dic-ordered-hash dic))
			  nil))
	    
	    ;; if we've found a cached result with enough completions...
	    (if (and cache (or (null (setq cachenum (cache-num cache)))
			       (and (not (null maxnum)) (<= maxnum cachenum))))
		(progn
		  (setq vect (cache-vect cache))
	          ;; drop any excess cached completions
		  (when (and maxnum (> (length vect) maxnum))
		    (setq vect (subseq vect 0 maxnum))))
	      
	      ;; if nothing was in the cache or the cached result didn't
	      ;; contain enough compleitons, search tree and time the search
	      (setq time (float-time))
	      (setq vect (tstree-complete-ordered
			  (dic-tstree dic) str maxnum))
	      (setq time (- (float-time) time))
	      ;; if the completion function was slower than the dictionary's
	      ;; completion speed, add the results to the completion hash and
	      ;; set the dictionary's modified flag
	      (when (and (setq speed (dic-ordered-speed dic))
			 (or (eq speed t) (> time speed)))
		(dic-set-modified dic t)
		(puthash str (cache-create vect maxnum)
			 (dic-ordered-hash dic))))
	    
	    ;; if ALL is set, add string to the fronts of the completions
	    (when all
	      (setq vect (vector (mapcar vect
		      (lambda (s) (cons (concat str (car s)) (cdr s)))))))
	    ;; merge the cached completions with those already found
	    (setq completions (merge 'vector completions vect rankfun))
	    ;; drop any excess completions
	    (when (and maxnum (> (length completions) maxnum))
	      (setq completions (subseq completions 0 maxnum)))
	    ))
	
        ;; return the completions vector, or nil if none were found
	(if (= 0 (length completions)) nil completions)
      )))
)




(defun dict-populate-from-file (dict file)
  "Populate dictionary DICT from the word list in file FILE. Each line of the
file should contain a word, delimeted by \"\". Use the escape sequence \\\" to
include a \" in the string. If a line does not contain a delimeted string, it
is silently ignored. The words should ideally be sorted alphabetically.

Each line can also include data to be associated with the word, separated from
the word by whitespace. Anything after the whitespace is considered
data. String data should be \"\"-delimited, and must be on a single
line. However, the escape sequence \"\\n\" can be used to include a newline,
the escape sequence \\\" can again be used to include a \", and the escape
sequence \\\\ must be used to include a \\.


Technicalities:

The word and data can actually be separated by any character that is not a
word-constituent according to the standard syntax table. However, you're safe
if you stick to whitespace.

The data is read as a lisp expression and evaluated, so can be more complex
than a simple constant. However, it must be entirely on one line. The symbol
_word can be used to refer to the word associated with the data.

The word list is read from the middle outwards, i.e. first the middle word is
read, then the word directly after it, then the word directly before it, then
the one two lines after the middle, and so on. Assuming the words in the file
are sorted alphabetically, this helps produce a reasonably efficient
dictionary. However, it may have implications if the data is a lisp expression
that has side-effects."
  
  (save-excursion
    (let ((buff (generate-new-buffer " *dict-populate*")))
      ;; insert the word list into a temporary buffer
      (set-buffer buff)
      (insert-file-contents file)
      
      ;; insert the words starting from the median to ensure a well-balanced
      ;; tree
      (let* ((lines (count-lines (point-min) (point-max)))
	     (midpt (+ (/ lines 2) (mod lines 2)))
	     entry)
        ;; insert the median word and set the dictionary's modified flag
	(goto-line midpt)
	(when (setq entry (dict-read-line))
	  (dict-insert dict (car entry) (cdr entry))
	  (dic-set-modified dict t))
	(message "Inserting word 1 of %d..." lines)
        ;; insert words successively further away from the median in both
        ;; directions
	(dotimes (i (1- midpt))
	  (goto-line (+ midpt i 1))
	  (when (setq entry (dict-read-line))
	    (dict-insert dict (car entry) (cdr entry)))
	  (when (= 49 (mod i 50))
	    (message "Inserting word %d of %d..." (+ (* 2 i) 2) lines))
	  (goto-line (- midpt i 1))
	  (when (setq entry (dict-read-line))
	    (dict-insert dict (car entry) (cdr entry))))
	
        ;; if file contains an even number of words, we still have to add
        ;; the last one
	(when (= 0 (mod lines 2))
	  (goto-line lines)
	  (when (setq entry (dict-read-line))
	    (dict-insert dict (car entry) (cdr entry))))
	(message "Inserting word %d of %d...done" lines lines))
      
      (kill-buffer buff)))
)



;;; FIXME: doesn't fail gracefully if file has invalid format
(defun dict-read-line ()
  ;; Return a cons containing the word and data (if any, otherwise 0) at the
  ;; current line of the current buffer. Returns nil if line is in wrong
  ;; format.
  
  (save-excursion
    (let (data)
      ;; search for text between quotes "", ignoring escaped quotes \"
      (beginning-of-line)
      (setq _word (read (current-buffer)))
      ;; if there is anything after the quoted text, use it as data
      (if (eq (line-end-position) (point))
	  (cons _word nil)
	(setq data (eval (read (current-buffer))))
	;; return the word and data
	(cons _word data))))
)




(defun dict-save (dict)
  "Save dictionary DICT to it's associated file. Use `dict-write' to save to a
different file."
  (interactive "SDictionary to save: ")
  (when (interactive-p) (setq dict (eval dict)))
  
  (let* ((filename (dic-filename dict)))
    
    ;; if dictionary has no associated file, prompt for one
    (unless (and filename (> (length filename) 0))
      (setq filename (read-file-name (format "No file associated with \
dictionary %s. Write to file: " (dic-name dict)))))
    
    ;; save dictionary without requiring confirmation
    (dict-write dict filename t))
)




(defun dict-write (dict filename &optional overwrite uncompiled)
  "Write dictionary DICT to file FILENAME.
If optional argument OVERWRITE is non-nil, no confirmation will be asked for
before overwriting an existing file. If optional argument UNCOMPILED is set,
an uncompiled copy of the dictionary will be created."
  (interactive "SDictionary to write: \nFFile to write to: \nP")
  (when (interactive-p) (setq dict (eval dict)))

  (let* (dictname  ; saved dictionary name is constructed from the filename
	 (autosave (dic-autosave dict))
	 (lookup-only (dic-lookup-only dict))
	 lookup-speed completion-speed ordered-speed
	 tmpdict lookup-alist completion-alist ordered-alist
	 hashcode buff tmpfile)
    
    ;; add .el(c) extension to the filename if not already there
    (if uncompiled
	(unless (string= (substring filename -3) ".el")
	  (setq filename (concat filename ".el")))
      (unless (string= (substring filename -4) ".elc")
	(setq filename (concat filename ".elc"))))
    ;; remove .el(c) extension from filename to create saved dictionary name
    (setq dictname (if uncompiled
		       (substring (file-name-nondirectory filename) 0 -3)
		     (substring (file-name-nondirectory filename) 0 -4)))
    
    (save-excursion
      ;; create a temporary file
      (setq buff (find-file-noselect
		  (setq tmpfile (make-temp-file "dict-save"))))
      (set-buffer buff)
      
      ;; if the dictionary is lookup only, dump the lookup cache to an alist
      (if lookup-only
	  (progn
	    (maphash (lambda (key val) (push (cons key val) lookup-alist))
		     (dic-lookup-hash dict))
	    ;; generate code to reconstruct the lookup hash table
	    (setq hashcode
		  (concat
		   "(let ((lookup-hash (make-hash-table :test 'equal)))"
		   "  (mapcar (lambda (entry)"
		   "    (puthash (car entry) (cdr entry) lookup-hash))"
                   "    (dic-lookup-hash " dictname "))"
		   "  (dic-set-lookup-hash " dictname " lookup-hash)"))
	    ;; generate the structure to save
	    (setq tmpdict (list 'DICT dictname filename autosave
				(dic-insfun dict) lookup-only lookup-alist)))
	
	
	;; otherwise, dump caches to alists as necessary and generate code to
	;; reonstruct the hash tables from the alists
	(setq lookup-speed (dic-lookup-speed dict)
	      completion-speed (dic-completion-speed dict)
	      ordered-speed (dic-ordered-speed dict))
	
	;; create the lookup alist, if necessaru
	(when lookup-speed
	  (maphash (lambda (key val) (push (cons key val) lookup-alist))
		   (dic-lookup-hash dict))
	  ;; generate code to reconstruct the lookup hash table
	  (setq hashcode
		(concat
		 hashcode
		 "(let ((lookup-hash (make-hash-table :test 'equal)))"
		 "  (mapcar (lambda (entry)"
		 "    (puthash (car entry) (cdr entry) lookup-hash)"
	         "    (dic-lookup-hash " dictname ")))"
		 "  (dic-set-lookup-hash " dictname " lookup-hash))"
		 "\n")))
	
	;; create the completion alist, if necessary
	(when completion-speed
	  (maphash (lambda (key val) (push (cons key val) completion-alist))
		   (dic-completion-hash dict))
	  ;; generate code to reconstruct the completion hash table
	  (setq hashcode
		(concat
		 hashcode
		 "(let ((completion-hash (make-hash-table :test 'equal)))"
		 "  (mapcar (lambda (entry)"
		 "    (puthash (car entry) (cdr entry) completion-hash)"
	         "    (dic-completion-hash " dictname ")))"
		 "  (dic-set-completion-hash " dictname " completion-hash))"
		 "\n")))
	
	;; create the ordered completion alist, if necessary
	(when ordered-speed
	  (maphash (lambda (key val) (push (cons key val) ordered-alist))
		   (dic-ordered-hash dict))
	  ;; generate code to reconstruct the ordered hash table
	  (setq hashcode
		(concat
		 hashcode
		 "(let ((ordered-hash (make-hash-table :test 'equal)))"
		 "  (mapcar (lambda (entry)"
		 "    (puthash (car entry) (cdr entry) ordered-hash))"
	         "    (dic-ordered-hash " dictname "))"
		 "  (dic-set-ordered-hash " dictname " ordered-hash))"
		 "\n")))
	
	;; generate the structure to save
	(setq tmpdict (list 'DICT dictname filename autosave nil
			    (dic-tstree dict) lookup-only
			    lookup-alist lookup-speed
			    completion-alist completion-speed
			    ordered-alist ordered-speed))
      )
      
      
      ;; write lisp code that generates the dictionary object
      (insert "(provide '" dictname ")\n")
      (insert "(require 'dict)\n")
      (insert "(setq " dictname " '" (prin1-to-string tmpdict) ")\n")
      (insert hashcode)      
      (insert "(push " dictname " dict-loaded-list)\n")
      (save-buffer)
      (kill-buffer buff)
      
      ;; byte-compile the code (unless uncompiled option is set) and move the
      ;; file to its final destination
      (if (or uncompiled (save-window-excursion (byte-compile-file tmpfile)))
	  (progn
	    (when (or (not (file-exists-p filename))
		      overwrite
		      (yes-or-no-p
		       (format "File %s already exists. Overwrite? "
			       filename)))
	      (if uncompiled
		  (rename-file tmpfile filename t)
		;; if writing a compiled version, associate dictionary with
		;; the new file and mark it as unmodified
		(rename-file (concat tmpfile ".elc") filename t)
		(dic-set-filename dict filename)
		(dic-set-modified dict nil)
		(delete-file tmpfile))
	      (message "Dictionary %s saved to %s" dictname filename)
	      t))  ; return t if dictionary was successfully saved
	;; if there were errors compiling, throw error
	(error "Error saving %s. Dictionary not saved" dictname))
      ))
)




(defun dict-save-modified (&optional ask)
  "Save all modified dictionaries. If optional argument ASK is non-nil, ask
for confirmation before saving."
  (interactive "P")
  ;; For each loaded dictionary, check if dictionary has been modified. If so,
  ;; save it if autosave is on
  (dolist (dict dict-loaded-list)
    (when (and (dic-modified dict)
	       (dic-autosave dict)
	       (or (not ask) (y-or-n-p (format "Save modified dictionary %s? "
					       (dic-filename dict)))))
      (dict-save dict)
      (dic-set-modified dict nil)))
)




(defun dict-load (file)
  "Load a dictionary object from file FILE. Returns t if successful, nil
otherwise."
  (interactive "fDictionary file to load: ")
  
  ;; sort out dictionary name and file name
  (let (dictname dict)
    (when (not (string= (substring file -4) ".elc"))
      (setq file (concat file ".elc")))
    (setq dictname (substring (file-name-nondirectory file) 0 -4))
    
    ;; load the dictionary
    (load file t)
    (setq dict (eval (intern-soft dictname)))
    (when (not (dict-p dict))
      (beep)
      (error "Error loading dictionary from %s" file))
    
    ;; ensure the dictionary name and file name associated with the dictionary
    ;; matche the file it was loaded from
    (dic-set-filename dict (expand-file-name file))
    (dic-set-name dict dictname)
    
    ;; make sure the dictionary is in dict-loaded-list (normally the lisp code
    ;; in the dictionary itself should do that)
    (unless (memq dict dict-loaded-list) (push dict dict-loaded-list))
    (message (format "Loaded dictionary %s" dictname)))
)




(defun dict-unload (dict)
  "Unload dictionary DICT."
  (interactive "SDictionary to unload: ")
  (when (interactive-p) (setq dict (eval dict)))
  
  ;; if dictionary has been modified and autosave is set, save it first
  (when (and (dic-modified dict)
	     (or (eq (dic-autosave dict) t)
	       (and (eq (dic-autosave dict) 'ask)
	         (y-or-n-p (format
			    "Dictionary %s modified. Save before unloading? "
			    dict)))))
    (dict-save dict)
    (dic-set-modified dict nil))
  
  ;; remove dictionary from list of loaded dictionaries and unload it
  (setq dict-loaded-list (delq dict dict-loaded-list))
  (unintern (dic-name dict))
)




;; Add the dict-save-modified function to the kill-emacs-hook to save modified
;; dictionaries when exiting emacs
(add-hook 'kill-emacs-hook 'dict-save-modified)



;;; dict.el ends here
