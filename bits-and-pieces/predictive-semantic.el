


(require 'semantic-ctxt)
(require 'cl)

(provide 'predictive-semantic)


(defun predictive-semantic-generate-function-dict (overlay)
  "Generate a dictionary containing function arguments for OVERLAY."

  (let (arguments dict)
    ;; if there are local function arguments, and add them to a new dictionary
    ;; and return it
    (when (setq arguments
		(mapcar 'car
			(semantic-get-local-arguments (overlay-end overlay))))
      (setq dict (predictive-create-dict "local-args"))
      (mapc (lambda (arg) (dict-insert dict arg)) arguments)
      
      ;; store list of arguments in overlay
      (overlay-put overlay 'words arguments)
      
      ;; return the new dictionary
      dict))
)



(defun predictive-semantic-refresh-function-dict (overlay)
  "Refresh OVERLAY's function argument dictionary."

  (let ((dict (plist-get (overlay-get overlay 'dict) :dict))
	newargs oldargs diffargs)
		 
    ;; get local function arguments, and add them to a new dictionary
    (setq oldargs (overlay-get overlay 'words))
    (setq newargs
	  (mapcar 'car (semantic-get-local-arguments (overlay-end overlay))))
    ;; if new arguments have been added, add them to the dictionary
    (when (setq diffargs (set-difference newargs oldargs))
      (mapc (lambda (arg) (dict-insert dict arg)) diffargs))
    ;; if old arguments have been deleted, remove them from the dictionary
    (when (setq diffargs (set-difference oldargs newargs))
      (mapc (lambda (arg) (dict-delete dict arg)) diffargs))

    ;; store new argument list in overlay
    (overlay-put overlay 'words newargs)
    
    ;; return nil to indicate dictionary doesn't need regenerating
    nil)
)



(defun predictive-semantic-generate-variable-dict (overlay)
  "Generate a dictionary containing local variables for OVERLAY."

  (let (variables dict)
    ;; if there are local function arguments, and add them to a new dictionary
    ;; and return it
    (when (setq variables
		(mapcar 'car
			(semantic-get-local-variables (overlay-end overlay))))
      (setq dict (predictive-create-dict "local-vars"))
      (mapc (lambda (var) (dict-insert dict var)) variables)
      
      ;; store list of variables in overlay
      (overlay-put overlay 'words variables)
      
      ;; return the new dictionary
      dict))
)



(defun predictive-semantic-refresh-variable-dict (overlay)
  "Refresh OVERLAY's local variable dictionary."

  (let ((dict (plist-get (overlay-get overlay 'dict) :dict))
	newvars oldvars diffvars)
		 
    ;; get local function arguments, and add them to a new dictionary
    (setq oldvars (overlay-get overlay 'words))
    (setq newvars
	  (mapcar 'car (semantic-get-local-variables (overlay-end overlay))))
    ;; if new arguments have been added, add them to the dictionary
    (when (setq diffvars (set-difference newvars oldvars))
      (mapc (lambda (var) (dict-insert dict var)) diffvars))
    ;; if old arguments have been deleted, remove them from the dictionary
    (when (setq diffvars (set-difference oldvars newvars))
      (mapc (lambda (var) (dict-delete dict var)) diffvar))

    ;; store new argument list in overlay
    (overlay-put overlay 'words newvars)
    
    ;; return nil to indicate dictionary doesn't need regenerating
    nil)
)



(defun predictive-semantic-generate-dict (overlay)
  "Generate a dictionary containing local variables for OVERLAY."

  (let (variables dict)
    ;; if there are local function arguments, and add them to a new dictionary
    ;; and return it
    (when (setq variables
		(semantic-get-all-local-variables (overlay-end overlay)))
      (setq variables (concat (mapcar 'car (car variables))
			      (mapcar 'car (cdr variables))))
      (setq dict (predictive-create-dict "local-vars"))
      (mapc (lambda (var) (dict-insert dict var)) variables)
      
      ;; store list of variables in overlay
      (overlay-put overlay 'words variables)
      
      ;; return the new dictionary
      dict))
)



(defun predictive-semantic-refresh-dict (overlay)
  "Refresh OVERLAY's local variable dictionary."

  (let ((dict (plist-get (overlay-get overlay 'dict) :dict))
	newvars oldvars diffvars)
		 
    ;; get local function arguments, and add them to a new dictionary
    (setq oldvars (overlay-get overlay 'words))
    (setq newvars (semantic-get-local-variables (overlay-end overlay)))
    (setq newvars (concat (mapcar 'car (car variables))
			  (mapcar 'car (cdr variables))))
    ;; if new arguments have been added, add them to the dictionary
    (when (setq diffvars (set-difference newvars oldvars))
      (mapc (lambda (var) (dict-insert dict var)) diffvars))
    ;; if old arguments have been deleted, remove them from the dictionary
    (when (setq diffvars (set-difference oldvars newvars))
      (mapc (lambda (var) (dict-delete dict var)) diffvar))

    ;; store new argument list in overlay
    (overlay-put overlay 'words newvars)
    
    ;; return nil to indicate dictionary doesn't need regenerating
    nil)
)

