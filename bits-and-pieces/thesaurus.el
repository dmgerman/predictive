
(defun thes-word ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((word (word-at-point)))
      (set-text-properties 0 (length word) nil word)
      word)))


(defun thes-lookup (word)
  (dictree-lookup dict-mthesaur word))


(define-completion-at-point-function
 mthesaur-completion-at-point thes-lookup
 :name thesaurus
 :prefix-function thes-word
 :non-prefix-completion t
 :no-auto-completion t)


(defun thes-create-dict ()
  (dictree-create 'dict-mthesaur "bits-and-pieces/dict-mthesaur.el" t)
  (dictree-populate-from-file
   dict-mthesaur "bits-and-pieces/mthesaur.word-list"))
