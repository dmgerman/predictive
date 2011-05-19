
(require 'completion-ui)


(eval-and-compile
  (defun dummy-completion-function (prefix &optional maxnum)
    '("there" "the" "their" "they're" "thallium")))


(completion-ui-register-source
 'dummy-completion-function
 :name 'dummy)
