(in-package :cl)

(in-package :clomp-system)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *shadowed-functions-and-macros*
    (loop for symbol being the symbols of :common-lisp
       when (fboundp symbol)
       collect symbol))
  
  (defparameter *special-operators*
    (list
     :block
     :catch
     :eval-when
     :flet
     :function
     :go
     :if
     :labels
     :let
     :let*
     :load-time-value
     :locally
     :macrolet
     :multiple-value-call
     :multiple-value-prog1
     :progn
     :progv
     :quote
     :return-from
     :setq
     :symbol-macrolet
     :tagbody
     :the
     :throw
     :unwind-protect)))

(defmacro define-clomp-package ()
  `(defpackage :clomp
     (:use :cl)
     (:shadow
      ,@*special-operators*
      ,@(loop for symbol in *shadowed-functions-and-macros*
           collect (intern (symbol-name symbol) :keyword)))
     (:export
      :evaluate
      :sexp
      :closure
      :static-closure
      :funarg
      :let-init-form
      :let-body
      ,@*special-operators*
      ,@(loop for symbol in *shadowed-functions-and-macros*
           collect (intern (symbol-name symbol) :keyword)))))

(defpackage :clomp
  (:use :cl)
  #.`(:shadow
      ,@(loop for symbol being the external-symbols of :common-lisp
           when (or (fboundp symbol)
                    (special-operator-p symbol))
           collect (intern (symbol-name symbol) :keyword)))
  
  #.`(:export
      :form
      :lexical-form
      :evaluate
      :sexp
      :closure
      :static-closure
      :static-layer
      :funarg
      :special-operator
      :macro
      :invocation
      :user-function
      :value
      :block-form
      :catch-body
      :eval-when-body
      :flet-body
      :if-test
      :if-then
      :if-else
      :labels-body
      :let-init-form
      :let-body
      :let*-init-form
      :let*-body
      :load-time-value-form
      :locally-body
      :macrolet-body
      :multiple-value-call-function
      :multiple-value-call-argument
      :multiple-value-prog1-values-form
      :multiple-value-prog1-forms
      :progn-forms
      :progv-symbols
      :progv-values
      :progv-forms
      :return-from-value
      :setq-form
      :symbol-macrolet-body
      :tagbody-form
      :the-form
      :throw-result
      :unwind-protect-protected
      :unwind-protect-cleanup
      
      :cond-test
      :cond-then
      
      ,@(remove-if (constantly nil);;(lambda (x) (member x *unshadowed-symbols*))
                   (loop for symbol being the external-symbols of :common-lisp
                      when (or (fboundp symbol)
                               (special-operator-p symbol))
                      collect (intern (symbol-name symbol) :keyword)))))

(defpackage :clomp-implementation
  (:use :cl :contextl))

(in-package :cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :clomp)
    (named-readtables:defreadtable :clomp
      (:merge :standard)
      (:dispatch-macro-char #\# #\'
                            (lambda (stream subchar arg)
                              (declare (ignore subchar arg))
                              `(clomp:function ,(read stream t nil t)))))))
