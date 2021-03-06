(in-package :cl-user)

(in-package :clomp-system)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *shadowed-functions-and-macros*
    (loop for symbol being the symbols of :common-lisp
       when (fboundp symbol)
       collect symbol))
  
  (defparameter *special-operators*
    '(#:block
      #:catch
      #:eval-when
      #:flet
      #:function
      #:go
      #:if
      #:labels
      #:let
      #:let*
      #:load-time-value
      #:locally
      #:macrolet
      #:multiple-value-call
      #:multiple-value-prog1
      #:progn
      #:progv
      #:quote
      #:return-from
      #:setq
      #:symbol-macrolet
      #:tagbody
      #:the
      #:throw
      #:unwind-protect)))

(defmacro define-clomp-package ()
  `(defpackage :clomp
     (:use :cl)
     (:shadow
      ,@*special-operators*
      ,@(loop for symbol in *shadowed-functions-and-macros*
           collect symbol))
     (:export
      #:evaluate
      #:sexp
      #:closure
      #:static-closure
      #:funarg
      #:let-init-form
      #:let-body
      ,@*special-operators*
      ,@(loop for symbol in *shadowed-functions-and-macros*
           collect (make-symbol (symbol-name symbol))))))

(defpackage :clomp-shadow
  (:use :cl)
  #.`(:shadow
      ,@(loop for symbol being the external-symbols of :common-lisp
           when (or (fboundp symbol)
                    (special-operator-p symbol))
           collect (make-symbol (symbol-name symbol))))
  #.`(:export
      ,@(loop for symbol being the external-symbols of :common-lisp
           when (or (fboundp symbol)
                    (special-operator-p symbol))
           collect (make-symbol (symbol-name symbol)))))

(defpackage :clomp
  (:use :cl :contextl :macroexpand-dammit.clomp-patch)
  #.`(:export
      #:continuation
      #:closure
      #:form
      #:sexp
      #:static-closure
      #:lexical-form
      #:special-operator
      #:macro
      #:function-call
      #:user-function-call
      #:value
      #:environment
      #:frame
      #:bindings
      #:invocation
      #:function-object
      #:function-args
      #:static-layer
      #:evaluate
      #:lookup-environment
      #:funarg
      #:block-form
      #:catch-body
      #:eval-when-body
      #:flet-body
      #:if-test
      #:if-then
      #:if-else
      #:labels-body
      #:let-init-form
      #:let-body
      #:let*-init-form
      #:let*-body
      #:load-time-value-form
      #:locally-body
      #:macrolet-body
      #:multiple-value-call-function
      #:multiple-value-call-argument
      #:multiple-value-prog1-values-form
      #:multiple-value-prog1-forms
      #:progn-forms
      #:progv-symbols
      #:progv-values
      #:progv-forms
      #:return-from-value
      #:setq-form
      #:symbol-macrolet-body
      #:tagbody-form
      #:the-form
      #:throw-result
      #:unwind-protect-protected
      #:unwind-protect-cleanup
      
      #:defparameter-val
      #:cond-test
      #:cond-then
      #:and-form
      #:or-form
      
      #:function-path-graph))

(defpackage :clomp-implementation
  (:use :cl :contextl))

(in-package :cl-user)

(unless (named-readtables:find-readtable :clomp)
  (named-readtables:defreadtable :clomp
    (:merge :standard)
    (:dispatch-macro-char #\# #\'
                          (lambda (stream subchar arg)
                            (declare (ignore subchar arg))
                            `(clomp-shadow:function ,(read stream t nil t))))))
