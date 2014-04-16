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
      :funarg
      ,@*special-operators*
      ,@(loop for symbol in *shadowed-functions-and-macros*
           collect (intern (symbol-name symbol) :keyword)))))

(defpackage :clomp-implementation
  (:use :cl))

(in-package :cl)

(clomp-system::define-clomp-package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :clomp)
    (named-readtables:defreadtable :clomp
      (:merge :standard)
      (:dispatch-macro-char #\# #\'
                            (lambda (stream subchar arg)
                              (declare (ignore subchar arg))
                              `(clomp:function ,(read stream t nil t)))))))
