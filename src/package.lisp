(in-package :cl)

(in-package :clomp-system)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *shadowed-functions*
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
      ,@(loop for symbol in *shadowed-functions*
           collect (intern (symbol-name symbol) :keyword)))
     (:export
      :evaluate
      :sexp
      :closure
      ,@*special-operators*
      ,@(loop for symbol in *shadowed-functions*
           collect (intern (symbol-name symbol) :keyword)))))

(in-package :cl)

(clomp-system::define-clomp-package)

(in-package :clomp)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (named-readtables:find-readtable :clomp)
    (named-readtables:defreadtable :clomp
      (:merge :standard)
      (:dispatch-macro-char #\# #\'
                            (cl:lambda (stream subchar arg)
                              (declare (ignore subchar arg))
                              `(function ,(cl:read stream t nil t)))))))
