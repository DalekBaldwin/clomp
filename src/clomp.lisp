(cl:in-package :clomp)
(named-readtables:in-readtable :clomp)

(cl:defclass form ()
  ((sexp
    :accessor sexp
    :initarg :sexp)
   (closure
    :accessor closure
    :initarg :closure)))

(cl:defclass funarg (form) ())

(cl:defgeneric evaluate (form)
  (:method (form)
    (cl:funcall (closure form))))

(cl:defmacro funarg (&whole whole-sexp arg)
  (cl:declare (cl:ignorable whole-sexp))
  `(evaluate
    (cl:make-instance 'funarg
     :sexp ',arg
     :closure (cl:lambda () ,arg))))

(cl:defmacro define-closure-wrapper (symbol)
  (cl:let ((symbol-name (cl:symbol-name symbol)))
    (cl:multiple-value-bind (cl-symbol foundp)
        (cl:find-symbol symbol-name :common-lisp)
      (cl:if (cl:not foundp)
             (cl:error "Symbol ~A not in package COMMON-LISP" symbol-name)
             (cl:unless (cl:eql symbol cl-symbol)
               `(cl:progn
                  (cl:defclass ,symbol (form) ())
                  (cl:defmethod evaluate ((form ,symbol))
                    (cl:call-next-method))
                  (cl:defmacro ,symbol (&whole whole-sexp &rest args)
                    (cl:declare (cl:ignorable args))
                    `(evaluate
                      (cl:make-instance ',',symbol
                       :sexp ',whole-sexp
                       :closure (cl:lambda ()
                                  (,',cl-symbol
                                   ,@(cl:if
                                      (cl:or (cl:special-operator-p ',cl-symbol)
                                             (cl:macro-function ',cl-symbol))
                                      (cl:rest whole-sexp)
                                      (cl:loop :for arg :in args
                                               :collect `(funarg ,arg))))))))))))))

(cl:defmacro define-closure-wrappers (&rest symbols)
  (cl:if (cl:null symbols)
         `(cl:progn
            ,@(cl:loop :for symbol :in (cl:package-shadowing-symbols cl:*package*)
                       :collect `(define-closure-wrapper ,symbol)))
         `(cl:progn
            ,@(cl:loop :for symbol :in symbols
                       :collect `(define-closure-wrapper ,symbol)))))

(cl:defmacro define-closure-wrappers ()
  `(cl:progn
     ,@(cl:loop :for symbol :in (cl:package-shadowing-symbols cl:*package*)
                :collect `(define-closure-wrapper ,symbol))))

(define-closure-wrappers)

(cl:defmacro function (&whole whole-sexp thing)
  (cl:cond
    ((cl:and (cl:listp thing)
             (cl:eql (cl:first thing) 'lambda))
     `(evaluate
       (cl:make-instance 'function
        :sexp ',whole-sexp
        :closure (cl:lambda ()
                   (cl:function (cl:lambda ,@(cl:rest thing)))))))
    ((cl:and (cl:symbolp thing)
             (cl:multiple-value-bind (cl-symbol foundp)
                 (cl:find-symbol (cl:symbol-name thing) :common-lisp)
               (cl:and foundp (cl:fboundp cl-symbol))))
     `(evaluate
       (cl:make-instance 'function
        :sexp ',whole-sexp
        :closure (cl:lambda ()
                   (cl:function
                    ,(cl:find-symbol
                      (cl:symbol-name thing)
                      :common-lisp))))))
    (t
     `(evaluate
       (cl:make-instance 'function
        :sex ',whole-sexp
        :closure (cl:lambda () (cl:function ,@(cl:rest whole-sexp))))))))
