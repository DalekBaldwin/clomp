(in-package :clomp-implementation)

(defclass clomp::form ()
  ((clomp:sexp
    :accessor clomp:sexp
    :initarg :sexp)
   (clomp:closure
    :accessor clomp:closure
    :initarg :closure)))

(defclass clomp:funarg (clomp::form) ())

(defgeneric clomp:evaluate (form)
  (:method (form)
    (funcall (clomp:closure form))))

(defmacro clomp:funarg (&whole whole-sexp arg)
  (declare (ignorable whole-sexp))
  `(clomp:evaluate
    (make-instance 'clomp:funarg
     :sexp ',arg
     :closure (lambda () ,arg))))

(defmacro define-closure-wrapper (symbol)
  (let ((symbol-name (symbol-name symbol)))
    (multiple-value-bind (cl-symbol foundp)
        (find-symbol symbol-name :common-lisp)
      (if (not foundp)
             (error "Symbol ~A not in package COMMON-LISP" symbol-name)
             (unless (eql symbol cl-symbol)
               `(progn
                  (defclass ,symbol (clomp::form) ())
                  (defmacro ,symbol (&whole whole-sexp &rest args)
                    (declare (ignorable args))
                    `(clomp:evaluate
                      (make-instance ',',symbol
                       :sexp ',whole-sexp
                       :closure (lambda ()
                                  (,',cl-symbol
                                   ,@,(if
                                       (or (special-operator-p cl-symbol)
                                           (macro-function cl-symbol))
                                       `(rest whole-sexp)
                                       `(loop :for arg :in args
                                           :collect `(clomp:funarg ,arg))))))))))))))

(defmacro define-closure-wrappers ()
  `(progn
     ,@(loop :for symbol :in (package-shadowing-symbols (find-package :clomp))
                :collect `(define-closure-wrapper ,symbol))))

(define-closure-wrappers)

(defmacro clomp:function (&whole whole-sexp thing)
  (let ((wrapped-function-form
            (cond
              ((and (listp thing)
                       (eql (first thing) 'lambda))
               `(function (lambda ,@(rest thing))))
              ((and (symbolp thing)
                       (multiple-value-bind (cl-symbol foundp)
                           (find-symbol (symbol-name thing) :common-lisp)
                         (and foundp (fboundp cl-symbol))))
               `(function ,(find-symbol (symbol-name thing) :common-lisp)))
              (t
               `(function ,@(rest whole-sexp))))))
    `(clomp:evaluate
      (make-instance 'clomp:function
       :sexp ',whole-sexp
       :closure (lambda () ,wrapped-function-form)))))
