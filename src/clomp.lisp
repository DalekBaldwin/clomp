(in-package :clomp-implementation)

(defclass clomp:form ()
  ((clomp:sexp
    :accessor clomp:sexp
    :initarg :sexp)
   (clomp:closure
    :accessor clomp:closure
    :initarg :closure)
   (clomp:static-closure
    :accessor clomp:static-closure
    :initarg :static-closure)))



(defclass clomp:lexical-form (clomp:form) ())
(defclass clomp:special-operator (clomp:lexical-form) ())
(defclass clomp:macro (clomp:lexical-form) ())
(defclass clomp:invocation (clomp:lexical-form) ())
(defclass clomp:user-function (clomp:invocation) ())
(defclass clomp:value (clomp:lexical-form) ())



(define-layered-function clomp:evaluate (form)
  (:method ((form clomp:form))
    (funcall (clomp:closure form))))

(deflayer clomp:static-layer)

(define-layered-method clomp:evaluate
  :in-layer clomp:static-layer
  :around ((form clomp:form))
  (append (list form)
        (funcall (clomp:static-closure form))))

(deflayer clomp:funarg)

(define-closure-wrappers)

(defmacro clomp:function (&whole whole-sexp thing)
  (let ((wrapped-function-form
         (cond
           ((and (listp thing)
                 (or
                  ;; this should work regardless of whether the user has shadow
                  ;; shadow imported our lambda wrapper
                  (eql (first thing) 'lambda)
                  (eql (first thing) 'clomp:lambda)))
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

(deflayer clomp:block-form)

(defmacro clomp:block (&whole whole-sexp name &rest forms)
  `(clomp:evaluate
    (make-instance 'clomp:block
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (block ,name
         ,@(loop for form in forms
              collect `(with-active-layers (clomp:block-form)
                         ,form)))))))

(deflayer clomp:catch-body)

(defmacro clomp:catch (&whole whole-sexp tag &body body)
  `(clomp:evaluate
    (make-instance 'clomp:catch
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (catch ,tag
         (with-active-layers (clomp:catch-body) ,@body))))))

(deflayer clomp:eval-when-body)

(defmacro clomp:eval-when (&whole whole-sexp (&rest situations) &rest body)
  `(eval-when ,situations
     (clomp:evaluate
      (make-instance 'clomp:eval-when
       :sexp ',whole-sexp
       :closure
       (lambda ()
         (with-active-layers (clomp:eval-when-body)
           ,@body))))))

(deflayer clomp:flet-body)

(defmacro clomp:flet (&whole whole-sexp definitions &body body)
  `(clomp:evaluate
    (make-instance 'clomp:flet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (flet ,definitions
         (with-active-layers (clomp:flet-body)
           ,@body))))))

;; nothing to do...
#+nil
(defmacro clomp:go (&whole whole-sexp tag))

#+nil
(defclass clomp:if-test (clomp:form) ())

#+nil
(defmacro clomp::if-test (form)
  `(clomp:evaluate
    (make-instance 'clomp:if-test
     :sexp '(_if-test_ ,form)
     :closure (lambda () ,form))))

#+nil
(defclass clomp:if-then (clomp:form) ())

#+nil
(defmacro clomp::if-then (form)
  `(clomp:evaluate
    (make-instance 'clomp:if-then
     :sexp '(_if-then_ ,form)
     :closure (lambda () ,form))))

#+nil
(defclass clomp:if-else (clomp:form) ())

#+nil
(defmacro clomp::if-else (form)
  `(clomp:evaluate
    (make-instance 'clomp:if-else
     :sexp '(_if-else_ ,form)
     :closure (lambda () ,form))))

#+nil
(defmacro clomp:if (&whole whole-sexp test then &optional else)
  `(clomp:evaluate
    (make-instance 'clomp:if
     :sexp ',whole-sexp
     :closure (lambda ()
                (if (clomp::if-test ,test)
                    (clomp::if-then ,then)
                    (clomp::if-else ,else))))))

(deflayer clomp:if-test)
(deflayer clomp:if-then)
(deflayer clomp:if-else)

(defmacro clomp:if (&whole whole-sexp test then &optional else)
  `(clomp:evaluate
    (make-instance 'clomp:if
     :sexp ',whole-sexp
     :closure (lambda ()
                (if (with-active-layers (clomp:if-test) ,(maybe-value test))
                    (with-active-layers (clomp:if-then) ,(maybe-value then))
                    (with-active-layers (clomp:if-else) ,(maybe-value else))))
     :static-closure (lambda ()
                       (list
                        (with-active-layers (clomp:if-test) ,(maybe-value test))
                        (with-active-layers (clomp:if-then) ,(maybe-value then))
                        (with-active-layers (clomp:if-else) ,(maybe-value else)))))))

(deflayer clomp:labels-body)

(defmacro clomp:labels (&whole whole-sexp definitions &body body)
  `(clomp:evaluate
    (make-instance 'clomp:labels
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (labels ,definitions
         (with-active-layers (clomp:labels-body)
           ,@body))))))

#+nil
(defclass clomp:let-init-form (clomp:form) ())

#+nil
(defmacro clomp::let-init-form (form)
  `(clomp:evaluate
    (make-instance 'clomp:let-init-form
     :sexp ',form
     :closure (lambda () ,form))))

#+nil
(defclass clomp:let-body (clomp:form) ())

#+nil
(defmacro clomp::let-body (&body body)
  `(clomp:evaluate
    (make-instance 'clomp:let-body
     :sexp '(_implicit-progn_ ,@body)
     :closure (lambda ()
                ,@body))))

#+nil
(defmacro clomp:let (&whole whole-sexp bindings &body body)
  `(clomp:evaluate
    (make-instance 'clomp:let
     :sexp ',whole-sexp
     :closure (lambda ()
                (let (,@(loop for binding in bindings
                           collect `(,(first binding)
                                      (clomp::let-init-form
                                        ,(second binding)))))
                  (clomp::let-body ,@body))))))

(deflayer clomp:let-init-form)
(deflayer clomp:let-body)

(defmacro clomp:let (&whole whole-sexp bindings &body body)
  `(clomp:evaluate
    (make-instance 'clomp:let
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (let (,@(loop for binding in bindings
                  collect
                    (if (atom binding)
                        binding
                        `(,(first binding)
                           (with-active-layers (clomp:let-init-form)
                             ,(second binding))))))
         (with-active-layers (clomp:let-body) ,@body))))))

(deflayer clomp:let*-init-form)
(deflayer clomp:let*-body)

(defmacro clomp:let* (&whole whole-sexp bindings &body body)
  `(clomp:evaluate
    (make-instance 'clomp:let*
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (let* (,@(loop for binding in bindings
                   collect
                     (if (atom binding)
                         binding
                         `(,(first binding)
                            (with-active-layers (clomp:let*-init-form)
                              ,(second binding))))))
         (with-active-layers (clomp:let*-body) ,@body))))))

(deflayer clomp:load-time-value-form)

(defmacro clomp:load-time-value (&whole whole-sexp form &optional read-only-p)
  `(clomp:evaluate
    (make-instance 'clomp:load-time-value
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (load-time-value ,form ,read-only-p)))))

(deflayer clomp:locally-body)


;; ... what to do?
#+nil
(defmacro clomp:locally (&whole whole-sexp &body body))

(deflayer clomp:macrolet-body)

(defmacro clomp:macrolet (&whole whole-sexp definitions &rest body)
  `(clomp:evaluate
    (make-instance 'clomp:macrolet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (macrolet ,definitions
         (with-active-layers (clomp:macrolet-body)
           ,@body))))))

(deflayer clomp:multiple-value-call-function)
(deflayer clomp:multiple-value-call-argument)

(defmacro clomp:multiple-value-call (&whole whole-sexp function &rest arguments)
  `(clomp:evaluate
    (make-instance 'clomp:multiple-value-call
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (multiple-value-call
           (with-active-layers (clomp:multiple-value-call-function)
             ,function)
         ,@(loop for argument in arguments
                collect `(with-active-layers (clomp:multiple-value-call-argument)
                           ,argument)))))))

(deflayer clomp:multiple-value-prog1-values-form)
(deflayer clomp:multiple-value-prog1-forms)

(defmacro clomp:multiple-value-prog1 (&whole whole-sexp values-form &rest forms)
  `(clomp:evaluate
    (make-instance 'clomp:multiple-value-prog1
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (multiple-value-prog1
           (with-active-layers (clomp:multiple-value-prog1-values-form)
             ,values-form)
         (with-active-layers (clomp:multiple-value-prog1-forms)
           ,@forms))))))

(deflayer clomp:progn-forms)

(defmacro clomp:progn (&whole whole-sexp &rest forms)
  `(clomp:evaluate
    (make-instance 'clomp:progn
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (with-active-layers (clomp:progn-forms)
         ,@forms)))))

(deflayer clomp:progv-symbols)
(deflayer clomp:progv-values)
(deflayer clomp:progv-forms)

(defmacro clomp:progv (&whole whole-sexp vars vals &body body)
  `(clomp:evaluate
    (make-instance 'clomp:progv
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (progv
           (with-active-layers (clomp:progv-symbols)
             ,vars)
           (with-active-layers (clomp:progv-values)
             ,vals)
         (with-active-layers (clomp:progv-forms)
           ,@body))))))

;; nothing to do here?
#+nil
(defmacro clomp:quote (&whole whole-sexp thing))

(deflayer clomp:return-from-value)

(defmacro clomp:return-from (&whole whole-sexp name &optional value)
  `(clomp:evaluate
    (make-instance 'clomp:return-from
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (return-from ,name
         (with-active-layers (clomp:return-from-value)
           ,value))))))

(deflayer clomp:setq-form)

(defmacro clomp:setq (&whole whole-sexp &rest things)
  `(clomp:evaluate
    (make-instance 'clomp:setq
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (setq
        ,@(loop for (var form) on things by #'cddr
             collect var
             collect `(with-active-layers (clomp:setq-form)
                        ,form)))))))

(deflayer clomp:symbol-macrolet-body)

(defmacro clomp:symbol-macrolet (&whole whole-sexp macrobindings &body body)
  `(clomp:evaluate
    (make-instance 'clomp:symbol-macrolet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (symbol-macrolet ,macrobindings
         (with-active-layers (clomp:symbol-macrolet-body)
           ,@body))))))

(deflayer clomp:tagbody-form)

(defmacro clomp:tagbody (&whole whole-sexp &rest statements)
  `(clomp:evaluate
    (make-instance 'clomp:tagbody
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (tagbody
          ,@(loop for statement in statements
                 collect
                 (if (atom statement)
                     statement
                     `(with-active-layers (clomp:tagbody-form)
                        ,statement))))))))

(deflayer clomp:the-form)

(defmacro clomp:the (&whole whole-sexp value-type form)
  `(clomp:evaluate
    (make-instance 'clomp:the
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (the ,value-type
            (with-active-layers (clomp:the-form)
              ,form))))))

(deflayer clomp:throw-result)

(defmacro clomp:throw (&whole whole-sexp tag result)
  `(clomp:evaluate
    (make-instance 'clomp:throw
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (throw ,tag (with-active-layers (clomp:throw-result)
                     ,result))))))

(deflayer clomp:unwind-protect-protected)
(deflayer clomp:unwind-protect-cleanup)

(defmacro clomp:unwind-protect (&whole whole-sexp protected &body cleanup)
  `(clomp:evaluate
    (make-instance 'clomp:unwind-protect
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (unwind-protect
            (with-active-layers (clomp:unwind-protect-protected)
              ,protected)
         (with-active-layers (clomp:unwind-protect-cleanup)
           ,@cleanup))))))



;;;; MODIFY-MACROS

(define-simple-wrapper clomp:setf (&rest args)
  `(setf ,@(loop for (place form) on args by #'cddr
              collect (sanitize-accessor place)
              collect form)))

(define-simple-wrapper clomp:psetf (&rest args)
  `(psetf ,@(loop for (place form) on args by #'cddr
               collect (sanitize-accessor place)
               collect form)))

#+nil
(defmacro clomp:psetq (&whole whole-sexp &rest things)
  `(clomp:evaluate
    (make-instance 'clomp:psetq
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (psetq
        ,@(loop for (var form) on things by #'cddr
             collect var
             collect form))))))

(define-simple-wrapper clomp:incf (place &optional (delta 1))
  `(incf ,(sanitize-accessor place) ,delta))

(define-simple-wrapper clomp:decf (place &optional (delta 1))
  `(decf ,(sanitize-accessor place) ,delta))

(define-simple-wrapper clomp:remf (place indicator)
  `(remf ,(sanitize-accessor place) ,indicator))

(define-simple-wrapper clomp:rotatef (&rest args)
  `(rotatef ,@(mapcar #'sanitize-accessor args)))

(define-simple-wrapper clomp:shiftf (&rest args)
  `(shiftf ,@(mapcar #'sanitize-accessor (butlast args)) ,@(last args)))

(define-simple-wrapper clomp:push (obj place)
  `(push ,obj ,(sanitize-accessor place)))

(define-simple-wrapper clomp:pushnew (obj place &rest keys)
  `(pushnew ,obj ,(sanitize-accessor place) ,@keys))

(define-simple-wrapper clomp:pop (place)
  `(pop ,(sanitize-accessor place)))


;;;;

(deflayer clomp:cond-test)
(deflayer clomp:cond-then)

(defmacro clomp:cond (&whole whole-sexp &rest clauses)
  `(clomp:evaluate
    (make-instance 'clomp:cond
     :sexp ',whole-sexp
     :closure (lambda ()
                (cond
                  ,@(loop for clause in clauses
                         collect 
                         `((with-active-layers (clomp:cond-test)
                             ,(maybe-value (first clause)))
                           (with-active-layers (clomp:cond-then)
                             ,(maybe-value (second clause)))))))
     :static-closure (lambda ()
                       (list
                        ,@(loop for clause in clauses
                               collect `(with-active-layers (clomp:cond-test)
                                          ,(maybe-value (first clause)))
                               collect `(with-active-layers (clomp:cond-then)
                                          ,(maybe-value (second clause)))))))))

(defmacro clomp::defun* (&whole whole-sexp name args &body body)
  `(defun ,name ,args
     (clomp:evaluate
      (make-instance 'clomp:user-function
       :sexp (list ',name ,@args)
       :closure
       (lambda ()
         ,@body)))))

#+nil
(defmacro clomp:defpackage (package &rest options)
  (let ((clomp-package-symbol (intern format nil "CLOMP.PACKAGE.~A" (symbol-name package))))
    `(defpackage ,clomp-package-symbol
       ,@options)))

(defmacro clomp:defun (&whole whole-sexp name args &body body)
  (let* (;;(internal-name (intern (format nil "+~A-INTERNAL+" (symbol-name name))))
         (internal-package-name (format nil "CLOMP.PACKAGE.~A" (package-name (symbol-package name))))
         (real-symbol (intern (format nil "~A" name)
                              (or 
                               (find-package internal-package-name)
                               (make-package internal-package-name)))))
    `(progn
       (defmacro ,name (&whole whole-sexp ,@args)
         `(clomp:evaluate
           (make-instance 'clomp:user-function
            :sexp ',whole-sexp
            :closure
            (lambda ()
              
              (,',real-symbol
               ,@(loop for arg in (list ,@args)
                    collect
                      `(with-active-layers (clomp:funarg)
                         ,(maybe-value arg)))))
            :static-closure
            (lambda ()
              (list
               ,@(loop for arg in (list ,@args)
                      collect (maybe-value arg)))))))
       (defun ,real-symbol ,args
         ,@body)

       (let ((static-stuff
              (list
               ,@(loop for thing in body
                      collect `(with-active-layers (clomp:static-layer)
                                 ,thing)))))
         (dft static-stuff
              (lambda (stuff)
                (record-call ',name stuff))))
       
       ;; defun should return function name as symbol.
       ;; should it be the original or the internal symbol?
       ',name
       )))

(defgeneric record-call (parent child)
  (:method (parent child))
  (:method (parent (child clomp:invocation))
    (pushnew (class-name (class-of child)) (gethash parent *callers*)))
  (:method (parent (child clomp:user-function))
    (pushnew (first (clomp:sexp child)) (gethash parent *callers*))))
