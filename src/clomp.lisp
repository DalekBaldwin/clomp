(in-package :clomp)

(defclass continuation ()
  ((closure
    :accessor closure
    :initarg :closure)))

(defclass form (continuation)
  ((sexp
    :accessor sexp
    :initarg :sexp)
   (static-closure
    :accessor static-closure
    :initarg :static-closure)))

(defclass lexical-form (form) ())

(defclass special-operator (lexical-form) ())
(defclass macro (lexical-form) ())
(defclass function-call (lexical-form) ())

(defclass user-function-call (function-call) ())

(defclass value (lexical-form)
  ((environment
    :accessor environment
    :initarg :environment)))

(defclass frame (continuation)
  ((bindings
    :accessor bindings
    :initarg :bindings)))

(defclass environment ()
  ((bindings
    :accessor bindings
    :initarg :bindings)
   (parent
    :accessor parent
    :initarg :parent)))

(defparameter *compile-time-initial-environment* (make-instance 'environment
                                                                :bindings nil
                                                                :parent nil))
(defparameter *compile-time-environment* *compile-time-initial-environment*)




(defclass invocation (continuation)
  ((function-object
    :accessor function-object
    :initarg :function-object)
   (function-args
    :accessor function-args
    :initarg :function-args)))

;; should the closure be a slot of the object, or passed as a separate argument to evaluate??
(define-layered-function evaluate (continuation)
  (:method ((continuation continuation))
    (funcall (closure continuation))))

(deflayer static-layer)

(define-layered-method evaluate
  :in static-layer
  :around ((form continuation))
  (append (list form)
        (funcall (static-closure form))))

(deflayer within-frame)

(define-layered-function box-value (arg)
  (:method (arg)
    `(evaluate
      (make-instance 'value
       :sexp ',arg
       :environment nil
       :closure
       (lambda () ,arg)
       :static-closure
       (lambda () (list ',arg)))))
  (:method :in within-frame (arg)
    `(evaluate
      (make-instance 'value
       :sexp ',arg
       :environment environment
       :closure
       (lambda () ,arg)
       :static-closure
       (lambda () (list ',arg))))))

(defun maybe-value (arg) ;; pass macro-time env too??
  (let (;;(let-in-package-p (find 'clomp-shadow:let (package-shadowing-symbols *package*)))
        )
    (if (atom arg)
        (box-value arg)
        arg)))

(defun lookup-environment-aux (var env)
  (unless (null env)
    (let ((found (member var (bindings env) :key #'car)))
      (or (and found env) (lookup var (parent env))))))

(defgeneric lookup-environment (form)
  (:method ((form value))
    (lookup-environment-aux (sexp form) (environment form))))

(deflayer funarg)

#.`(progn
     ,@(loop for symbol in (package-shadowing-symbols (find-package :clomp-shadow))
          collect `(define-closure-wrapper ,symbol)))

(defmacro clomp-shadow:function (&whole whole-sexp thing)
  ;; todo: wrap symbol lookup
  (let ((wrapped-function-form
         (cond
           ((and (listp thing)
                 (or
                  ;; this should work regardless of whether the user has
                  ;; shadow imported our lambda wrapper
                  (eql (first thing) 'lambda)
                  (eql (first thing) 'clomp-shadow:lambda)))
            `(function (lambda ,@(rest thing))))
           ((and (symbolp thing)
                 (multiple-value-bind (cl-symbol foundp)
                     (find-symbol (symbol-name thing) :common-lisp)
                   (and foundp (fboundp cl-symbol))))
            `(function ,(find-symbol (symbol-name thing) :common-lisp)))
           (t
            `(function ,@(rest whole-sexp))))))
    `(evaluate
      (make-instance 'clomp-shadow:function
       :sexp ',whole-sexp
       :closure (lambda () ,wrapped-function-form)))))

(deflayer block-form)

(defmacro clomp-shadow:block (&whole whole-sexp name &rest forms)
  `(evaluate
    (make-instance 'clomp-shadow:block
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (block ,name
         ,@(loop for form in forms
              collect `(with-active-layers (block-form) ;; or just one context for the whole body?
                         ,(maybe-value form))))))))

(deflayer catch-body)

(defmacro clomp-shadow:catch (&whole whole-sexp tag &body body)
  `(evaluate
    (make-instance 'clomp-shadow:catch
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (catch ,tag
         (with-active-layers (catch-body) ,@(mapcar #'maybe-value body)))))))

(deflayer eval-when-body)

(defmacro clomp-shadow:eval-when (&whole whole-sexp (&rest situations) &rest body)
  `(eval-when ,situations
     (evaluate
      (make-instance 'clomp-shadow:eval-when
       :sexp ',whole-sexp
       :closure
       (lambda ()
         (with-active-layers (eval-when-body)
           ;; maybe-value might not make sense at some eval-times...
           ,@(mapcar #'maybe-value body)))))))

(deflayer flet-body)

(defmacro clomp-shadow:flet (&whole whole-sexp definitions &body body)
  `(evaluate
    (make-instance 'clomp-shadow:flet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (flet ,definitions
         (with-active-layers (flet-body)
           ,@(mapcar #'maybe-value body)))))))

;; nothing to do...
#+nil
(defmacro clomp-shadow:go (&whole whole-sexp tag))

(deflayer if-test)
(deflayer if-then)
(deflayer if-else)

(defmacro clomp-shadow:if (&whole whole-sexp test then &optional else)
  `(evaluate
    (make-instance 'clomp-shadow:if
     :sexp ',whole-sexp
     :closure (lambda ()
                (if (with-active-layers (if-test) ,(maybe-value test))
                    (with-active-layers (if-then) ,(maybe-value then))
                    (with-active-layers (if-else) ,(maybe-value else))))
     :static-closure (lambda ()
                       (list
                        (with-active-layers (if-test) ,(maybe-value test))
                        (with-active-layers (if-then) ,(maybe-value then))
                        (with-active-layers (if-else) ,(maybe-value else)))))))

(deflayer labels-body)

(defmacro clomp-shadow:labels (&whole whole-sexp definitions &body body)
  `(evaluate
    (make-instance 'clomp-shadow:labels
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (labels ,definitions
         (with-active-layers (labels-body)
           ,@(mapcar #'maybe-value body)))))))

(deflayer let-init-form)
(deflayer let-body)

(define-layered-function call-with-extended-bindings (binding-var var-names env &rest body)
  (:method (binding-var var-names env &rest body)
    (let ((*compile-time-environment*
           (make-instance 'environment
            :bindings var-names
            :parent *compile-time-environment*)))
      `(let ((,binding-var
              (list (list
                     ,@(loop for var-name in var-names
                          collect `(cons ',var-name ,var-name))
                     ;;',*compile-time-environment*
                     )
                    ;;',(active-layers)
                    ))
             (environment
              (make-instance 'environment
                             :bindings (list ,@(loop for var-name in var-names
                                                  collect `(cons ',var-name ,var-name)))
                             :parent nil)))
         ,@body)))
  (:method :in within-frame (binding-var var-names env &rest body)
    (let ((*compile-time-environment*
           (make-instance 'environment
                          :bindings var-names
                          :parent *compile-time-environment*)))
      `(let ((,binding-var
              (cons (list ,@(loop for var-name in var-names
                               collect `(cons ',var-name ,var-name))
                          ;;',*compile-time-environment*
                          )
                    ,binding-var))
             (environment
              (make-instance 'environment
               :bindings (list ,@(loop for var-name in var-names
                                    collect `(cons ',var-name ,var-name)))
               :parent environment)))
         ,@body))))


(defmacro with-extended-bindings ((binding-var var-names env) &body body)
  `(call-with-extended-bindings ',binding-var ,var-names ,env
                                ,@body))

(defmacro clomp-shadow:let (&whole whole-sexp bindings &body body &environment env)
  (let* (;; todo: handle declares and put them in the proper place in final expansion
         (processed-bindings (loop for binding in bindings
                                collect
                                  (if (atom binding)
                                      binding
                                      `(,(first binding)
                                         (with-active-layers (let-init-form)
                                           ,(maybe-value (second binding)))))))
         (extended-body
          (with-extended-bindings ( clomp-let-bindings (mapcar #'first bindings) env)
            `(evaluate
              (make-instance 'frame
               :bindings clomp-let-bindings
               :closure
               (lambda ()
                 ,@(with-active-layers (within-frame)
                     ;;(macroexpand-dammit (mapcar #'maybe-value body)) ;; why did this work?
                     (mapcar #'macroexpand-dammit (mapcar #'maybe-value body)))))))))
    `(evaluate
      (make-instance 'clomp-shadow:let
                     :sexp ',whole-sexp
                     :closure
                     (lambda ()
                       (let (,@processed-bindings)
                         (with-active-layers (let-body)
                           ,extended-body)))))))

(deflayer let*-init-form)
(deflayer let*-body)

(defmacro clomp-shadow:let* (&whole whole-sexp bindings &body body)
  `(evaluate
    (make-instance 'clomp-shadow:let*
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (let* (,@(loop for binding in bindings
                   collect
                     (if (atom binding)
                         binding
                         `(,(first binding)
                            (with-active-layers (let*-init-form)
                              ,(second binding))))))
         (with-active-layers (let*-body) ,@(mapcar #'maybe-value body)))))))

(deflayer load-time-value-form)

(defmacro clomp-shadow:load-time-value (&whole whole-sexp form &optional read-only-p)
  `(evaluate
    (make-instance 'clomp-shadow:load-time-value
     :sexp ',whole-sexp
     :closure
     (lambda ()
       ;; should we maybe-value this?
       (load-time-value ,form ,read-only-p)))))

(deflayer locally-body)


;; ... what to do?
#+nil
(defmacro clomp-shadow:locally (&whole whole-sexp &body body))

(deflayer macrolet-body)

(defmacro clomp-shadow:macrolet (&whole whole-sexp definitions &rest body)
  `(evaluate
    (make-instance 'clomp-shadow:macrolet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (macrolet ,definitions
         (with-active-layers (macrolet-body)
           ,@(mapcar #'maybe-value body)))))))

(deflayer multiple-value-call-function)
(deflayer multiple-value-call-argument)

(defmacro clomp-shadow:multiple-value-call (&whole whole-sexp function &rest arguments)
  `(evaluate
    (make-instance 'clomp-shadow:multiple-value-call
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (multiple-value-call
           (with-active-layers (multiple-value-call-function)
             ,function)
         ,@(loop for argument in arguments
                collect `(with-active-layers (multiple-value-call-argument)
                           ,(maybe-value argument))))))))

(deflayer multiple-value-prog1-values-form)
(deflayer multiple-value-prog1-forms)

(defmacro clomp-shadow:multiple-value-prog1 (&whole whole-sexp values-form &rest forms)
  `(evaluate
    (make-instance 'clomp-shadow:multiple-value-prog1
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (multiple-value-prog1
           (with-active-layers (multiple-value-prog1-values-form)
             ,(maybe-value values-form))
         (with-active-layers (multiple-value-prog1-forms)
           ,@(mapcar #'maybe-value forms)))))))

(deflayer progn-forms)

(defmacro clomp-shadow:progn (&whole whole-sexp &rest forms)
  `(evaluate
    (make-instance 'clomp-shadow:progn
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (with-active-layers (progn-forms)
         ,@(mapcar #'maybe-value forms))))))

(deflayer progv-symbols)
(deflayer progv-values)
(deflayer progv-forms)


;; how do we set up the environment in this form? which forms should be maybe-valued?
(defmacro clomp-shadow:progv (&whole whole-sexp vars vals &body body)
  `(evaluate
    (make-instance 'clomp-shadow:progv
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (progv
           (with-active-layers (progv-symbols)
             ,vars)
           (with-active-layers (progv-values)
             ,vals)
         (with-active-layers (progv-forms)
           ,@body))))))

;; nothing to do here?
#+nil
(defmacro clomp-shadow:quote (&whole whole-sexp thing))

(deflayer return-from-value)

(defmacro clomp-shadow:return-from (&whole whole-sexp name &optional (value nil value-p))
  `(evaluate
    (make-instance 'clomp-shadow:return-from
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (return-from ,name
         (with-active-layers (return-from-value)
           ,(if value-p
                (maybe-value value)
                value)))))))

(deflayer setq-form)

(defmacro clomp-shadow:setq (&whole whole-sexp &rest things)
  `(evaluate
    (make-instance 'clomp-shadow:setq
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (setq
        ,@(loop for (var form) on things by #'cddr
             collect var
             collect `(with-active-layers (setq-form)
                        ,(maybe-value form))))))))

(deflayer symbol-macrolet-body)

(defmacro clomp-shadow:symbol-macrolet (&whole whole-sexp macrobindings &body body)
  `(evaluate
    (make-instance 'clomp-shadow:symbol-macrolet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (symbol-macrolet ,macrobindings
         (with-active-layers (symbol-macrolet-body)
           ;; I don't know how to handle symbol macros with maybe-value...
           ,@body))))))

(deflayer tagbody-form)

(defmacro clomp-shadow:tagbody (&whole whole-sexp &rest statements)
  `(evaluate
    (make-instance 'clomp-shadow:tagbody
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (tagbody
          ,@(loop for statement in statements
                 collect
                 (if (atom statement)
                     statement
                     `(with-active-layers (tagbody-form)
                        ,statement))))))))

(deflayer the-form)

(defmacro clomp-shadow:the (&whole whole-sexp value-type form)
  `(evaluate
    (make-instance 'clomp-shadow:the
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (the ,value-type
            (with-active-layers (the-form)
              ,(maybe-value form)))))))

(deflayer throw-result)

(defmacro clomp-shadow:throw (&whole whole-sexp tag result)
  `(evaluate
    (make-instance 'clomp-shadow:throw
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (throw ,tag (with-active-layers (throw-result)
                     ,(maybe-value result)))))))

(deflayer unwind-protect-protected)
(deflayer unwind-protect-cleanup)

(defmacro clomp-shadow:unwind-protect (&whole whole-sexp protected &body cleanup)
  `(evaluate
    (make-instance 'clomp-shadow:unwind-protect
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (unwind-protect
            (with-active-layers (unwind-protect-protected)
              ,(maybe-value protected))
         (with-active-layers (unwind-protect-cleanup)
           ,@(mapcar #'maybe-value cleanup)))))))



;;;; MODIFY-MACROS

(define-simple-wrapper clomp-shadow:setf (&rest args)
  `(setf ,@(loop for (place form) on args by #'cddr
              collect (sanitize-accessor place)
              collect (maybe-value form))))

(define-simple-wrapper clomp-shadow:psetf (&rest args)
  `(psetf ,@(loop for (place form) on args by #'cddr
               collect (sanitize-accessor place)
               collect (maybe-value form))))

(defmacro clomp-shadow:psetq (&whole whole-sexp &rest things)
  `(evaluate
    (make-instance 'clomp-shadow:psetq
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (psetq
        ,@(loop for (var form) on things by #'cddr
             collect var
             collect (maybe-value form)))))))

(define-simple-wrapper clomp-shadow:incf (place &optional (delta 1))
  `(incf ,(sanitize-accessor place) ,delta))

;; probably should trace delta lookup only if supplied. this is about
;; what is lexically apparent in the original code, after all
#+nil
(define-simple-wrapper clomp-shadow:incf (place &optional (delta 1 delta-p))
  `(incf ,(sanitize-accessor place)
         ,(if delta-p (maybe-value delta)
              delta)))

(define-simple-wrapper clomp-shadow:decf (place &optional (delta 1))
  `(decf ,(sanitize-accessor place) ,delta))

(define-simple-wrapper clomp-shadow:remf (place indicator)
  `(remf ,(sanitize-accessor place) ,indicator))

(define-simple-wrapper clomp-shadow:rotatef (&rest args)
  `(rotatef ,@(mapcar #'sanitize-accessor args)))

(define-simple-wrapper clomp-shadow:shiftf (&rest args)
  `(shiftf ,@(mapcar #'sanitize-accessor (butlast args)) ,@(last args)))

(define-simple-wrapper clomp-shadow:push (obj place)
  `(push ,obj ,(sanitize-accessor place)))

(define-simple-wrapper clomp-shadow:pushnew (obj place &rest keys)
  `(pushnew ,obj ,(sanitize-accessor place) ,@keys))

(define-simple-wrapper clomp-shadow:pop (place)
  `(pop ,(sanitize-accessor place)))


;;;;

(deflayer cond-test)
(deflayer cond-then)

(defmacro clomp-shadow:cond (&whole whole-sexp &rest clauses)
  `(evaluate
    (make-instance 'clomp-shadow:cond
     :sexp ',whole-sexp
     :closure (lambda ()
                (cond
                  ,@(loop for clause in clauses
                         collect 
                         `((with-active-layers (cond-test)
                             ,(maybe-value (first clause)))
                           (with-active-layers (cond-then)
                             ,(maybe-value (second clause)))))))
     :static-closure (lambda ()
                       (list
                        ,@(loop for clause in clauses
                               collect `(with-active-layers (cond-test)
                                          ,(maybe-value (first clause)))
                               collect `(with-active-layers (cond-then)
                                          ,(maybe-value (second clause)))))))))

(deflayer function-body)

(defmacro clomp-shadow:defun (&whole whole-sexp name args &body body &environment env)
  (let* (;;(internal-name (intern (format nil "+~A-INTERNAL+" (symbol-name name))))
         (internal-package-name (format nil "CLOMP.PACKAGE.~A" (package-name (symbol-package name))))
         (real-symbol (intern (format nil "~A" name)
                              (or 
                               (find-package internal-package-name)
                               (make-package internal-package-name)))))
    `(progn
       (defmacro ,name (&whole whole-sexp ,@args)
         `(evaluate
           (make-instance 'user-function-call
            :sexp ',whole-sexp
            :closure
            (lambda ()
              (let (,@(loop for arg in (list ,@args)
                           for param in ',args
                         collect `(,param (with-active-layers (funarg)
                                            ,(maybe-value arg)))))
                
                ;; added additional step for possibility of introspection
                ;; after arguments are evaluated
                (evaluate
                 (make-instance 'invocation
                                ;; need to change this if we're going to support anonymous functions too
                                :function-object (function ,',real-symbol)
                                :function-args (list ,@(loop for param in ',args collect param))
                                :closure
                                (lambda ()
                                  (,',real-symbol
                                   ,@(loop for param in ',args
                                          collect param))))))
              #+nil
              (,',real-symbol
               ,@(loop for arg in (list ,@args)
                    collect
                      `(with-active-layers (funarg)
                         ,(maybe-value arg)))))
            :static-closure
            (lambda ()
              (list
               ,@(loop for arg in (list ,@args)
                      collect (maybe-value arg)))))))
       ,(let ((extended-body
               (with-extended-bindings (clomp-let-bindings args env)
                 `(evaluate
                   (make-instance 'frame
                                  :bindings clomp-let-bindings
                                  :closure
                                  (lambda ()
                                    ,@(with-active-layers (within-frame)
                                        (macroexpand-dammit (mapcar #'maybe-value body)))))))))
             `(defun ,real-symbol ,args
                (with-active-layers (function-body)
                  ,extended-body)))
       
       #+nil
       (let ((static-stuff
              (list
               ,@(loop for thing in body
                      collect `(with-active-layers (static-layer)
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
  (:method (parent (child function-call))
    (pushnew (class-name (class-of child)) (gethash parent *callers*)))
  (:method (parent (child user-function-call))
    (pushnew (first (sexp child)) (gethash parent *callers*))))
