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

(defclass value (lexical-form) ())

(defclass frame (continuation)
  ((bindings
    :accessor bindings
    :initarg :bindings)))

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
  :in-layer static-layer
  :around ((form continuation))
  (append (list form)
        (funcall (static-closure form))))

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
              collect `(with-active-layers (block-form)
                         ,form)))))))

(deflayer catch-body)

(defmacro clomp-shadow:catch (&whole whole-sexp tag &body body)
  `(evaluate
    (make-instance 'clomp-shadow:catch
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (catch ,tag
         (with-active-layers (catch-body) ,@body))))))

(deflayer eval-when-body)

(defmacro clomp-shadow:eval-when (&whole whole-sexp (&rest situations) &rest body)
  `(eval-when ,situations
     (evaluate
      (make-instance 'clomp-shadow:eval-when
       :sexp ',whole-sexp
       :closure
       (lambda ()
         (with-active-layers (eval-when-body)
           ,@body))))))

(deflayer flet-body)

(defmacro clomp-shadow:flet (&whole whole-sexp definitions &body body)
  `(evaluate
    (make-instance 'clomp-shadow:flet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (flet ,definitions
         (with-active-layers (flet-body)
           ,@body))))))

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
           ,@body))))))

#+nil
(defclass let-init-form (form) ())

#+nil
(defmacro :let-init-form (form)
  `(evaluate
    (make-instance 'let-init-form
     :sexp ',form
     :closure (lambda () ,form))))

#+nil
(defclass let-body (form) ())

#+nil
(defmacro :let-body (&body body)
  `(evaluate
    (make-instance 'let-body
     :sexp '(_implicit-progn_ ,@body)
     :closure (lambda ()
                ,@body))))

#+nil
(defmacro let (&whole whole-sexp bindings &body body)
  `(evaluate
    (make-instance 'let
     :sexp ',whole-sexp
     :closure (lambda ()
                (let (,@(loop for binding in bindings
                           collect `(,(first binding)
                                      (:let-init-form
                                        ,(second binding)))))
                  (:let-body ,@body))))))

(deflayer let-init-form)
(deflayer let-body)

(defun expands-in-environment-p (form env)
  (second (multiple-value-list (macroexpand-1 form env))))

(defmacro clomp-shadow:let (&whole whole-sexp bindings &body body &environment env)
  `(evaluate
    (make-instance 'clomp-shadow:let
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (let (,@(loop for binding in bindings
                  collect
                    (if (atom binding)
                        binding
                        `(,(first binding)
                           (with-active-layers (let-init-form)
                             ,(maybe-value (second binding)))))))
         (let ((clomp-let-bindings
                ,(if (expands-in-environment-p 'clomp-frame-established env)
                     `(cons (list
                             ,@(loop for binding in bindings
                                  collect `(cons ',(first binding) ,(first binding))))
                            clomp-let-bindings)
                     `(list (list
                             ;; we can query the package the macro is expanded in...
                             ;; can probably query which CL symbols are shadowed to
                             ;; decide whether to strip out unneeded overhead at expand-time
                             ;;',*package*
                             ,@(loop for binding in bindings
                                  collect `(cons ',(first binding) ,(first binding))))))))
           (symbol-macrolet ((clomp-frame-established 't))
             (evaluate
              (make-instance 'frame
               ;; This binding structure won't intercept later set/gets, but
               ;; we can track what environment we are in and verify that later
               ;; changes to variables are associated with the same symbols and
               ;; update this representation accordingly
               :bindings clomp-let-bindings
               
               ;;(list ,@(loop for binding in bindings
               ;;                     collect `(cons ',(first binding)
               ;;                                    ,(first binding))))
               :closure
               (lambda ()
                 (with-active-layers (let-body)
                   ,@body)))))))
       
       ;;(let (,@(loop for binding in bindings
       ;;           collect
       ;;             (if (atom binding)
       ;;                 binding
       ;;                 `(,(first binding)
       ;;                    (with-active-layers (let-init-form)
       ;;                      ,(second binding))))))
         
       ;;  (with-active-layers (let-body) ,@body))
       ))))

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
         (with-active-layers (let*-body) ,@body))))))

(deflayer load-time-value-form)

(defmacro clomp-shadow:load-time-value (&whole whole-sexp form &optional read-only-p)
  `(evaluate
    (make-instance 'clomp-shadow:load-time-value
     :sexp ',whole-sexp
     :closure
     (lambda ()
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
           ,@body))))))

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
                           ,argument)))))))

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
             ,values-form)
         (with-active-layers (multiple-value-prog1-forms)
           ,@forms))))))

(deflayer progn-forms)

(defmacro clomp-shadow:progn (&whole whole-sexp &rest forms)
  `(evaluate
    (make-instance 'clomp-shadow:progn
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (with-active-layers (progn-forms)
         ,@forms)))))

(deflayer progv-symbols)
(deflayer progv-values)
(deflayer progv-forms)

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

(defmacro clomp-shadow:return-from (&whole whole-sexp name &optional value)
  `(evaluate
    (make-instance 'clomp-shadow:return-from
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (return-from ,name
         (with-active-layers (return-from-value)
           ,value))))))

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
                        ,form)))))))

(deflayer symbol-macrolet-body)

(defmacro clomp-shadow:symbol-macrolet (&whole whole-sexp macrobindings &body body)
  `(evaluate
    (make-instance 'clomp-shadow:symbol-macrolet
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (symbol-macrolet ,macrobindings
         (with-active-layers (symbol-macrolet-body)
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
              ,form))))))

(deflayer throw-result)

(defmacro clomp-shadow:throw (&whole whole-sexp tag result)
  `(evaluate
    (make-instance 'clomp-shadow:throw
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (throw ,tag (with-active-layers (throw-result)
                     ,result))))))

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
              ,protected)
         (with-active-layers (unwind-protect-cleanup)
           ,@cleanup))))))



;;;; MODIFY-MACROS

(define-simple-wrapper clomp-shadow:setf (&rest args)
  `(setf ,@(loop for (place form) on args by #'cddr
              collect (sanitize-accessor place)
              collect form)))

(define-simple-wrapper clomp-shadow:psetf (&rest args)
  `(psetf ,@(loop for (place form) on args by #'cddr
               collect (sanitize-accessor place)
               collect form)))

#+nil
(defmacro clomp-shadow:psetq (&whole whole-sexp &rest things)
  `(evaluate
    (make-instance 'clomp-shadow:psetq
     :sexp ',whole-sexp
     :closure
     (lambda ()
       (psetq
        ,@(loop for (var form) on things by #'cddr
             collect var
             collect form))))))

(define-simple-wrapper clomp-shadow:incf (place &optional (delta 1))
  `(incf ,(sanitize-accessor place) ,delta))

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

(defmacro defun* (&whole whole-sexp name args &body body)
  `(defun ,name ,args
     (evaluate
      (make-instance 'user-function-call
       :sexp (list ',name ,@args)
       :closure
       (lambda ()
         ,@body)))))

(defmacro clomp-shadow:defun (&whole whole-sexp name args &body body)
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
       (defun ,real-symbol ,args
         ,@body)

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
