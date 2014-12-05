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

;; sb-kernel:lexenv also has :funs, :blocks: :tags, :type-restrictions, :lambda (reified source object),
;; :cleanup, :handled-conditions, :disabled-package-locks, :%policy, and :user-data
;; note that :funs includes macros -- functions are repr'd like (,name . #<function ...>), macros
;; like (,name macro . #<function ...>)
(defclass environment ()
  ((bindings
    :accessor bindings
    :initarg :bindings)
   (parent
    :accessor parent
    :initarg :parent)))

(defparameter *compile-time-environment* nil)




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

;; expansion-time layer
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
       :environment current-environment
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
      (or (and found env) (lookup-environment-aux var (parent env))))))

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
             (current-environment
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
             (current-environment
              (make-instance 'environment
               :bindings (list ,@(loop for var-name in var-names
                                    collect `(cons ',var-name ,var-name)))
               :parent current-environment)))
         ,@body))))


(defmacro with-extended-bindings ((binding-var var-names env) &body body)
  `(call-with-extended-bindings ',binding-var ,var-names ,env
                                ,@body))



(defmacro clomp-shadow:let (&whole whole-sexp bindings &body body &environment env)
  (let* (;; todo: handle declares and put them in the proper place in final expansion
         (listified-bindings (loop for binding in bindings
                                collect
                                  (if (atom binding)
                                      (list binding)
                                      binding)))
         (processed-bindings (loop for binding in bindings
                                collect
                                  (if (atom binding)
                                      binding
                                      `(,(first binding)
                                         (with-active-layers (let-init-form)
                                           ,(maybe-value (second binding)))))))
         (extended-body
          (with-extended-bindings (clomp-let-bindings
                                   (mapcar #'first listified-bindings) env)
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


;;;; OTHER MACROS

#+nil
(length
 '(ASSERT CALL-METHOD CASE CCASE CHECK-TYPE CTYPECASE DECLAIM
      DEFCLASS DEFCONSTANT DEFGENERIC DEFINE-COMPILER-MACRO DEFINE-CONDITION
      DEFINE-METHOD-COMBINATION DEFINE-MODIFY-MACRO DEFINE-SETF-EXPANDER
      DEFINE-SYMBOL-MACRO DEFMACRO DEFMETHOD DEFPACKAGE DEFPARAMETER DEFSETF
      DEFSTRUCT DEFTYPE DEFVAR DESTRUCTURING-BIND DO DO* DO-ALL-SYMBOLS
      DO-EXTERNAL-SYMBOLS DO-SYMBOLS DOLIST DOTIMES ECASE ETYPECASE FORMATTER
      HANDLER-BIND HANDLER-CASE IGNORE-ERRORS IN-PACKAGE LAMBDA LOOP
      LOOP-FINISH MULTIPLE-VALUE-BIND MULTIPLE-VALUE-LIST MULTIPLE-VALUE-SETQ
      NTH-VALUE PPRINT-EXIT-IF-LIST-EXHAUSTED PPRINT-LOGICAL-BLOCK
      PPRINT-POP PRINT-UNREADABLE-OBJECT PROG PROG* PROG1 PROG2
      RESTART-BIND RESTART-CASE RETURN STEP
      TIME TRACE TYPECASE UNLESS UNTRACE WHEN WITH-ACCESSORS
      WITH-COMPILATION-UNIT WITH-CONDITION-RESTARTS WITH-HASH-TABLE-ITERATOR
      WITH-INPUT-FROM-STRING WITH-OPEN-FILE WITH-OPEN-STREAM
      WITH-OUTPUT-TO-STRING WITH-PACKAGE-ITERATOR WITH-SIMPLE-RESTART WITH-SLOTS
      WITH-STANDARD-IO-SYNTAX))

(deflayer defparameter-val)

(defmacro clomp-shadow:defparameter (&whole whole-sexp var val &optional doc)
  `(evaluate
    (make-instance 'clomp-shadow:defparameter
     :sexp ',whole-sexp
     :closure (lambda ()
                (defparameter ,var (with-active-layers (defparameter-val) ,val) ,doc)))))

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

(deflayer and-form)

(defmacro clomp-shadow:and (&whole whole-sexp &rest forms)
  `(evaluate
    (make-instance 'clomp-shadow:and
     :sexp ',whole-sexp
     :closure (lambda ()
                (and
                 ,@(loop for form in forms
                        collect
                        `(with-active-layers (and-form)
                           ,(maybe-value form))))))))

(deflayer or-form)

(defmacro clomp-shadow:or (&whole whole-sexp &rest forms)
  `(evaluate
    (make-instance 'clomp-shadow:or
     :sexp ',whole-sexp
     :closure (lambda ()
                (or
                 ,@(loop for form in forms
                        collect
                        `(with-active-layers (or-form)
                           ,(maybe-value form))))))))

(deflayer function-body)

(defun internal-symbol (symbol)
  (let ((internal-package-name
         (format nil "CLOMP.PACKAGE.~A" (package-name (symbol-package symbol)))))
    (intern (princ-to-string symbol)
            (or 
             (find-package internal-package-name)
             (make-package internal-package-name)))))

(defmacro clomp-shadow:defun (&whole whole-sexp name args &body body &environment env)
  (let* ((internal-symbol (internal-symbol name))
         (macro-body
          ;; need same code transformation available this within the current form for
          ;; recursive calls, and elsewhere for nonrecursive calls
          ``(evaluate
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
                    :function-object (function ,',internal-symbol)
                    :function-args (list ,@(loop for param in ',args collect param))
                    :closure
                    (lambda ()
                      (,',internal-symbol
                       ,@(loop for param in ',args
                            collect param))))))
                #+nil
                (,',internal-symbol
                 ,@(loop for arg in (list ,@args)
                      collect
                        `(with-active-layers (funarg)
                           ,(maybe-value arg)))))
              :static-closure
              (lambda ()
                (list
                 ,@(loop for arg in (list ,@args)
                      collect (maybe-value arg))))))))
    `(progn
       ;; ensure we don't get complaints about package not existing when systems
       ;; are compiled/loaded in some weird order - does this make sense?
       (internal-symbol ',name)
       (defmacro ,name (&whole whole-sexp ,@args)
         ,macro-body)
       ,(let ((extended-body
               (with-extended-bindings (clomp-let-bindings args env)
                 `(evaluate
                   (make-instance 'frame
                                  :bindings clomp-let-bindings
                                  :closure
                                  (lambda ()
                                    ,@(with-active-layers (within-frame)
                                        ;; expansions further down tree rely on expansion-time dynamic context,
                                        ;; so we must force expansion with macroexpand-dammit to use that context
                                        ;; before it is thrown out on the next pass
                                        (macroexpand-dammit
                                         `(macrolet ((,name (&whole whole-sexp ,@args)
                                                       ,macro-body))
                                            ,(mapcar #'maybe-value body))))))))))
             
             ;; define actual function in special package
             `(defun ,internal-symbol ,args
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

;;;; 

(defgeneric record-call (parent child)
  (:method (parent child))
  (:method (parent (child function-call))
    (pushnew (class-name (class-of child)) (gethash parent *callers*)))
  (:method (parent (child user-function-call))
    (pushnew (first (sexp child)) (gethash parent *callers*))))
