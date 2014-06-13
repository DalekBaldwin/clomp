(in-package :clomp)

(defun sanitize-accessor (form)
  (cond ((atom form) form)
        ((and (nth-value 1 (find-symbol
                            (symbol-name (first form))
                            :common-lisp))
              (member (symbol-package (first form))
                      (list (find-package :clomp-shadow)
                            (find-package :common-lisp))))
         `(,(find-symbol (symbol-name (first form)) :common-lisp)
            ,@(rest form)))
        (t form)))

(defun wrap-context (symbol body)
  (cond
    ((special-operator-p symbol)
     #+nil
     ``(with-active-layers (special-operator)
         ,,body)
     body)
    ((macro-function symbol)
     #+nil
     ``(with-active-layers (macro)
         ,,body)
     body)
    (t
     #+nil
     ``(with-active-layers (invocation)
         ,,body)
     body)))

(defun maybe-value (arg ;;&optional env
                           )
  (let ((let-in-package-p
         (find 'clomp-shadow:let (package-shadowing-symbols *package*))))
    (if (atom arg)
        `(evaluate
          (make-instance 'value
           :sexp ',arg
           :closure
           (lambda () ,arg)
           :static-closure
           (lambda () (list ',arg))))
        arg)))

(defmacro define-closure-wrapper (symbol)
  (let ((symbol-name (symbol-name symbol)))
    (multiple-value-bind (cl-symbol foundp)
        (find-symbol symbol-name :common-lisp)
      (if (not foundp)
             (error "Symbol ~A not in package COMMON-LISP" symbol-name)
             (unless (eql symbol cl-symbol)
               `(progn
                  (defclass ,symbol
                      (,(cond
                         ((special-operator-p cl-symbol)
                          `special-operator)
                         ((macro-function cl-symbol)
                          `macro)
                         (t
                          `function-call))) ())
                  (defmacro ,symbol (&whole whole-sexp &rest args)
                    (declare (ignorable args))
                    ,(wrap-context cl-symbol
                      ``(evaluate
                         (make-instance ',',symbol
                          :sexp ',whole-sexp
                          :closure
                          (lambda ()
                            (,',cl-symbol
                             ,@,(if
                                 (or (special-operator-p cl-symbol)
                                     (macro-function cl-symbol))
                                 `(rest whole-sexp)
                                 #+nil
                                 `(loop for arg in args
                                     collect `(funarg ,arg))
                                 `(loop for arg in args
                                     collect
                                       `(with-active-layers (funarg)
                                          ,(maybe-value arg))))))
                          :static-closure
                          (lambda ()
                            (list
                             ,@,(if
                                 (or (special-operator-p cl-symbol)
                                     (macro-function cl-symbol))
                                 `(list '',cl-symbol)
                                 `(loop for arg in args
                                     collect (maybe-value arg))))
                            #+nil
                            ,@,(if
                             (or (special-operator-p cl-symbol)
                                 (macro-function cl-symbol))
                             ``(list ,',cl-symbol)
                             ``(list ,@(loop for arg in args
                                          collect (maybe-value arg)))))))))
                  
                  ;; is this legit?
                  ;;(define-symbol-macro ,symbol ,cl-symbol)
                  ))))))

(defmacro define-simple-wrapper (symbol args body)
  `(defmacro ,symbol (&whole whole-sexp ,@args)
     `(evaluate
       (make-instance ',',symbol
        :sexp ',whole-sexp
        :closure
        (lambda ()
          ,,body)))))

(defparameter *callers* (make-hash-table))



(defun dft-reversed (tree &optional (function #'identity))
  (labels ((dft-aux (tree)
             (cond ((null tree) nil)
                   ((atom tree) (funcall function tree))
                   ((atom (car tree))
                    (cons (dft-aux (car tree))
                          (reverse (dft-aux (cdr tree)))))
                   (t (cons (dft-aux (car tree))
                            (dft-aux (cdr tree))))
                   )))
    (dft-aux tree)))

(defun dft (tree &optional (function #'identity))
  (labels ((dft-aux (tree)
             (cond ((null tree) nil)
                   ((atom tree) (funcall function tree))
                   (t (dft-aux (car tree))
                      (dft-aux (cdr tree))))))
    (dft-aux tree)))

(defun function-path-graph ()
  (let ((functions)
        (point-to))
    (loop for key being the hash-keys of *callers*
       do (pushnew (symbol-name key) functions :test #'equal))
    (loop for val being the hash-values of *callers*
       do (loop for stuff in val do (pushnew (symbol-name stuff) functions)))
    (values
     functions
     (loop for key being the hash-keys of *callers*
        collect (cons (symbol-name key)
                      (loop for val in (gethash key *callers*)
                                           collect (symbol-name val)))))))
