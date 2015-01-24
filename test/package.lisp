(in-package #:cl-user)

(defpackage #:clomp-test
  (:use #:cl #:stefil)
  #.`(:shadowing-import-from #:clomp-shadow
                             ,@(loop for symbol being the external-symbols of :clomp-shadow
                                  when (or (fboundp symbol)
                                           (special-operator-p symbol))
                                  collect (make-symbol (symbol-name symbol))))
  (:export
   #:test-all
   #:run-all-tests))

(defpackage #:clomp-test-implementation
  (:use #:cl #:stefil #:contextl))

(in-package #:clomp-test-implementation)

(unless (named-readtables:find-readtable :clomp-test)
  (named-readtables:defreadtable :clomp-test
    (:merge :clomp)))
