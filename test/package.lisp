(in-package :cl)

(defpackage :clomp-test
  (:use :cl :stefil)
  #.`(:shadowing-import-from :clomp
                             ,@(loop for symbol being the external-symbols of :clomp
                                  collect symbol))
  (:export
   #:test-all
   #:run-all-tests))

(defpackage :clomp-test-implementation
  (:use :cl :stefil :contextl))

(in-package :clomp-test-implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :clomp-test)
    (named-readtables:defreadtable :clomp-test
      (:merge :clomp))))
