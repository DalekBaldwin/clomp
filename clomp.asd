;;;; clomp.asd

(defpackage :clomp-system
  (:use :cl :asdf))
(in-package :clomp-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *unshadowed-symbols*
    '(#:declaim
      #:declare
      #:proclaim
      #:defmacro
      #:defclass
      #:defun
      #:defpackage
      #:defparameter
      #:defvar
      #:handler-bind
      #:handler-case
      #:error
      #:cerror
      #:assert
      #:signal
      #:load)))

(defsystem :clomp
  :name "clomp"
  :serial t
  :licence "LLGPL"
  :components
  ((:static-file "clomp.asd")
   (:module :src
            :components ((:file "package")
                         (:file "utils")
                         (:file "clomp"))
            :serial t))
  :depends-on (:named-readtables :contextl :macroexpand-dammit.clomp-patch))

(defsystem :clomp-test
  :name "clomp-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "clomp-test" :depends-on ("package")))))
  :depends-on (:clomp :stefil))
