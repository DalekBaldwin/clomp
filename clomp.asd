;;;; clomp.asd

(defpackage :clomp-system
  (:use :cl :asdf))
(in-package :clomp-system)



(defsystem :clomp
  :name "clomp"
  :serial t
  :components
  ((:static-file "clomp.asd")
   (:module :src
            :components ((:file "package")
                         (:file "clomp" :depends-on ("package")))))
  :depends-on (:named-readtables))

(defsystem :clomp-test
  :name "clomp-test"
  :serial t
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "clomp-test" :depends-on ("package")))))
  :depends-on (:clomp :stefil))
