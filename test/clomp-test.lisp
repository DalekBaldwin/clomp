(in-package #:clomp-test-implementation)
(named-readtables:in-readtable :clomp-test)

(in-root-suite)

(defsuite* clomp-test:test-all)

(defun clomp-test:run-all-tests ()
  (clomp-test:test-all))

(define-layered-method clomp:evaluate
  :around ((form clomp:form))
  (call-next-method))

(in-package #:clomp-test)

;;;; most of these tests are pulled from examples in the HyperSpec

(deftest test-block ()
  (is (cl:null (block empty))))

(deftest test-catch ()
  (is (cl:=
       3
       (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4))))

(deftest test-flet ()
  (is (cl:=
       6
       (flet ((flet1 (n) (+ n n)))
         (flet ((flet1 (n) (+ 2 (flet1 n))))
           (flet1 2))))))

(deftest test-function ()
  (is (cl:functionp (function (lambda (x) (+ x 1)))))
  (is (cl:functionp #'(lambda (x) (+ x 1))))
  (is (cl:eql (function car) #'car)))

(deftest test-if ()
  (is (cl:=
       2
       (if nil 1 2)))
  (is (cl:=
       1
       (if t 1 2)))
  (is (cl:null
       (if nil 1))))

(deftest test-labels ()
  (is (cl:=
       6
       (labels ((temp (n) 
                  (if (zerop n) 0 (+ 2 (temp (1- n))))))
         (temp 3)))))

(deftest test-let ()
  (is (cl:every
       (cl:function cl:null)
       (let (a b c d e)
         (list a b c d e)))))

(deftest test-let* ()
  (is (cl:equal
       (cl:list 1 2 3 nil 4)
       (let* ((a 1)
              (b (1+ a))
              (c (1+ b))
              d
              (e (1+ c)))
         (list a b c d e)))))

(deftest test-load-time-value ()
  (is (let ((numbers
             (loop for i from 0 to 1000
                collect (load-time-value (random most-positive-fixnum)))))
        (every (lambda (x) (= x (first numbers))) numbers))))

(deftest test-macrolet ()
  (is (cl:equal
       (cl:list 1 3)
       (cl:multiple-value-list
        (let ((z (list 3 4)))
          (macrolet ((%m (x) `(car ,x)))
            (let ((y (list 1 2)))
              (values (%m y) (%m z)))))))))



(deftest test-multiple-value-call ()
  (is (cl:equal
       (cl:list 1 '/ 2 3 '/ '/ 2 0.5)
       (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))))
  (is (cl:=
       10
       (multiple-value-call #'+ (floor 5 3) (floor 19 4)))))

(deftest test-multiple-value-prog1 ()
  (is (cl:equal
       (cl:list 1 2 3)
       (cl:multiple-value-list
        (let ((temp (list 1 2 3)))
          (multiple-value-prog1
              (values-list temp)
            (setq temp nil)
            (values-list temp)))))))

(deftest test-progn ()
  (is (cl:equal
       (cl:list 1 1)
       (cl:multiple-value-list
        (let ((x 0))
          (values (progn (incf x) x) x))))))

(deftest test-progv ()
  (is (cl:equal
       (cl:list 3 4)
       (let ((*x* 3)) 
         (progv '(*x*) '(4) 
           (list *x* (symbol-value '*x*)))))))

;;(deftest test-quote ())

(deftest test-return-from ()
  (is (cl:null (block alpha (return-from alpha) 1)))
  (is (cl:=
       1
       (block alpha (return-from alpha 1) 2)))
  (is (cl:equal
       (cl:list 1 2)
       (cl:multiple-value-list
        (block alpha (return-from alpha (values 1 2)) 3))))
  (is (cl:=
       1
       (block out
         (flet ((exit (n) (return-from out n)))
           (block out (exit 1)))
         2)))
  (is (cl:=
       2
       (block nil   
         (unwind-protect (return-from nil 1)
           (return-from nil 2))))))

(deftest test-setq ()
  (is (cl:equal
       (cl:list 1 2 3 4 5)
       (let (a b c d e)
         (setq a 1 b 2 c 3 d 4 e 5)
         (list a b c d e)))))

(deftest test-symbol-macrolet ()
  (is (cl:equal
       '(foo bar)
       (symbol-macrolet ((x 'foo))
         (list x (let ((x 'bar)) x)))))
  (is (cl:equal
       '((foo x))
       (symbol-macrolet ((x '(foo x)))
         (list x)))))

(deftest test-tagbody ()
  (is (cl:=
       15
       (let (val)
         (tagbody
            (setq val 1)
            (go point-a)
            (incf val 16)
          point-c
            (incf val 04)
            (go point-b)
            (incf val 32)
          point-a
            (incf val 02)
            (go point-c)
            (incf val 64)
          point-b
            (incf val 08))
         val))))

(deftest test-the ()
  (is (cl:=
       12
       (the fixnum (+ 5 7))))
  (is (cl:equal
       (cl:list 1 1.2)
       (cl:multiple-value-list
        (the (values) (truncate 3.2 2))))))

(deftest test-throw ()
  (is (cl:equal
       (cl:list 3 9)
       (cl:multiple-value-list
        (catch 'result
          (setq i 0 j 0)
          (loop (incf j 3) (incf i)
             (if (= i 3) (throw 'result (values i j))))))))
  (is (cl:=
       2
       (catch nil 
         (unwind-protect (throw nil 1)
           (throw nil 2)))))
  (is (cl:=
       2
       (catch 'a
         (catch 'b
           (unwind-protect (throw 'a 1)
             (throw 'b 2)))))))

(deftest test-unwind-protect ()
  (is (cl:=
       2
       (block nil   
         (unwind-protect (return 1)
           (return 2)))))
  (is (cl:=
       10
       (catch 'a
         (catch 'b
           (unwind-protect (1+ (catch 'a (throw 'b 1)))
             (throw 'a 10))))))
  (is (cl:=
       4
       (catch 'bar
         (catch 'foo
           (unwind-protect (throw 'foo 3)
             (throw 'bar 4)
             (print 'xxx)))))))

;;;;;;;;;;;;;;;;;;;;

(deftest test-setf ()
  (is (cl:equal
       (cl:list 5 2 3 4)
       (let ((x (list 1 2 3 4)))
         (setf (car x) 5)
         x))))

(deftest test-handler-case ()
  (is (cl:=
       5
       (handler-case (derp)
         (undefined-function () 5)))))


