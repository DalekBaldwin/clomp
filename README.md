(CL)<sup>2</sup>OMP: Common Lisp CLosure-Oriented MetaProgramming
=================================================================

This project is intended to be a jumping-off point for exploring a number of different AOP-style metaprogramming strategies. For purposes of demonstration, I'm presenting what I've found to be the most concise and comprehensible possible implementation of a powerful concept. Here's how it could be used to trace a few selected language elements while leaving the rest of the language untouched:

```lisp
(in-package :cl)

(defpackage :clomp-user
  (:use :cl)
  (:shadowing-import-from :clomp
                          :let
                          :if
                          :+))

(let ((depth 0))
  (defmethod clomp:evaluate :around (form)
             (let ((dashes
                    (with-output-to-string (s)
                      (dotimes (i depth)
                        (princ "--" s)))))
               (format t "~&-~A> ~A~%" dashes (clomp:sexp form))
               (incf depth)
               (let ((result (call-next-method)))
                 (decf depth)
                 (format t "~&<-~A ~A: ~A~%" dashes (clomp:sexp form) result)
                 result))))

(defmethod clomp:evaluate :before ((form clomp:if))
  (format t "~&(This is an if-form!)~%"))

(in-package :clomp-user)

(let ((x 2)
      (y 3))
  (if (< x y)
      (+ x y)
      (- x y))
  (if (> x y)
      (+ x y)
      (* x y)))
```

```
-> (LET ((X 2) (Y 3)) (IF (< X Y) (+ X Y) (- X Y))
        (IF (> X Y) (+ X Y) (* X Y)))
---> (IF (< X Y) (+ X Y) (- X Y))
(This is an if-form!)
-----> (+ X Y)
-------> X
<------- X: 2
-------> Y
<------- Y: 3
<----- (+ X Y): 5
<--- (IF (< X Y) (+ X Y) (- X Y)): 5
---> (IF (> X Y) (+ X Y) (* X Y))
(This is an if-form!)
<--- (IF (> X Y) (+ X Y) (* X Y)): 6
<- (LET ((X 2) (Y 3)) (IF (< X Y) (+ X Y) (- X Y))
        (IF (> X Y) (+ X Y) (* X Y))): 6
```

I took the phrase "closure-oriented metaprogramming" from Vladimir Sedach's [awesome example] [1] which demonstrates how this approach can be used to infer static code structure from dynamic behavior. He notes a few caveats:

> This code also provides examples of the two problems that the closure-oriented metaprogramming approach encounters in Common Lisp:

> The first is the fact that we had to shadow = in our package. Common Lisp forbids the redefinition of the functions, macros and special forms defined in the standard, so we have to go out of our way if we want to achieve that effect. Barry Margolin provided a rationale for this in comp.lang.lisp post.

As Clomp demonstrates, this is not too difficult.

> The second is the fact that Common Lisp has so many special forms and macros - and just happens to be one of them. Smalltalk avoids this problem by doing virtually everything via message passing and closures. In Common Lisp we don't have this straightjacket, but we also don't have this luxury of assuming that everything is an object or a closure.

... i.e., this technique is vulnerable to many of the same pitfalls as writing a code walker -- it's not an inherently bad idea, but it's generally ill-advised to roll your own for use in a one-off project. For this reason Clomp only encloses complete special forms and built-in macros, but a finer-grained approach could be used enclose their evaluated subforms, e.g. the test, then, and else clauses of an if-form. (For built-in-function forms, however, Clomp wraps each argument in a closure, as seen in the example above.) This resembles the process writing a metacircular interpreter, but from a different vantage point. The underlying Common Lisp implementation is in the driver's seat, and it only calls out to your reflective variants at the lexical locations you've declared interest in, so you don't pay for what you don't use and you can test how your interpreter interacts with the rest of the language with the benefit of all the optimizations your implementation already provides. You might call it "just-in-time interpretation". I'm experimenting with a few different approaches to this at the moment.

One last note: There may be issues involving certain special forms no longer receiving special treatment as top-level forms by the underlying implementation.

[1]:http://carcaddar.blogspot.com/2009/04/closure-oriented-metaprogramming-via.html
