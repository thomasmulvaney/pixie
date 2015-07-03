(ns pixie.test.test-protocols
    (require pixie.test :as t))

(defprotocol IFoo
  (foo [a]
    "great doc string")
  (bar [a b]
    {:variadicity 2}
    "Doc string"))

(t/deftest test-polymorphic-fn-types
  (t/assert= (type foo) PolymorphicFn)
  (t/assert= (type bar) DoublePolymorphicFn))

(extend-protocol IFoo
  Integer
  (foo [a]
    (str "foo: " a))
  
  [Integer Integer]
  (bar [a b]
    (+ a b))
 
  INamed
  (foo [a]
   (str "foo: " a)) 

  [INamed INamed]
  (bar [a b]
    (str (name a) " and " (name b))))

;; extending a protocol with a parent type means children types
;; are extended
(t/deftest test-polymorphic-fn-types
  (t/assert= (foo 42) "foo: 42")
  (t/assert= (bar 42 7) 49)
  (t/assert-throws? RuntimeException 
                    "No override for [pixie.stdlib.Integer pixie.stdlib.String] on bar in protocol IFoo"
                    (bar 42 "7")))
