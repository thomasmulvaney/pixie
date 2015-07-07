(ns pixie.test.test-protocols
    (require pixie.test :as t))

(defprotocol IFoo
  (foo [a]
    "great doc string"))

(t/deftest test-polymorphic-fn-types
  (t/assert= (type foo) PolymorphicFn))

;; A protocol can extend a type
(t/deftest test-polymorphic-fn-types
  (extend-protocol IFoo
    Integer
    (foo [a]
      (str "foo: " a)))
  (t/assert= (foo 42)   "foo: 42")
  (t/assert= (foo 1337) "foo: 1337")
  (t/assert-throws? RuntimeException 
                    "No override for pixie.stdlib.Ratio on foo in protocol IFoo"
                    (foo 1/2))
  (t/assert-throws? RuntimeException 
                    "No override for pixie.stdlib.Float on foo in protocol IFoo"
                    (foo 0.1)))

;; A protocol can extend a parent type
(t/deftest test-polymorphic-fn-parent-types
  (extend-protocol IFoo
    Number
    (foo [a]
      (str "foo: " a)))
  (t/assert= (foo 42)   "foo: 42")
  (t/assert= (foo 1337) "foo: 1337")
  (t/assert= (foo 1/2)  "foo: 1/2"))

;; A protocol can extend a protocol
(t/deftest test-polymorphic-fn-protocols
  (extend-protocol IFoo
    INamed
    (foo [a]
      (str "name: " (name a))))
  (t/assert= (foo 'dog)   "name: dog")
  (t/assert= (foo :dog)   "name: dog")
  (t/assert= (foo "dog")  "name: dog"))


;; DoublePolymorphic Protocols
(defprotocol IAdd
  (add [a b]
       {:variadicity 2}
       "add stuff"))

(t/deftest test-double-polymorphic-fns-types
  (extend-protocol IAdd
    [Integer Integer]
    (add [a b] 
      (+ a b)))
  (t/assert= (add 1 3) 4)
  (t/assert-throws? RuntimeException 
                    "No override for [pixie.stdlib.Float pixie.stdlib.Integer] on add in protocol IAdd"
                    (add 1.0 2))
  (t/assert-throws? RuntimeException 
                    "No override for [pixie.stdlib.Integer pixie.stdlib.Float] on add in protocol IAdd"
                    (add 1 2.0))
  (t/assert-throws? RuntimeException 
                    "No override for [pixie.stdlib.String pixie.stdlib.String] on add in protocol IAdd"
                    (add "dog" "cat")))
 
(t/deftest test-double-polymorphic-fns-parent-types
  (extend-protocol IAdd
    [Number Number]
    (add [a b] 
      (+ a b)))
  (t/assert= (add 1 3) 4)
  (t/assert= (add 1/2 1/2) 1)
  (t/assert= (add 1/4 1) 5/4)) 

(t/deftest test-double-polymorphic-fns-protocols
  (extend-protocol IAdd
    [Number INamed]
    (add [a b] 
      (str "number and named"))
    
    [INamed Number]
    (add [a b]
      (str "named and number"))
    
    [INamed INamed]
    (add [a b]
      (str "named and named")))
  (t/assert= (add 1 "dog") "number and named")
  (t/assert= (add "dog" 1) "named and number")
  (t/assert= (add "dog" "cat") "named and named")
  )
