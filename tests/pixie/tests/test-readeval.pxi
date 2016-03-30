(ns pixie.tests.test-readeval
  (require pixie.test :as t))

(t/deftest test-read
  (t/assert= (read-string "0xDEADBEEF") 3735928559)
  (t/assert= (read-string "0xDeadBeef") 3735928559)
  (t/assert= (read-string "0xdeadbeef") 3735928559) 
  (t/assert= (read-string "foo") 'foo)
  (t/assert= (read-string "()") '())
  (t/assert= (read-string "(1 2 3)") '(1 2 3))
  (t/assert= (read-string "[1 2 3]") [1 2 3])
  (t/assert= (read-string "{:a 1 :b 2 :c 3}") {:a 1 :b 2 :c 3})
  (t/assert= (read-string "\"foo\"") "foo")
  (t/assert= (read-string "\"fo\\\\o\"") "fo\\o")
  (t/assert= (read-string "false") false)
  (t/assert= (read-string "true") true)
  (t/assert= (read-string "#{1 2 3}") #{1 2 3})
  (t/assert= (read-string "(foo (bar (baz)))") '(foo (bar (baz)))))

(t/deftest test-list-unclosed-list-fail
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched list open '('"
                    (read-string "("))
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched list open '('"
                    (read-string "((foo bar)")))

(t/deftest test-vector-unclosed-list-fail
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched vector open '['"
                    (read-string "["))
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched vector open '['"
                    (read-string "[[foo bar]")))

(t/deftest test-map-unclosed-list-fail
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched map open '{'"
                    (read-string "{"))
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched map open '{'"
                    (read-string "{foo {a b}")))

(t/deftest test-set-unclosed-list-fail
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched set open '#{'"
                    (read-string "#{"))
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched set open '#{'"
                    (read-string "#{foo #{a}")))

(t/deftest test-string-unclosed-fail
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched string quote '\"'"
                    (read-string "\""))
  (t/assert-throws? :pixie.stdlib/AssertionException
                    "Unmatched string quote '\"'"
                    (read-string "\"foo")))

(t/deftest test-comments-in-forms
  (t/assert= (read-string "(foo ; a comment\n )") '(foo))
  (t/assert= (read-string "[foo ; a comment\n ]") '[foo])
  (t/assert= (read-string "{:foo :bar ; a comment\n }") '{:foo :bar})
  (t/assert= (read-string "#{:foo ; a comment\n }") '#{:foo})
  (t/assert= (read-string "{:foo ; a comment\n :bar }") '{:foo :bar}))

(t/deftest test-comment-reader-macro
  (t/assert= (read-string "(foo #_bar baz)") '(foo baz))
  (t/assert= (read-string "(foo #_  bar baz)") '(foo baz))
  (t/assert= (read-string "(foo #_ #_ bar baz)") '(foo))
  (t/assert= (read-string "(foo #_(bar goo) baz)") '(foo baz))
  (t/assert= (read-string "(foo bar #_baz)") '(foo bar))
  (t/assert= (read-string "(foo bar #_ ; comment \n baz)") '(foo bar)))
