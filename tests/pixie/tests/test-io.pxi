(ns pixie.tests.test-io
  (require pixie.test :as t)
  (require pixie.streams :as st :refer :all)
  (require pixie.streams.utf8 :as utf8 :refer :all)
  (require pixie.io :as io)
  (require pixie.zlib :as zlib))

(t/deftest test-file-reduction
  (let [f (io/open-read "tests/pixie/tests/test-io.txt")]
    (t/assert= (transduce (map identity)
                          count-rf
                          f)
               91)))

(t/deftest test-process-reduction
  (let [f (io/run-command "ls tests/pixie/tests/test-io.txt")]
    (t/assert= f "tests/pixie/tests/test-io.txt\n")))

(t/deftest test-read-line
  (let [f (io/open-read "tests/pixie/tests/test-io.txt")]
    (io/read-line f)
    (t/assert= (io/read-line f) "Second line.")
    (t/assert= (io/read-line f) nil)))

(t/deftest test-line-seq
  (let [f (io/open-read "tests/pixie/tests/test-io.txt")
        s (io/line-seq f)]
    (t/assert= (last s) "Second line.")))

(t/deftest test-seek
  (let [f (io/open-read "tests/pixie/tests/test-io.txt")]
    (io/read-line f)
    (t/assert= (io/read-line f) "Second line.")
    (io/rewind f)
    (io/read-line f)
    (t/assert= (io/read-line f) "Second line.")
    (io/seek f (- (position f) 6))
    (t/assert= (io/read-line f) "line.")))

(t/deftest test-slurp-spit
  (let [val (vec (range 1280))]
    (io/spit "test.tmp" val)
    (t/assert= val (read-string (io/slurp "test.tmp"))))
  (t/assert-throws? (io/slurp 1))
  (t/assert-throws? (io/slurp :foo)))

(t/deftest test-slurzp-spit
  (io/run-command "rm fooz.gz")
  (zlib/test2 (io/open-write "fooz.gz") (range 1000))
  (io/run-command "zcat fooz.gz > fooz.txt")
  (t/assert= (range 1000)
             (read-string (io/slurp "fooz.txt")))
  )
