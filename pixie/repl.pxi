(ns pixie.repl
  (:require [pixie.stacklets :as st]
            [pixie.ffi       :as ffi]
            [pixie.ffi-infer :as f]))

(f/with-config {:library "edit"
                :includes ["editline/readline.h"]}
  (f/defcfn readline)
  (f/defcfn add_history))


(defn repl []
  (let [rdr (reader-fn (fn []
                         (let [prompt (if (= 0 pixie.stdlib/*reading-form*)
                                        (str (name pixie.stdlib/*ns*) " => ")
                                        "")
                               line-ptr (st/apply-blocking readline prompt)]
                           (if line-ptr
                             (let [line (ffi/charp->str line-ptr)]
                               ;(dispose! line-ptr)
                               (add_history line)
                               (str line "\n"))
                             ""))))]
    (loop []
      (try (let [form (read rdr false)]
             (if (or (= form eof) (= form :exit))
               (exit 0)
               (let [x (eval form)]
                 (pixie.stdlib/-push-history x)
                 (prn x))))
           (catch ex
             (pixie.stdlib/-set-*e ex)
             (println "ERROR: \n" ex)))
      (recur))))
