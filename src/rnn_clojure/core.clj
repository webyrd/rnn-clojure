(ns rnn-clojure.core
    (:gen-class)
    (:require [clojure.tools.analyzer.ast :as ana.ast]
	      [clojure.tools.analyzer.jvm :as ana.jvm]
	      [clojure.pprint :as pprint]
              [clojure.core.match :as m]))

(defn ops-used [ast] (set (map #(:op %) (ana.ast/nodes ast))))

(defn op-freqs
  [ns-ast]  
  (sort-by
    val
    >
    (frequencies (apply concat (map #(into [] %) (map ops-used ns-ast))))))

(defn simplify-ast
  [node]
  "write me!"
  "see what ast type it is; throw away if bad (and throw exception); otherwise, simplify down to the minimal ast we want (throw away meta data, etc)")

;; Need to write a fn that will try simplifying every function ast in
;; a namespace, cath the exception for the ones that fail.  Once
;; everything passes through that, generate linear traces by doing
;; tree walks.  Output of the program will be a huge long string we
;; can pass to an RNN.

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Freakin awesome!")
  (pprint/pprint (op-freqs (ana.jvm/analyze-ns 'clojure.core)))
  )
