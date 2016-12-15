(ns rnn-clojure.core
    (:gen-class)
    (:require [clojure.tools.analyzer.ast :as ana.ast]
	      [clojure.tools.analyzer.jvm :as ana.jvm]
	      [clojure.pprint :as pprint]))

(defn ops-used [ast] (set (map #(:op %) (ana.ast/nodes ast))))

(defn op-freqs
  [ns-ast]  
  (sort-by
    val
    >
    (frequencies (apply concat (map #(into [] %) (map ops-used ns-ast))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Freakin awesome!")
  (pprint/pprint (op-freqs (ana.jvm/analyze-ns 'clojure.core)))
  )
