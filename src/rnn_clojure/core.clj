(ns rnn-clojure.core
    (:gen-class)
    (:require [clojure.tools.analyzer.ast :as ana.ast]
              [clojure.tools.analyzer.jvm :as ana.jvm]
              [clojure.pprint :as pprint]
              [clojure.core.match :as m]
              [clojure.tools.namespace.repl :as repl]))

;; remember:  (repl/refresh)


(defn ops-used [ast] (set (map #(:op %) (ana.ast/nodes ast))))

(defn op-freqs
  [ns-ast]  
  (sort-by
    val
    >
    (frequencies (apply concat (map #(into [] %) (map ops-used ns-ast))))))


(defn gete
  [hash key]
  "safe accessor for hashtables, throws exception if key does not exist"
  (let [v (key hash)]
    (if (nil? v)
        (throw (Exception. (format "unknown key %s in node %s" key hash)))
         v)))

;;; TODO  Why doesn't this work as we expect?
(defn simplify-ast
  [node]
  "see what ast type it is; throw away if bad (and throw exception); otherwise, simplify down to the minimal ast we want (throw away meta data, etc)"
  (case (gete node :op)
    :def [:def (simplify-ast (gete node :init))]
    :with-meta (simplify-ast (gete node :expr))
    :fn (if (gete node :variadic?)
          (Exception. (format "variadic function"))
          (let [m (first (gete node :methods))
                body (gete m :body)]
            (println  "m: " m)
            (println "")
            (println "body: " body)
            (println "")
            (when (not (empty? (gete body :statements)))
              (Exception. (format "do with statements")))
            [:fn (simplify-ast (gete body :ret))]))
    (throw (Exception. (format "unknown node type %s" (gete node :op))))))

;; Need to write a fn that will try simplifying every function ast in
;; a namespace, cath the exception for the ones that fail.  Once
;; everything passes through that, generate linear traces by doing
;; tree walks.  Output of the program will be a huge long string we
;; can pass to an RNN.

(def out-file-name "rs-post-no-meta-out.clj")

(defn print-short
  []
  (set! *print-level* 3)
  (set! *print-length* 10))

(defn print-long
  []
  (set! *print-level* 1000)
  (set! *print-length* 1000))

(defn repl-setup
  []
  (repl/refresh)
  (print-short)
  (def rs (with-open [r (java.io.PushbackReader. (java.io.FileReader. (java.io.File. out-file-name)))]
            (read r))))

(defn analyze
  []
  (def asts (ana.jvm/analyze-ns 'clojure.core))
  (def rs (first (filter (fn [node] (and (contains? node :name) (= (:name node) 'random-sample))) asts)))
  (def rs-post (ana.ast/postwalk rs (fn [node] (dissoc node :ns :atom :meta))))
  (with-open [w (java.io.FileWriter. (java.io.File. out-file-name))]
    (print-dup rs-post w))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]  
  (repl-setup)  
  ;(println "w00t awesome!")
  ;(def asts (ana.jvm/analyze-ns 'clojure.core))
  ;(pprint/pprint (op-freqs asts))
  ;(def rs (first (filter (fn [node] (and (contains? node :name) (= (:name node) 'random-sample))) asts)))
  )
