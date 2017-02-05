(ns rnn-clojure.core
    (:gen-class)
    (:require [clojure.tools.analyzer.ast :as ana.ast]
              [clojure.tools.analyzer.jvm :as ana.jvm]
              [clojure.pprint :as pprint]
              [clojure.core.match :as m]
              [clojure.tools.namespace.repl :as repl]))

;; remember:  (do (repl/refresh) (def rs (load-rs)))

(def ops-supported #{:fn-method :instance-call :the-var :binding :const :with-meta :do
                     :try :catch :vector :new :keyword-invoke :static-call
                     :quote :let :if :var :protocol-invoke :local :set :map :def :invoke :fn})

(defn ops-used [ast] (set (mapv #(:op %) (ana.ast/nodes ast))))

(defn ops-unsupported [ast]
  (clojure.set/difference (ops-used ast) ops-supported))

(defn using-op
  [asts op]
  (filter
    (fn [ast]
      (contains? (ops-used ast) op))
    asts))

(defn op-freqs
  [ns-ast]
  (sort-by
    val
    >
    (frequencies (apply concat
                        (mapv #(into [] %)
                             (mapv ops-unsupported ns-ast))))))


(defn gete
  [hash key]
  "safe accessor for hashtables, throws exception if key does not exist"
  (let [v (key hash)]
    (if (nil? v)
        (throw (Exception. (format "unknown key %s in node %s" key hash)))
         v)))

(defn simplify-ast
  [node]
  "see what ast type it is; throw away if bad (and throw exception);
   otherwise, simplify down to the minimal ast we want (throw away meta data, etc)"
  (case (gete node :op)

    ; forms we erase
    :with-meta (simplify-ast (gete node :expr))
    :do (if (not (empty? (gete node :statements)))
          (throw (Exception. (format "do with statements")))
          (simplify-ast (gete node :ret)))
    :static-call (simplify-ast (first (gete node :args)))
    :try (simplify-ast (first (gete node :body)))

    ; leaf forms
    :var {:op :topvar}
    :the-var {:op :topvar}
    :local (case (gete node :local)
             :arg {:op :fnvar}
             {:op :letvar}) 
    :quote {:op :quote}

    ; TODO: these need to recur to arguments. Map should look like an alist literal via
    ; the list constructor.
    :map {:op :map}
    :set {:op :set}
    :vector {:op :vector}

    ; TODO: add loop/recur

    :new {:op :invoke :children (into [{:op :topvar}]
                                      (mapv simplify-ast (gete node :args)))}

    :keyword-invoke {:op :invoke :children [{:op :topvar}
                                            (simplify-ast (gete node :target))]}

    :let (if (> (count (gete node :bindings)) 1)
           (throw (Exception. "can only handle single let binding"))
           {:op :let,
            :children (vector (simplify-ast (gete (first (gete node :bindings)) :init))
                              (simplify-ast (gete node :body)))})

    :def {:op :def,
          :children (vector (simplify-ast (gete node :init)))}

    :invoke {:op :invoke
             :children (into (vector (simplify-ast (gete node :fn)))
                             (mapv simplify-ast (gete node :args)))}

    :instance-call {:op :invoke
                    :children (into (vector {:op :topvar})
                                    (mapv simplify-ast (gete node :args)))}


    :protocol-invoke {:op :invoke
                      :children (into (vector (simplify-ast (gete node :protocol-fn)))
                                      (mapv simplify-ast (gete node :args)))}

    :fn (if (or (> (count (gete node :methods)) 1)
                (gete node :variadic?))
          (throw (Exception. (format "variadic function")))
          (let [m (first (gete node :methods))]
            {:op :fn,
             :children (vector (simplify-ast (gete m :body)))}))

    :if {:op :if,
         :children (mapv #(simplify-ast (gete node %)) [:test :then :else])}

    (do (throw (Exception. (format "unknown node type %s" (gete node :op)))) nil)))

(defn try-simplify
  [ast]
  (try
    (simplify-ast ast)
    (catch Exception e
      nil)))

(defn simplify-all
  [asts]
  (filter
    identity
    (mapv
      try-simplify
      asts)))

;; Need to write a fn that will try simplifying every function ast in
;; a namespace, cath the exception for the ones that fail.  Once
;; everything passes through that, generate linear traces by doing
;; tree walks.  Output of the program will be a huge long string we
;; can pass to an RNN.

(defn tree-structure
  "Show the basic structure of an unsimplified AST. Useful when trying to
   understand the AST format."
  [ast]
  (if (vector? ast)
    (mapv tree-structure ast)
    (into {:op (gete ast :op)}
          (mapv (fn [key]
                 [key (tree-structure (gete ast key))])
               (or (get ast :children) [])))))

(defn assexpr
  [ast]
  (if (contains? ast :children)
    (concat (if (= (gete ast :op) :invoke)
              ()
              (list (symbol (name (gete ast :op)))))
            (map assexpr (get ast :children)))
    (symbol (name (gete ast :op)))))

(def out-file-name "rs-post-no-meta-out.clj")

(defn print-short
  []
  (set! *print-level* 3)
  (set! *print-length* 10))

(defn print-long
  []
  (set! *print-level* 1000)
  (set! *print-length* 1000))

(defn load-fn []
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. (java.io.File. out-file-name)))]
    (read r)))

(defn update-if-present
  [coll key f]
  (if (contains? coll key)
    (update coll key f)
    coll))

(defn write-fn
  [asts name]
  (let [fnast (first (filter (fn [node] (and (contains? node :name)
                                          (= (:name node) name)))
                          asts))]
    (with-open [w (java.io.FileWriter. (java.io.File. out-file-name))]
      (print-dup fnast w))))

(defn analyze [ns]
  (let [asts (ana.jvm/analyze-ns ns)]
    (map (fn [ast]
           (ana.ast/postwalk
             ast
             (comp
               #(update-if-present % :meta (fn [env] (dissoc env :ns)))
               #(update-if-present % :env (fn [env] (dissoc env :ns)))
               #(dissoc % :atom))))
         asts)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [asts (analyze (first args))
        filtered (filter #(not (= % {:op :def, :children [[:op :fn] [:children [[:op :local]]]]}))
                         (simplify-all asts))]

    (pprint/pprint (map (fn [ast] 
                          (try (assexpr ast)
                               (catch Exception e
                                 (pprint/pprint ast)
                                 (throw (Exception. "done"))))) filtered))
    (println (count filtered))
    (pprint/pprint (op-freqs asts))))

