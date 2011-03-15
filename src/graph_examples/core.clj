(ns graph-examples.core
  (:use clojure.core)
  (:import
   [org.apache.commons.collections15 Factory]
   [edu.uci.ics.jung.algorithms.metrics Metrics]
   [edu.uci.ics.jung.algorithms.shortestpath DistanceStatistics]
   [edu.uci.ics.jung.algorithms.generators.random ErdosRenyiGenerator]
   [edu.uci.ics.jung.graph Graph SparseGraph UndirectedSparseGraph]))

(set! *warn-on-reflection* true)

(defn average [coll]
  (if (seq coll)
    (loop [coll coll
           sum (double 0.0)
           len (int 0)]
      (if (seq coll)
        (recur (rest coll) (double (+ sum (first coll))) (int (inc len)))
        (/ sum len)))
    Double/NaN))


(defn ^Double clustering-coefficient [^Graph graph]
  "Compute the clustering coefficient of graph."
  (let [mapping (Metrics/clusteringCoefficients graph)
        coefficients (vals mapping)]
    (average coefficients)))

(defn ^Double diameter [^Graph graph]
  "Compute the diameter of the graph."
  (DistanceStatistics/diameter graph))

(defn ^Double average-shortest-paths [^Graph graph]
  "Return a map connecting each node to its average shortest path.

  Notice that some nodes are not connected, it is Double/POSITIVE_INFINITY."
  (let [transformer (DistanceStatistics/averageDistances graph)
        path-lengths (map #(.transform transformer %)
                          (.getVertices graph))]
    (average (filter #(not (or (Double/isNaN %) (Double/isInfinite %)))
                     path-lengths))))

(defn edge-factory []
  (proxy [Factory] []
    (create []
            (gensym "E"))))

(defn vertex-factory []
  (proxy [Factory] []
    (create []
            (gensym "V"))))

(defmacro graph-factory
  ([] `(graph-factory UndirectedSparseGraph))
  ([klass]
     `(proxy [Factory] []
        (create [] (new ~klass)))))
  
(defn make-erdos-reyini [size p]
  (let [generator
        (ErdosRenyiGenerator. (graph-factory)
                              (vertex-factory)
                              (edge-factory)
                              size
                              p)]
    (.create generator)))

  
(defn erdos-table [p sizes]
  (println "+----------+------------+------------+------------+")
  (println "| size     |      C     |    APL     |     D      |")
  (println "+----------+------------+------------+------------+")
  (doseq [s sizes]
    (let [g (make-erdos-reyini s p)]
      (println (format "| % 8d | % 10f | % 10f | % 10f |"
                     s (clustering-coefficient g)
                      (average-shortest-paths g)
                      (diameter g)))
      (println "+----------+------------+------------+------------+"))))
  
            
      