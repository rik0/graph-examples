;; Copyright (C) 2011 by Enrico Franchi

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(ns graph-examples.core
  (:use clojure.core)
  (:import
   [java.util HashSet Collection]
   [javax.swing JFrame JPanel]
   [org.apache.commons.collections15 Factory]
   [edu.uci.ics.jung.algorithms.metrics Metrics]
   [edu.uci.ics.jung.algorithms.shortestpath DistanceStatistics]
   [edu.uci.ics.jung.algorithms.generators.random ErdosRenyiGenerator BarabasiAlbertGenerator]
   [edu.uci.ics.jung.algorithms.layout Layout
    FRLayout KKLayout ISOMLayout CircleLayout]
   [edu.uci.ics.jung.visualization VisualizationViewer]
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

(defn make-erdos-renyi [p size]
  (let [generator
        (ErdosRenyiGenerator. (UndirectedSparseGraph/getFactory)
                              (vertex-factory)
                              (edge-factory)
                              size
                              p)]
    (.create generator)))

(defn make-barabasi-albert [m starting size]
  (let [^Collection nodes (range starting)
        gg (BarabasiAlbertGenerator.
            (UndirectedSparseGraph/getFactory)
            (vertex-factory)
            (edge-factory) starting m (HashSet. nodes))]
    (.evolveGraph gg (- size starting))
    (.create gg)))

(defn show-layout [^Layout layout ^String title]
  (let [viewer (VisualizationViewer. layout)
        frame (JFrame. title)
        pane (JPanel. )]
    (.add pane viewer)
    (doto frame
      (.setContentPane pane)
      (.pack)
      (.setVisible true))
    frame))

(defmacro show-layout* [layout-class graph-exp title]
  `(let [^Graph g# ~graph-exp]
     (show-layout (new ~layout-class g#) (str (quote ~title)))))

(defn show-graph
  ([^Graph g] (show-graph g "The Graph"))
  ([^Graph g ^String title] (show-layout (KKLayout. g) title)))

  
      
(defn graph-stats [^Graph g]
  (vector (clustering-coefficient g)
          (average-shortest-paths g)
          (diameter g)))
    
(defn print-table [graph-generator sizes]
  (println "+----------+------------+------------+------------+")
  (println "| size     |      C     |    APL     |     D      |")
  (println "+----------+------------+------------+------------+")
  (doseq [s sizes]
    (let [g (graph-generator s)]
      (show-graph g (str "GRAPH" s))
      (println
       (apply format
              "| % 8d | % 10f | % 10f | % 10f |"
              s (graph-stats g)))
      (println "+----------+------------+------------+------------+"))))
  
            
      