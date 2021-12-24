(require '[clojure.string :as string])

(defn make-graph [a0 a1 b0 b1 c0 c1 d0 d1]
  {:n0 {:edges {:n1 1}}
   :n1 {:edges {:n0 1 :a0 2 :n2 2}}
   :a0 {:edges {:n1 2 :n2 2 :a1 1} :wants :a :who a0}
   :a1 {:edges {:a0 1}             :wants :a :who a1}
   :n2 {:edges {:n1 2 :a0 2 :b0 2 :n3 2}}
   :b0 {:edges {:n2 2 :n3 2 :b1 1} :wants :b :who b0}
   :b1 {:edges {:b0 1}             :wants :b :who b1}
   :n3 {:edges {:n2 2 :b0 2 :c0 2 :n4 2}}
   :c0 {:edges {:n3 2 :n4 2 :c1 1} :wants :c :who c0}
   :c1 {:edges {:c0 1}             :wants :c :who c1}
   :n4 {:edges {:n3 2 :c0 2 :d0 2 :n5 2}}
   :d0 {:edges {:n4 2 :n5 2 :d1 1} :wants :d :who d0}
   :d1 {:edges {:d0 1}             :wants :d :who d1}
   :n5 {:edges {:n4 2 :d0 2 :n6 1}}
   :n6 {:edges {:n5 1}}})

;#############
;#01.2.3.4.56#
;###a#b#c#d###
;  #a#b#c#d#
;  #########

(def -graph (make-graph :b :a :c :d :b :c :d :a))

(defn move [graph from to]
  (let [who (:who (graph from))]
    (-> graph
        (update from (fn [node] (dissoc node :who)))
        (update to (fn [node] (assoc node :who who))))))

(def costs {:a 1 :b 10 :c 100 :d 1000})

(defn edge-cost [graph from to]
  (let [start (graph from)]
    (* (costs (:who start))
       ((:edges start) to))))

(def -after (move -graph :b0 :n2))

(:b0 -graph)
(:n2 -graph)
(:b0 -after)
(:n2 -after)

(edge-cost -graph :n2 :b0)