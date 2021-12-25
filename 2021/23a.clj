(require '[clojure.string :as string])
(require '[clojure.set :as set])

;#############
;#01.2.3.4.56#
;###a#b#c#d###
;  #a#b#c#d#
;  #########

(defn make-graph []
  {:n0 {:edges {:n1 1}}
   :n1 {:edges {:n0 1 :a0 2 :n2 2}}
   :a0 {:edges {:n1 2 :n2 2 :a1 1} :wants :a :after :a1}
   :a1 {:edges {:a0 1}             :wants :a}
   :n2 {:edges {:n1 2 :a0 2 :b0 2 :n3 2}}
   :b0 {:edges {:n2 2 :n3 2 :b1 1} :wants :b :after :b1}
   :b1 {:edges {:b0 1}             :wants :b}
   :n3 {:edges {:n2 2 :b0 2 :c0 2 :n4 2}}
   :c0 {:edges {:n3 2 :n4 2 :c1 1} :wants :c :after :c1}
   :c1 {:edges {:c0 1}             :wants :c}
   :n4 {:edges {:n3 2 :c0 2 :d0 2 :n5 2}}
   :d0 {:edges {:n4 2 :n5 2 :d1 1} :wants :d :after :d1}
   :d1 {:edges {:d0 1}             :wants :d}
   :n5 {:edges {:n4 2 :d0 2 :n6 1}}
   :n6 {:edges {:n5 1}}})

(defn initial-position [a0 a1 b0 b1 c0 c1 d0 d1]
  (zipmap [:a0 :a1 :b0 :b1 :c0 :c1 :d0 :d1]
          [a0 a1 b0 b1 c0 c1 d0 d1]))

(def -graph (make-graph))
(def -position (initial-position :b :a :c :d :b :c :d :a))

(def step-costs {:a 1 :b 10 :c 100 :d 1000})

(defn open-edges [graph position from]
  (let [edges ((graph from) :edges)]
    (remove (fn [[key _]] (key position)) edges)))

-position
(open-edges -graph -position :a0)

(defn position-after-moving [position from to]
  (assoc (dissoc position from) to (position from)))

(position-after-moving -position :a0 :n1)

(defn next-states-from [graph position from]
  (let [edges (open-edges graph position from)]
    (map (fn [[to steps]]
           [steps (position-after-moving position from to)])
         edges)))

(next-states-from -graph -position :a0)

(defn next-states-for-position [graph position]
  (mapcat (fn [[from who]]
            (let [step-cost (step-costs who)]
              (map (fn [[steps position]]
                     [(* steps step-cost) position])
                   (next-states-from graph position from))))
          position))

(next-states-for-position -graph -position)

(defn next-states-for-positions [graph visited-positions base-cost positions]
  (keep (fn [[cost position]]
          (when (not (visited-positions position))
            [(+ base-cost cost) position]))
        (mapcat (partial next-states-for-position graph) positions)))

(def -visited-positions
  #{{:a0 :b, :a1 :a, :b0 :c, :b1 :d, :c0 :b, :c1 :c, :d1 :a, :n4 :d}})
(next-states-for-positions -graph -visited-positions 10 [-position])

(defn is-solved [graph position]
  (every? (fn [at] (= (position at) ((graph at) :wants)))
          [:a1 :b1 :c1 :d1 :a0 :b0 :c0 :d0]))

(defn search [graph position depth]
  (loop [visited-positions #{position}
         positions-by-cost (sorted-map 0 (list position))
         depth depth]
    (let [[cost positions] (first positions-by-cost)
          other-positions-by-cost (dissoc positions-by-cost cost)]
      ;; (println cost)
      (if (zero? depth) [cost (count visited-positions) (count positions-by-cost)]
          (if (some (partial is-solved graph) positions) cost
              (let [new-states
                    (next-states-for-positions graph visited-positions cost positions)]
                (recur (set/union visited-positions (set (map second new-states)))
                       (apply merge-with concat
                              other-positions-by-cost
                              (map (fn [[cost position]] {cost [position]}) new-states))
                       (dec depth))))))))

(time (search -graph -position 1000))

;; (let [positions-by-cost (sorted-map 3 [:ok] 4 :nah)
;;       [cost positions] (first positions-by-cost)
;;       other-positions-by-cost (dissoc positions-by-cost cost)]
;;   (first (assoc other-positions-by-cost 8 :boop)))
