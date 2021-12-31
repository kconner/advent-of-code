(require '[clojure.set :as set])

(defn places-are-empty [path position]
  (not-any? position path))

;; the owner cannot leave the back.
(defn from-room-back [result path back owner]
  (fn [position]
    (when (and (not= owner (position back))
               (places-are-empty path position))
      result)))

;; the owner cannot leave the front if the back is the owner too.
(defn from-room-front [result path front back owner]
  (fn [position]
    (when (and (or (not= owner (position front))
                   (not= owner (position back)))
               (places-are-empty path position))
      result)))

;; only the owner can enter.
;; back must already be the owner.
(defn to-room-front [result path back hall owner]
  (fn [position]
    (when (and (= owner (position hall))
               (= owner (position back))
               (places-are-empty path position))
      result)))

;; only the owner can enter.
(defn to-room-back [result path hall owner]
  (fn [position]
    (when (and (= owner (position hall))
               (places-are-empty path position))
      result)))

(defn make-edges [owner back front path hall steps-from-front]
  [{back [(from-room-back {:from back :to hall :steps (inc steps-from-front)}
                          (conj path front hall)
                          back owner)]}
   {front [(from-room-front {:from front :to hall :steps steps-from-front}
                            (conj path hall)
                            front back owner)]}
   {hall [(to-room-front {:from hall :to front :steps steps-from-front}
                         (conj path front)
                         back hall owner)
          (to-room-back {:from hall :to back :steps (inc steps-from-front)}
                        (conj path front back)
                        hall owner)]}])

;; #############
;; #01.2.3.4.56#
;; ###a#b#c#d###
;;   #a#b#c#d#
;;   #########

(defn make-graph []
  (->> [(make-edges :a :a1 :a0 [:n1] :n0 3)
        (make-edges :a :a1 :a0 [] :n1 2)
        (make-edges :a :a1 :a0 [] :n2 2)
        (make-edges :a :a1 :a0 [:n2] :n3 4)
        (make-edges :a :a1 :a0 [:n2 :n3] :n4 6)
        (make-edges :a :a1 :a0 [:n2 :n3 :n4] :n5 8)
        (make-edges :a :a1 :a0 [:n2 :n3 :n4 :n5] :n6 9)
        (make-edges :b :b1 :b0 [:n2 :n1] :n0 5)
        (make-edges :b :b1 :b0 [:n2] :n1 4)
        (make-edges :b :b1 :b0 [] :n2 2)
        (make-edges :b :b1 :b0 [] :n3 2)
        (make-edges :b :b1 :b0 [:n3] :n4 4)
        (make-edges :b :b1 :b0 [:n3 :n4] :n5 6)
        (make-edges :b :b1 :b0 [:n3 :n4 :n5] :n6 7)
        (make-edges :c :c1 :c0 [:n3 :n2 :n1] :n0 7)
        (make-edges :c :c1 :c0 [:n3 :n2] :n1 6)
        (make-edges :c :c1 :c0 [:n3] :n2 4)
        (make-edges :c :c1 :c0 [] :n3 2)
        (make-edges :c :c1 :c0 [] :n4 2)
        (make-edges :c :c1 :c0 [:n4] :n5 4)
        (make-edges :c :c1 :c0 [:n4 :n5] :n6 5)
        (make-edges :d :d1 :d0 [:n4 :n3 :n2 :n1] :n0 9)
        (make-edges :d :d1 :d0 [:n4 :n3 :n2] :n1 8)
        (make-edges :d :d1 :d0 [:n4 :n3] :n2 6)
        (make-edges :d :d1 :d0 [:n4] :n3 4)
        (make-edges :d :d1 :d0 [] :n4 2)
        (make-edges :d :d1 :d0 [] :n5 2)
        (make-edges :d :d1 :d0 [:n5] :n6 3)]
       (apply concat)
       (apply merge-with concat)))

(def step-costs {:a 1 :b 10 :c 100 :d 1000})

(defn edges-from [graph position from]
  (let [step-cost (step-costs (position from))]
    (map (fn [edge] (assoc edge :cost (* step-cost (:steps edge))))
         (keep (fn [f] (f position)) (graph from)))))

(defn edges [graph position]
  (mapcat (partial edges-from graph position) (keys position)))

(defn position-after-moving [position from to]
  (assoc (dissoc position from) to (position from)))

(defn next-states [graph base-cost position]
  (map (fn [{from :from to :to cost :cost}]
         [(+ base-cost cost)
          (position-after-moving position from to)])
       (edges graph position)))

(defn search [graph initial-position solution-position]
  (loop [visited-positions #{initial-position}
         positions-by-cost (sorted-map 0 #{initial-position})]
    (let [[cost positions] (first positions-by-cost)
          other-positions-by-cost (dissoc positions-by-cost cost)
          next (remove (fn [[_ position]] (visited-positions position))
                       (apply concat (map (partial next-states graph cost)
                                          positions)))]
      (if (positions solution-position) cost
          (recur (set/union visited-positions positions)
                 (apply merge-with set/union
                        other-positions-by-cost
                        (map (fn [[cost position]] {cost #{position}})
                             next)))))))

(def input-regex #".*\n.*\n.*([ABCD])#([ABCD])#([ABCD])#([ABCD]).*\n.*([ABCD])#([ABCD])#([ABCD])#([ABCD]).*\n.*")

(time (let [position
            (->> (slurp "23.txt")
                 (re-matches input-regex)
                 rest
                 (map (fn [letter] (case letter "A" :a "B" :b "C" :c "D" :d)))
                 (zipmap [:a0 :b0 :c0 :d0 :a1 :b1 :c1 :d1]))]
        (search (make-graph)
                position
                {:a0 :a :a1 :a
                 :b0 :b :b1 :b
                 :c0 :c :c1 :c
                 :d0 :d :d1 :d})))
