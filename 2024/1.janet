(import ./tools :only [lines-from-file])

# parsing

(def line-grammar
  (let [location-id ~(capture (some (range "09")))
        spaces ~(some " ")]
    (peg/compile ~(* ,location-id ,spaces ,location-id))))

(defn numbers-in-line [text]
  (map scan-number (peg/match line-grammar text)))

(defn lists-from-file [path]
  (map array ;(map numbers-in-line (tools/lines-from-file path))))

# problem 1

(defn problem1 [path]
  (->> path
       (lists-from-file)
       (map sort)
       (apply map -)
       (map math/abs)
       (apply +)))

# problem 2

(defn kinda-like-a-dot-product [freqs1 freqs2]
  (->> freqs1
       (keys)
       (map
         (fn [k]
           (* k
              (get freqs1 k)
              (get freqs2 k 0))))
       (apply +)))

(defn problem2 [path]
  (->> path
       (lists-from-file)
       (map frequencies)
       (apply kinda-like-a-dot-product)))

# main 

(defn main [&]
  # (def input-path "1.test.txt")
  (def input-path "1.txt")
  (print (problem1 input-path))
  (print (problem2 input-path)))
