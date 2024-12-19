(use ./tools)

# arrange towels.
# towels have patterns of colored stripes.
# stripe colors: white (w), blue (u), black (b), red (r), or green (g)
# no reversing order.
# first line: towel types.
# rest: designs to be assembled by catenating towels of whatever types
# p1: how many designs are possible at all?
# p2, i assume: how many permutations make any of the designs?

(defn model-from-file [path]
  (->> (slurp path)
       (peg/match
         ~{:main (/ (* :patterns "\n\n" :designs) ,|{:patterns $0 :designs $1})
           :patterns (/ (* :word (some (* ", " :word))) ,tuple)
           :designs (/ (some (* :word "\n")) ,tuple)
           :word (<- (some (range "az")))})
       (first)))

(defn possible? [pattern-tails-by-head design]
  # (pp ["design:" design])
  (if (empty? design)
    true
    (do
      (def head (string/slice design 0 1))
      (def design-tail (string/slice design 1))
      # (match design
      #   [head & design-tail]
      (def pattern-tails (pattern-tails-by-head head))
      # (pp ["head and design-tail:" head design-tail])
      (some (fn [pattern-tail]
              # (pp ["pattern-tail:" pattern-tail])
              (def chars (length pattern-tail))
              # todo: can special case 0 chars
              # (pp ["chars:" chars])
              # (pp ["sliced pattern-tail:" (string/slice design-tail 0 chars)])
              # (when (= pattern-tail (string/slice design-tail 0 chars))
              (when (string/has-prefix? pattern-tail design-tail)
                (def design-tail-tail (string/slice design-tail chars))
                # (pp ["design-tail-tail:" design-tail-tail])
                (possible? pattern-tails-by-head design-tail-tail)))
            (or pattern-tails []))
      # [] true))
)))

(defn tails-by-head [strings]
  (def result @{})
  (each string strings
    (update result (string/slice string 0 1)
            (fn [tails] (default tails @[])
              (array/push tails (string/slice string 1)))))

  result)

(defn problem1 [{:patterns patterns :designs designs}]
  (def pattern-tails-by-head (tails-by-head patterns))
  (count |(possible? pattern-tails-by-head $) (tuple/slice designs 0 1)))

# (defn possible-peg [patterns]
#   (peg/compile
#     ~{:main :loop
#       :loop (choice -1 ,;(map |~(* (<- ,$) :loop) patterns))}))

# (defn problem1 [{:patterns patterns :designs designs}]
#   (def peg (possible-peg patterns))
#   (count |(peg/match peg $) (tuple/slice designs 0 1)))

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      # (def path "19.txt")
      (def path "19.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))

# (def peg (possible-peg (model :patterns)))

# (peg/match (possible-peg (model :patterns))
#            ((model :designs) 7))

