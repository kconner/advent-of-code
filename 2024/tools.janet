(defn profile [n form]
  (def start (os/time))
  (var result nil)
  (for _ 1 n
    (set result (form)))
  (def end (os/time))
  (print (- end start))
  result)

(defn lines-from-file [path]
  (filter |(not (empty? $))
          (string/split "\n" (slurp path))))
