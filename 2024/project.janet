(declare-project
  :name "advent-2024"
  :dependencies [])

(let [day-count 8]
  (for day 1 (inc day-count)
    (declare-executable
      :name (string day)
      :entry (string/format "%d.janet" day))))
