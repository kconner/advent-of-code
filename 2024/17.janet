(use ./tools)

# 3-bit computer. a word is a number, 0-7
# instruction: [opcode operand]
# - each is one word.
# program is read into memory and kept there. that's an array
# instruction pointer is an index into that array
# halt when trying to read IP out of bounds ("past the end")

# (implemntations of each opcode…)

# Your first task is to determine what the program is trying to output.
# To do this,
# - initialize the registers to the given values, then
# - run the given program,
#   - collecting any output produced by :out instructions.
#   - (Always join the values produced by :out instructions with commas.)
# After one exampleabove program halts, its final output will be
# 4,6,3,5,6,3,5,2,1,0.

# - initialize the registers to the given values,
# - then run the program.
# - what do you get if you use commas to join the values it output into a single
#   string?

(defn model-from-file [path]
  (->> (slurp path)
       (peg/match
         ~{:main (/ (* :registers "\n" :memory)
                    ,|{:registers $0 :memory $1})
           :registers (/ (* :register :register :register)
                         ,|@{:a $0 :b $1 :c $2})
           :register (* "Register " (range "AC") ": " :int "\n")
           :memory (/ (* "Program: " :int (some (* "," :int)) "\n") ,tuple)
           :int (/ (<- (some (choice "-" (range "09")))) ,scan-number)})
       (first)))

(defn fetch [memory ip]
  (assert (<= 0 ip))
  (assert (< ip (dec (length memory))))
  (assert (even? ip))
  {:opcode (memory ip) :operand (memory (inc ip))})

(defn deref-combo [registers operand]
  (case operand
    7 (assert false)
    4 (registers :a) 5 (registers :b) 6 (registers :c)
    operand))

(defn div-instruction [registers operand register]
  (def combo (deref-combo registers operand))
  (set (registers register) (brshift (registers :a) combo))
  {})

(defn adv [registers operand]
  (div-instruction registers operand :a))

(defn bxl [registers operand]
  (update registers :b |(bxor $ operand))
  {})

(defn bst [registers operand]
  (def combo (deref-combo registers operand))
  (set (registers :b) (mod combo 8))
  {})

(defn jnz [registers operand]
  (if (not (zero? (registers :a)))
    {:ip operand}
    {}))

(defn bxc [registers _]
  (update registers :b |(bxor $ (registers :c)))
  {})

(defn out [registers operand]
  (def combo (deref-combo registers operand))
  {:out-item (string/format "%d" (mod combo 8))})

(defn bdv [registers operand]
  (div-instruction registers operand :b))

(defn cdv [registers operand]
  (div-instruction registers operand :c))

(defn execute [registers {:opcode opcode :operand operand}]
  (def op (case opcode 0 adv 1 bxl 2 bst 3 jnz 4 bxc 5 out 6 bdv 7 cdv))
  (op registers operand))

(defn run [registers memory]
  (var ip 0)
  (def out-items @[])
  (while (< ip (dec (length memory)))
    (def instruction (fetch memory ip))
    (def {:ip jump-ip :out-item out-item}
      (execute registers instruction))
    (when (not (nil? out-item))
      (array/push out-items out-item))
    (if (nil? jump-ip)
      (set ip (+ ip 2))
      (set ip jump-ip)))
  out-items)

(defn problem1 [{:registers registers :memory memory}]
  (string/join (run (table/clone registers) memory) ","))

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "17.txt")
      # (def path "17.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
