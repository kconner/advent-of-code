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
       (peg/match ~{:main (/ (* :registers "\n" :program)
                             ,|{:registers $0 :program $1 :ip 0})
                    :registers (/ (* :register :register :register)
                                  ,|@{:a $0 :b $1 :c $2})
                    :register (* "Register " (range "AC") ": " :int "\n")
                    :program (/ (* "Program: " :int (some (* "," :int)) "\n") ,tuple)
                    :int (/ (<- (some (choice "-" (range "09")))) ,scan-number)})))

(defn problem1 [model])

(defn problem2 [model])

(defn main [&]
  (spork/test/timeit
    (do
      (def path "17.txt")
      (def path "17.test.txt")
      (def model (model-from-file path))
      (print (problem1 model))
      (print (problem2 model)))))
