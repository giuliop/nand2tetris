(ns assembler.core
  (:require [clojure.string :as str]
            [assembler.parse :as parse]
            [clojure.test :refer :all]))

(def predefined-table
  {"R0" 0
   "R1" 1
   "R2" 2
   "R3" 3
   "R4" 4
   "R5" 5
   "R6" 6
   "R7" 7
   "R8" 8
   "R9" 9
   "R10" 10
   "R11" 11
   "R12" 12
   "R13" 13
   "R14" 14
   "R15" 15
   "SP" 0
   "LCL" 1
   "ARG" 2
   "THIS" 3
   "THAT" 4
   "SCREEN" 16384
   "KEYBOARD" 24576})

(defn add-labels [processed-lines]
  "Takes asm processed lines and returns an augmented predefined table with the
  labels added"
  (let [acc {:table predefined-table, :line-num 0}
        f (fn [acc line]
            (if-let [label (:label line)]
              (assoc-in acc [:table label] (:line-num acc))
              (if-not (parse/is-empty line)
                (update acc :line-num inc)
                acc)))]
    (:table (reduce f acc processed-lines))))

(defn pre-process [asm]
  "Take an asm program, removes whitespaces/comments, create a symbol table adding
  labels to the predefined table; return [processed-lines, table]"
  (let [processed-lines (map parse/labels (str/split-lines asm))
        table (add-labels processed-lines)]
    [(remove parse/is-empty processed-lines) table]))

(defn translate [pre-processed-lines table]
  "Take an asm program split into lines with whitespaces already removed and
  a symbol table with labels already added and translate it to a binary program"
  )

(defn name-file [file]
  "Change the suffix of file to .hack"
  (let [radix (re-find #".*\." file)]
    (if (nil? radix) (str file ".hack")
      (str radix "hack"))))

(defn assemble [file]
  "Take an input assembly file .asm and output the assembled binary code in a .hack
  file"
  (let [asm (slurp file)
        [pre-processed-asm, table] (pre-process asm)]
    (spit (name-file file) (translate pre-processed-asm table))))

;;; TESTING ;;;
(deftest name-file-test
  (is (= "ciao.hack" (name-file "ciao.asm")))
  (is (= "ciao.hack" (name-file "ciao")))
  (is (= "ciao.hello.hack" (name-file "ciao.hello.asm")))
)

(deftest pre-process-test
  (let [table (assoc predefined-table "OUTPUT_FIRST" 10
                                      "OUTPUT_D" 12
                                      "INFINITE_LOOP" 14)
        asm (slurp "src/assembler/test.asm")
        processed-asm (slurp "src/assembler/pre_processed_test.asm")]
    (is (= (str/split-lines processed-asm)
           (map :instr (first (pre-process asm)))))
    (is (= table
          (second (pre-process asm))))))

