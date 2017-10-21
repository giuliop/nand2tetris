(ns assembler.core
  (:require [clojure.string :as str]
            [assembler.parse :as parse]
            [assembler.table :as table]
            [clojure.test :refer :all]))

(defn add-labels [processed-lines]
  "Takes asm processed lines and returns an augmented predefined table with the
  labels added"
  (let [acc {:table table/predefined, :line-num 0}
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
  (let [processed-lines (map parse/tokenize (str/split-lines asm))
        table (add-labels processed-lines)]
    [(remove parse/is-empty processed-lines) table]))

(defn to-binary-16 [n]
  "Takes a decimal n and returns its 16-bit representation as a string; it assumes
  the number n fits in 16 bits"
  (let [b (Integer/toBinaryString n)
        pad (- 16 (count b))]
    (str (apply str (repeat pad "0")) b)))

(defn a-line-to-binary [line table]
"Takes an pre-prcessed a-instruction and converts it to binary with the input table"
  (if (not-empty (:a-value line)) (to-binary-16 (:a-value line))
    (to-binary-16 (get table (:a-var line)))))

(defn c-line-to-binary [line]
  (str (get table/comp-codes (:comp line))
       (get table/dest-codes (:dest line))
       (get table/jump-codes (:jump line))))

(defn line-to-binary-with [table]
  "Returns a function that takes a pre-processed asm line and translates it to
  binary with the supplied table"
  (fn [line]
    (if (parse/is-a-instruction line) (a-line-to-binary line table)
      (c-line-to-binary line))))

(defn translate [pre-processed-lines table]
  "Take an asm program split into lines with whitespaces already removed and
  a symbol table with labels already added and translate it to a binary program"
  (map (line-to-binary-with table) pre-processed-lines))

(defn name-file [file]
  "Change the suffix of file to .hack"
  (let [radix (re-find #".*\." file)]
    (if (nil? radix) (str file ".hack")
      (str radix "hack"))))

(defn write-file [file lines]
  "Take a filename and a seq of lines and writes them to disk"
  (with-open [w (clojure.java.writer file)]
    (doseq [line lines] (.write w line) (.newLine w))))

(defn assemble [file]
  "Take an input assembly file .asm and output the assembled binary code in a .hack
  file"
  (let [asm (slurp file)
        [pre-processed-asm, table] (pre-process asm)]
    (write-file (name-file file) (translate pre-processed-asm table))))

;;; TESTING ;;;
(deftest name-file-test
  (is (= "ciao.hack" (name-file "ciao.asm")))
  (is (= "ciao.hack" (name-file "ciao")))
  (is (= "ciao.hello.hack" (name-file "ciao.hello.asm")))
)

(deftest pre-process-test
  (let [table (assoc table/predefined "OUTPUT_FIRST" 10
                                      "OUTPUT_D" 12
                                      "INFINITE_LOOP" 14)
        asm (slurp "src/assembler/test.asm")
        processed-asm (slurp "src/assembler/pre_processed_test.asm")]
    (is (= (str/split-lines processed-asm)
           (map :instr (first (pre-process asm)))))
    (is (= table
          (second (pre-process asm))))))

