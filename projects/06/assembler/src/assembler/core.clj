(ns assembler.core
  (:require [clojure.string :as str]
            [assembler.parse :as parse]
            [assembler.table :as table]
            [clojure.test :refer :all]))

(defn add-labels [processed-lines]
  "Takes asm processed lines and returns an augmented predefined table with labels"
  (let [acc {:table table/predefined, :line-num 0}
        f (fn [acc line]
            (let [label (:label line)]
              (cond label (assoc-in acc [:table label] (:line-num acc))
                    (parse/is-empty line) acc
                    :else (update acc :line-num inc))))]
    (:table (reduce f acc processed-lines))))

(defn pre-process [asm]
  "Takes an asm program as a string, removes whitespaces/comments, creates a symbol
  table adding labels to the predefined table; returns [pre-processed-lines, table]"
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
  (if (not-empty (:a-value line)) (to-binary-16 (bigdec (:a-value line)))
    (to-binary-16 (bigdec (get table (:a-var line))))))

(defn c-line-to-binary [line]
  (str "111"
       (get table/comp-codes (:comp line))
       (get table/dest-codes (:dest line))
       (get table/jump-codes (:jump line))))

(defn add-var [table a-var-token]
  "Takes a table and the a-var token of a line, checks if the a-var token does
  contain a var and if so adds it to the table if not already present"
  (cond (nil? a-var-token) table
        (contains? table a-var-token) table
        :else (-> (assoc table a-var-token (:next table))
                  (update :next inc))))

(defn translate [pre-processed-lines table]
  "Takes pre-processed asm lines and a symbol table and outputs binary lines"
  (let [acc {:table table, :lines []}
        f (fn [{:keys [table lines] :as acc} line]
            (if (parse/is-c-instruction line)
              (update acc :lines conj (c-line-to-binary line))
              (let [table (add-var table (:a-var line))
                    lines (conj lines (a-line-to-binary line table))]
                {:table table, :lines lines})))]
    (:lines (reduce f acc pre-processed-lines))))

(defn name-file [file]
  "Change the suffix of file to .hack"
  (let [radix (re-find #".*\." file)]
    (if (nil? radix) (str file ".hack")
      (str radix "hack"))))

(defn write-file [file lines]
  "Takes a filename and a seq of lines and writes them to disk"
  (with-open [w (clojure.java.io/writer file)]
    (doseq [line lines] (.write w line) (.newLine w))))

(defn assemble [file]
  "Takes an assembly file and outputs the binary code in a same name .hack file"
  (let [asm (slurp file)
        [lines, symbol-table] (pre-process asm)]
    (write-file (name-file file) (translate lines symbol-table))))

;;; TESTING ;;;
(deftest name-file-test
  (is (= "ciao.hack" (name-file "ciao.asm")))
  (is (= "ciao.hack" (name-file "ciao")))
  (is (= "ciao.hello.hack" (name-file "ciao.hello.asm"))))

(deftest pre-process-test
  (let [table (assoc table/predefined "OUTPUT_FIRST" 10
                                      "OUTPUT_D" 12
                                      "INFINITE_LOOP" 14)
        asm (slurp "src/assembler/test_files/test.asm")
        processed-asm (slurp "src/assembler/test_files/pre_processed_test.asm")]
    (is (= (str/split-lines processed-asm) (map :instr (first (pre-process asm)))))
    (is (= table (second (pre-process asm))))))

(deftest a-line-to-binary-test
  (is (= "0000001111100111" (a-line-to-binary (parse/tokenize "@999")
                                              table/predefined)))
  (is (= "0000001111100111" (a-line-to-binary (parse/tokenize "@var")
                                              {"var" "999"}))))

(deftest c-line-to-binary-test
  (is (= "1111110111011000" (c-line-to-binary (parse/tokenize "MD=M+1")))))

(deftest assemble-test
  (doseq [file ["Add", "Max", "Pong", "Rect"]]
    (assemble (str "src/assembler/test_files/" file ".asm"))
    (is (= (slurp (str "src/assembler/test_files/" file ".hack"))
           (slurp (str "src/assembler/test_files/orig_" file ".hack"))))))
