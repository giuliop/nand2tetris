(ns vm2asm.core
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [vm2asm.parse :as parse]
            [vm2asm.table :as table]
            ))

(def test_dir "./src/vm2asm/test_files")

;(defn translate-to-asm [vm-lines]
  ;"Takes a seq of vm lines and translate them to asm lines"
  ;(let [acc [[] config]
        ;f (fn [acc line] ())]
    ;(first (reduce f acc vm-lines))))

(defn translate-f [vm-line]
  "Takes a vm string and returns its translation into an asm string"
  (let [{:keys [cmd arg1 arg2]} (parse/tokenize vm-line)
        cmd-f (get table/commands cmd)]
    (cond arg2 (cmd-f arg1 arg2)
          arg1 (cmd-f arg1)
          :else (cmd-f))))

(defn translate-to-asm [vm-lines]
  (conj (map translate-f vm-lines) table/init))

(defn name-file [file]
  "Change the suffix of file to .asm"
  (let [radix (re-find #".*\." file)]
    (if (nil? radix) (str file ".asm")
      (str radix "asm"))))

(defn write-file [file lines]
  "Takes a filename and a seq of lines and writes them to disk"
  ;lines)
  (with-open [w (clojure.java.io/writer file)]
    (doseq [line lines
          :when (not (empty? line))]
      (.write w line) (.newLine w))))

(defn vm-to-asm [file]
  "Takes a .vm file and outputs the assembly code in a same name .asm file"
  (->> (slurp file)
       (str/split-lines)
       (translate-to-asm)
       (write-file (name-file file))))

