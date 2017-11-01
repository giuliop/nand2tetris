(ns vm2asm.core
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [vm2asm.parse :as parse]
            [vm2asm.table :as table]
            ))

(defn translate-f [filename vm-line]
  "Takes a vm string and returns its translation into an asm string"
  (let [{:keys [cmd arg1 arg2]} (parse/tokenize vm-line)
        cmd-f (get table/commands cmd)]
    (cond arg2 (cmd-f arg1 arg2 filename)
          arg1 (cmd-f arg1 filename)
          :else (cmd-f))))

(defn translate-to-asm [filename vm-lines]
  (conj (map (partial translate-f filename) vm-lines) table/init))

(defn rename-to-asm [filename]
  "Change the suffix of file to .asm"
  (let [radix (re-find #".*\." filename)]
    (if (nil? radix) (str filename ".asm")
      (str radix "asm"))))

(defn write-file [filename lines]
  "Takes a filename and a seq of lines and writes them to disk"
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [line lines
          :when (not (empty? line))]
      (.write w line))))

(defn radix [filename]
  "Takes a filename and remove directory path and extension"
  (let [radix (last (re-matches #"^(?:.*/)?(.*?)(?:\.[^.]*$)?" filename))]
    radix))

(defn vm-to-asm [filename]
  "Takes a .vm file and outputs the assembly code in a same name .asm file"
  (->> (slurp filename)
       (str/split-lines)
       (translate-to-asm (radix filename))
       (write-file (rename-to-asm filename))))

(deftest radix-test
  (is (= "radix" (radix "radix")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix.")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix.ext")))
  (is (= "radix.notext" (radix "../dir1/dir2/dir3/radix.notext.ext"))))
