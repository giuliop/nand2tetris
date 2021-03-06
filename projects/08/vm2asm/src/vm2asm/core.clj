(ns vm2asm.core
  (:require [clojure.test :refer :all]
            [clojure.java.shell :as shell]
            [vm2asm.file :as file]
            [vm2asm.parse :as parse]
            [vm2asm.table :as table]))

(defn translate-line [meta-data vm-line]
  "Takes a vm line as a string and meta-data with file-name and function-name and
  returns its translation into an asm string"
  (let [{:keys [func-name file-name]} meta-data
        {:keys [cmd arg1 arg2]} (parse/tokenize vm-line)
        cmd-f (get table/commands cmd)]
    (cond arg2 (cmd-f arg1 arg2 meta-data)
          arg1 (cmd-f arg1 meta-data)
          :else (cmd-f))))

(defn translate-functions [vm-functions]
  "Takes a collection of vm-functions and returns a collection of asm strings"
  (map (fn [vm-func]
         (let [{:keys [func-name file-name vm-lines]} vm-func]
           (transduce (map (partial translate-line (dissoc vm-func :vm-lines)))
                      str
                      vm-lines)))
       vm-functions))

(defn vm-to-asm [file-or-dir-name]
  "Takes a directory containing .vm files (or a single .vm file) and outputs
  a single .asm file named as the dir (or the file)"
  (as-> (file/list-vm-files file-or-dir-name) x
    (map parse/functions-from-file x)
    (map translate-functions x)
    (conj x (list (table/init-code))) ; a collection of collections of strings
    (file/write (file/rename-to-asm file-or-dir-name) x)))


;;; TESTING ;;;
(deftest vm-to-asm-test
  (let [test-files [{:dir "../FunctionCalls/FibonacciElement/"
                     :test "../FunctionCalls/FibonacciElement/FibonacciElement.tst"}
                    {:dir  "../FunctionCalls/StaticsTest/"
                     :test "../FunctionCalls/StaticsTest/StaticsTest.tst"}]
        emulator "../../../tools/CPUEmulator.sh"]
    (doseq [x test-files] (vm-to-asm (:dir x))
                          (is (= "End of script - Comparison ended successfully\n"
                                 (:out (shell/sh emulator (:test x))))))))

