(ns vm2asm.parse
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(def re-without-comment #"([^//]*)(?://.*)?")
(def re-vm-line #"^(\S*)\s*(\S*)\s*(\S*)")

(def valid-commands ["add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not"
               "pop" "push" "label" "goto" "if-goto" "function" "call" "return"])

(defn remove-whitespace [line]
  "Takes a line, removes comments and leading/trailing whitespaces"
  (->> (re-matches re-without-comment line)
       (last)
       (str "")
       (str/trim)))

(defn tokenize [line]
  "Takes a line and returns a map with the following fields:
  :cmd (a string with the command or nil)
  :arg1 (a string with the first arg or nil)
  :arg2 (a string with the second arg or nil)
  "
  (let [line (remove-whitespace line)
        [cmd arg1 arg2] (rest (re-matches re-vm-line line))]
    {:cmd (some #{cmd} valid-commands)
     :arg1 (if-not (empty? arg1) arg1 nil)
     :arg2 (if-not (empty? arg2) arg2 nil)
     }))

(defn functions-from-file [file-name]
  "Takes a filename, opens it and returns a collection of all the funtions in it
  as maps {:file-name :func-name :vm-lines}"
  (let [func-dict (fn [func-string]
                    {:file-name file-name
                     :func-name (last (re-find #"(?:^|\n)function (\S*)" func-string))
                     :vm-lines (str/split-lines func-string)})]
    (->> (slurp file-name)
         (re-seq #"(?s)(?:\n|\A)function.*?(?=\nfunction|\z)")
         (map func-dict))))

  (defn f [func-string]
                    {:file-name "name"
                     :func-name (last (re-find #"(?:^|\n)function (\S*)" func-string))
                     :vm-lines (str/split-lines func-string)})
; TESTING ;;;
(deftest tokenize-test
  (is (= {:cmd nil :arg1 nil :arg2 nil} (tokenize "")))
  (is (= {:cmd nil :arg1 "arg1" :arg2 "arg2"} (tokenize "invalid arg1 arg2")))
  (is (= {:cmd "push" :arg1 "arg1" :arg2 "arg2"} (tokenize "push arg1 arg2")))
  )
