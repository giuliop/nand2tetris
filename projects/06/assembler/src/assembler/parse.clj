(ns assembler.parse
  (:require [clojure.string :as str]))

(def re-a-instr #"^@([a-zA-Z_.$:][a-zA-Z_.$:\d]*)")
(def re-label #"^\((\S*)\)")

(defn remove-whitespace [line]
  "Takes a line, removes comments and leading/trailing whitespaces"
  (->> (re-find #"([^//]*)" line)
       (last)
       (str "")
       (str/trim)))

(defn labels [line]
  "Takes a line and returns a map with the following fields:
  :label (a string with the label or nil if the line has no label)
  :instr (a string with the instruction with comments removed or nil if empty)
  "
  (let [line (remove-whitespace line)
        label (last (re-find re-label line))
        instr (if label "" line)]
    {:label label
     :instr instr
     }))

(defn is-empty [pre-processed-line]
  "Returns true if the pre-processed line is empty"
  (empty? (:instr pre-processed-line)))

