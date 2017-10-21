(ns assembler.parse
  (:require [clojure.string :as str]))

(def re-label #"^\((\S*)\)")
(def re-a-var #"^@([a-zA-Z_.$:][a-zA-Z_.$:\d]*)")
(def re-a-value #"^@(\d*)")
(def re-dest #"^[AMD]+(?==)")
(def re-jump #"((?<=;)J[GELNM][TQEP])")
(def re-comp #"(?:.*=)?([.^;]+).*")

(defn remove-whitespace [line]
  "Takes a line, removes comments and leading/trailing whitespaces"
  (->> (re-find #"([^//]*)" line)
       (last)
       (str "")
       (str/trim)))

(defn tokenize [line]
  "Takes a line and returns a map with the following fields:
  :label (a string with the label or nil if the line has no label)
  :instr (a string with the instruction with comments removed or empty if a label)
  :type ('a' for a instruction or 'c' for c instruction)
  :a-value (a string with the decimal value of the a instruction if there or empty)
  :a-var (a string with the var name of the a instruction if there or empty)
  :comp (a string with the comp field of the c instruction if there or empty)
  :dest (a string with the dest field of the c instruction if there or empty)
  :jump (a string with the jump field of the c instruction if there or empty)
  "
  (let [line (remove-whitespace line)
        label (last (re-find re-label line))
        i-type (if (= '@' (first line)) 'a' 'c') ]
    {:label label
     :instr (if label "" line)
     :type i-type
     :a-value (last (re-find re-a-value line))
     :a-var (last (re-find re-a-var line))
     :comp (if (= 'a'i-type) "" (last (re-find re-comp line)))
     :dest (if (= 'a'i-type) "" (last (re-find re-dest line)))
     :jump (if (= 'a'i-type) "" (last (re-find re-jump line)))
     }))

(defn is-empty [pre-processed-line]
  "Returns true if the pre-processed line is empty"
  (empty? (:instr pre-processed-line)))

(defn is-a-instruction [pre-processed-line]
  (= 'a' (:type pre-processed-line)))

(defn is-c-instruction [pre-processed-line]
  (= 'c' (:type pre-processed-line)))
