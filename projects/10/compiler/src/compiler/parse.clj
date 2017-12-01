(ns compiler.parse
 (:require [compiler.file :as file]
           [clojure.java.shell :as shell]
           [clojure.test :refer :all]
           [clojure.string :as str]))

(defn ok-reader [tokens readers]
  "Takes a list of tokens and a list of readers and return the next reader
  for the tokens (or nil)"
  (loop [reader (first readers) , readers (next readers)]
    (cond (reader tokens) reader
          (nil? readers) nil
          :else (recur (first readers) (next readers)))))

(declare next-readers)

(defn tree [tokens]
  "Takes a list of tokens and returns a parse tree of the following form
  where ' meand doublequote:
  ('class' ('keyword' 'class') ('identifier' '...class-name...') etc.)"
  (loop [tree () , tokens tokens , readers [class]]
    (if-let [reader (ok-reader tokens, readers)]
      (let [[tree, tokens] (reader tokens tree)
            readers (next-readers tree)]
        (recur tree tokens readers))
      nil)))
