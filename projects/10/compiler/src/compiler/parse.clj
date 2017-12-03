(ns compiler.parse
 (:require [compiler.file :as file]
           [clojure.java.shell :as shell]
           [clojure.test :refer :all]
           [clojure.zip :as zip]
           [clojure.string :as str]))

(defn class-tree []
  "Create an empty class parse tree as a zipper"
  (defn branch? [node]
    (vector? (:value node)))
  (defn children [branch]
    (seq (:value branch)))
  (defn make-node [node children-seq]
    {:type (:type node) :value (into [] children-seq)})
  (zip/zipper branch? children make-node {:type "class" :value []}))

(declare dispatch)

(defn parse-class [tokens]
  "Takes a list of tokens representing a class and returns a parse tree of the
  class"
  (let [tree (-> (class-tree)
                 (zip/append-child (nth tokens 0))    ; class keyword
                 (zip/append-child (nth tokens 1))    ; classname identifier
                 (zip/append-child (nth tokens 2))    ; { symbol
                 (zip/down) (zip/rightmost)
                 (zip/insert-right (last tokens)))    ; } symbol
        tokens (drop-last (drop 3 tokens))]
    ((dispatch (first tokens)) tokens tree)))

(defn parse-classVarDec [tokens tree]
  "Takes tokens to parse starting with class variable declarations and the parse
  tree so far and returns the complete parse tree"
  (let [tree (-> (zip/append-child {:type "classVarDec" :content []})
                 (zip/down)
                 (zip/append-child (nth tokens 0))  ; static or field keyword
                 (zip/append-child (nth tokens 1))  ; type keyword or class name id
                 (zip/append-child (nth tokens 2))) ; var name identifier
        [tree, tokens] (loop [tokens (drop 3 tokens), tree tree]
                         (if (= ";" (:value (first tokens)))
                           [(zip/append-child tree (first tokens)), (rest tokens)]
                           (recur (drop 2 tokens)   ; we have a "," and var name
                                  (-> (zip/append-child tree (first tokens))
                                      (zip/append-child tree (first tokens))))))]
    ((dispatch (first tokens)) tokens (zip/up tree))))

(def dispatch {"static" parse-classVarDec
               "field" parse-classVarDec
               ;"constructor" parse-subroutineDec
               ;"function" parse-subroutineDec
               ;"method" parse-subroutineDec
               ;"var" parse-varDec
               ;"let" parse-letStatement
               ;"if" parse-ifStatement
               ;"while" parse-whileStatement
               ;"do" parse-doStatement
               ;"return" parse-returnStatement
               })

;;; TESTING
(def toke
