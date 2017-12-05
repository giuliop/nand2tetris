(ns compiler.parse
 (:require [compiler.file :as file]
           [compiler.tokenize :as tokenize]
           [clojure.java.shell :as shell]
           [clojure.test :refer :all]
           [clojure.zip :as zip]
           [clojure.string :as str]))

(defn zipper-tree [root]
  "Create a parse tree as a zipper with the input root as root"
  (defn branch? [node]
    (vector? (:value node)))
  (defn children [branch]
    (seq (:value branch)))
  (defn make-node [node children-seq]
    {:type (:type node) :value (into [] children-seq)})
  (zip/zipper branch? children make-node root))

(declare dispatch)

(defn parse-class [tokens]
  "Takes a list of tokens representing a class and returns a parse tree of the
  class"
  (let [tree (-> (zipper-tree {:type "class" :value []})
                 (zip/append-child (nth tokens 0))    ; class keyword
                 (zip/append-child (nth tokens 1))    ; classname identifier
                 (zip/append-child (nth tokens 2))    ; { symbol
                 (zip/down) (zip/rightmost)
                 (zip/insert-right (last tokens)))    ; } symbol
        tokens (drop-last (drop 3 tokens))]
    (if (empty? tokens) tree
      ((dispatch (:value (first tokens))) tokens tree))))

(defn parse-classVarDec [tokens tree]
  "Takes tokens to parse starting with class variable declarations and the parse
  tree so far and returns the complete parse tree"
  (let [tree (-> tree
                 (zip/insert-right {:type "classVarDec" :value []})
                 (zip/right)
                 (zip/append-child (nth tokens 0))  ; static or field keyword
                 (zip/append-child (nth tokens 1))  ; type keyword or class name id
                 (zip/append-child (nth tokens 2))) ; var name identifier
        [tokens tree] (loop [tokens (drop 3 tokens), tree tree]
                         (if (= ";" (:value (first tokens)))
                           [(rest tokens), (zip/append-child tree (first tokens))]
                           (recur (drop 2 tokens)   ; we have a "," and var name
                                  (-> tree
                                      (zip/append-child (first tokens))
                                      (zip/append-child (second tokens))))))]
    (if (empty? tokens) tree
      ((dispatch (:value (first tokens))) tokens tree))))

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

(defn open-tag [text]
  "Outputs <text>"
  (str "<" text ">"))

(defn close-tag [text]
  "Outputs </text>"
  (str "</" text ">"))

(declare xmlize)
(defn xml-branch [loc]
  "Takes a branch and outputs its xml code"
  (let [node (zip/node loc)]
    (str (open-tag (:type node))
         "\n"
         (xmlize (zip/down loc))
         (close-tag (:type node))
         "\n")))

(defn xml-node [loc]
  "Takes a node and outputs its xml code"
  (let [node (zip/node loc)]
    (str (open-tag (:type node)) " " (:value node) " "
         (close-tag (:type node)) "\n")))

(defn xmlize [tree]
  "Takes a zipped parse tree and outputs its xml representation"
  (loop [loc tree, xml ""]
    (if loc
      (recur (zip/right loc)
             (str xml (if (zip/branch? loc) (xml-branch loc) (xml-node loc))))
      xml)))

(defn parse-tree [filename]
  "Takes a xxx.jack filename, tokenizes it, and outputs the (unzipped) parse tree"
  (->> (slurp filename)
       (tokenize/tokens)
       (parse-class)
       (zip/root)))

(defn xml-file [filename]
  "Takes a xxx.jack filename, reads the file, tokenizes it, and write an
  xxx-parse.xml file with the parse tree as xml"
  (let [xml (xmlize (zipper-tree (parse-tree filename)))]
    (file/write-string (file/make-parse-xml-filename filename) xml)))

;;; TESTING
(deftest test-parse-class
  (let [test-files [
                    {:file "src/compiler/test/class.jack"
                    :cmp "src/compiler/test/class.xml"}
                    {:file "src/compiler/test/classVarDec.jack"
                     :cmp  "src/compiler/test/classVarDec.xml"}
                    ]
        test-cmp "../../../tools/TextComparer.sh"]
    (doseq [x test-files] (xml-file (:file x))
      (is (= "Comparison ended successfully\n"
             (:out (shell/sh test-cmp (:cmp x)
                             (file/make-parse-xml-filename (:file x)))))))))
