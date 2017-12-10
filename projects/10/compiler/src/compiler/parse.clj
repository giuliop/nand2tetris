(ns compiler.parse

 (:require [compiler.file :as file]
           [compiler.tokenize :as tokenize]
           [compiler.xml :as xml]
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

(defn error? [tokens expected]
  "Takes a list of tokens and a list of expected tokens and check if the former
  starts with the latter, returning nil if so or an error message otherwise"
  (let [f (fn [t e]
            (let [exp_types (if-not (coll? (first e)) [(name (first e))]
                              (map name (first e)))
                  exp_values (if-not (coll? (second e)) [(second e)] (second e))]
              (if (and (some #{(:type t), "!"} exp_types)
                       (some #{(:value t), "!"} exp_values))
                nil
                (str "Syntax error. Expecting: " [:type exp_types :value exp_values]
                     " - " "Found: " t "\n"))))
        expected (partition 2 expected)
        num_tokens (count expected)
        tokens (take num_tokens tokens)]
    (some identity (map f tokens expected))))

(defn consume [[tokens tree] & expected]
  "Takes a list of tokens, a tree where to add the tokens and a list of expected
  tokens to consume. Returns a vector of two elements: the list of tokens wihtout
  the consummed one and the updated tree. Updates the tree with append-child"
  (let [num_tokens (count (partition 2 expected))]
    (if-let [e (error? tokens expected)] (throw (Exception. e))
      [(drop num_tokens tokens)
       (reduce zip/append-child tree (take num_tokens tokens))])))

(defn consume-* [[tokens tree] & expected]
  "Like the function consume but can consume zero or more instances of expected"
    (if (error? tokens expected) [tokens tree]
      (recur (apply (partial consume [tokens tree]) expected) expected)))

(defn dispatch-if [[tokens tree] pred]
  "Takes a [tokens tree] and a predicate for the value of the first token. If true
  dispatches based on the next token, otherwise returns [tokens tree] unchanged"
  (declare dispatch)
  (let [v (:value (first tokens))]
    (if (pred v) ((dispatch v) [tokens tree])
      [tokens tree])))

(defn tag-down [[tokens tree] tag-type]
  "Takes a [tokens tree] and a tag string, creates a node with :type tag
  and moves down to it"
  [tokens (-> tree
              (zip/append-child {:type tag-type :value []})
              (zip/down) (zip/rightmost))])

(defn move-up [[tokens tree]]
  "Moves up the tree and returns [tokens tree]"
  [tokens (zip/up tree)])

(defn parse-class [tokens]
  "Takes a list of tokens representing a class and returns the parse tree"
  (-> [tokens (zipper-tree {:type "class" :value []})]
      (consume :keyword "class"  :identifier "!"  :symbol "{")
      (dispatch-if #(not= "}" %))
      (consume :symbol "}")
      (second)))

(defn parse-classVarDec [[tokens tree]]
  "Takes tokens to parse starting with class variable declarations and the parse
  tree so far and returns the complete parse tree"
  (-> [tokens tree]
      (tag-down "classVarDec")
      (consume :keyword ["static" "field"]
               [:keyword, :identifier] ["int" "char" "boolean" "!"]
               :identifier "!")
      (consume-* :symbol ","  :identifier "!")
      (consume :symbol ";")
      (move-up)
      (dispatch-if #(not= "}" %))))

(defn parse-varDec-* [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming zero or more varDec"
  (if (error? tokens [:keyword "var"])
    [tokens tree]
    (recur (-> [tokens tree]
               (tag-down "varDec")
               (consume :keyword "var"
                        [:keyword, :identifier] ["int" "char" "boolean" "!"]
                        :identifier "!")
               (consume-* :symbol ","  :identifier "!")
               (consume :symbol ";")
               (move-up)))))

(defn parse-statement-* [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming zero or more statements"
  (if (error? tokens [:keyword ["if" "while" "do" "let" "return"]])
    [tokens tree]
    (recur ((dispatch (:value (first tokens))) [tokens tree]))))

(defn parse-statements [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a statements block"
  (-> [tokens tree]
      (tag-down "statements")
      (parse-statement-*)
      (move-up)))

(defn parse-subroutineBody [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a subroutine body"
  (-> [tokens tree]
      (tag-down "subroutineBody")
      (consume :symbol "{")
      (parse-varDec-*)
      (parse-statements)
      (consume :symbol "}")
      (move-up)))

(defn parse-parameterList-? [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a paramaeter list"
  (let [[tokens tree] (tag-down [tokens tree] "paramaeterList")]
    (if (error? tokens [:symbol "("])
      [tokens tree]
      (-> (consume [tokens tree] [:keyword, :identifier] ["int" "char" "boolean" "!"]
                   :identifier "!")
          (consume-* :symbol "," [:keyword, :identifier] ["int" "char" "boolean" "!"]
                     :identifier "!")
          (move-up)))))

(defn parse-subroutineDec [[tokens tree]]
  "Takes tokens to parse starting with class subroutine declarations and the parse
  tree so far and returns the complete parse tree"
  (-> [tokens tree]
      (tag-down "subroutineDec")
      (consume :keyword ["constructor" "function" "method"]
               [:keyword, :identifier] ["void" "int" "char" "boolean" "!"]
               :identifier "!" :symbol "(")
      (parse-parameterList-?)
      (consume :symbol ")")
      (parse-subroutineBody)
      (move-up)
      (dispatch-if #(not= "}" %))))

; refactored up to here

(defn is-op [token]
  (contains? #{"+" "-" "*" "/" "&" "|" "<" ">" "="} (:value token)))

(defn parse-term [tokens tree])

(defn parse-expression [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming an expression"
  (let [tree (-> tree
                 (zip/append-child {:type "expression" :value []})
                 (zip/down) (zip/rightmost))
        [tokens tree] (parse-term tokens tree)]
    (loop [[tokens tree] [tokens tree]]
      (if-not (is-op (first tokens))
        [tokens (zip/up tree)]
        (recur (parse-expression (drop 1 tokens)
                                 (zip/append-child tree (first tokens)))))))) ; op

(defn parse-letStatement [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a letStatement"
  (let [tree (-> tree
                 (zip/append-child {:type "letStatement" :value []})
                 (zip/down) (zip/rightmost)
                 (zip/append-child (nth tokens 0))  ; let keyword
                 (zip/append-child (nth tokens 1)))
        [tokens tree]
        (if (= (:value (nth tokens 2)) "[")      ; array
          (let [[tokens tree] (parse-expression (drop 3 tokens)
                                                (zip/append-child tree (nth tokens 2)))]
                [(drop 1 tokens) (zip/append-child tree (nth tokens 0))]); ]
          [(drop 2 tokens) tree])
        [tokens tree] (parse-expression (drop 1 tokens)
                                        (zip/append-child tree (nth tokens 0)))] ; =
    ((dispatch (:value (second tokens))) (drop 1 tokens)
     (-> tree (zip/append-child (first tokens)) (zip/up tree)))))  ; ;

(defn parse-ifStatement [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a ifStatement"
  (let [tree (-> tree
                 (zip/append-child {:type "ifStatement" :value []})
                 (zip/down) (zip/rightmost)
                 (zip/append-child (nth tokens 0))  ; if keyword
                 (zip/append-child (nth tokens 1)))  ; (
        [tokens tree] (parse-expression (drop 2 tokens) tree)
        [tokens tree] (parse-Statements (drop 2 tokens)
                                    (-> tree (zip/append-child (first tokens)) ; )
                                        (zip/append-child (second tokens))))   ; {
        [tokens tree] [(drop 1 tokens) (zip/append-child (first tokens))]      ; }
        [tokens tree] (if (= (:value (first tokens)) "else")
                        (let [[tokens tree]
                          (parse-Statements (drop 2 tokens)
                                            (-> tree (zip/append-child (first tokens)) ; else
                                                (zip/append-child (second tokens))))] ; {
                          [(drop 1 tokens) (zip/append-child (first tokens) tree )]) ; }
                        [tokens tree])]
    ((dispatch (:value (first tokens))) tokens (zip/up tree))))

(defn parse-whileStatement [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a whileStatement"
  (let [tree (-> tree
                 (zip/append-child {:type "whileStatement" :value []})
                 (zip/down) (zip/rightmost)
                 (zip/append-child (nth tokens 0))  ; while keyword
                 (zip/append-child (nth tokens 1)))  ; (
        [tokens tree] (parse-expression (drop 2 tokens) tree)
        [tokens tree] (parse-Statements (drop 2 tokens)
                                    (-> tree (zip/append-child (first tokens)) ; )
                                        (zip/append-child (second tokens))))   ; {
        [tokens tree] [(drop 1 tokens) (zip/append-child (first tokens))]]      ; }
    ((dispatch (:value (first tokens))) tokens (zip/up tree))))

(defn parse-expressionList [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming an expressionList"
    (let [tree (-> tree
                   (zip/append-child {:type "expressionList" :value []})
                   (zip/down) (zip/rightmost))
          [tokens tree] (if (= (:value (first tokens)) ")") [tokens tree]
                          (let [[tokens tree] (parse-expression tokens tree)]
                            (loop [[tokens tree] [tokens tree]]
                              (if (= (:value (first tokens)) ",")
                                (recur (parse-expression (drop 1 tokens)
                                                         (zip/append-child (first tokens)))) ; ,
                                [tokens tree]))))]
      ((dispatch (:value (first tokens))) tokens (zip/up tree))))


(defn parse-subroutineCall [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a subroutineCall"
  (let [tree (-> tree
                 (zip/append-child {:type "subroutineCall" :value []})
                 (zip/down) (zip/rightmost))
        [tokens tree] (if (= (:value (second tokens) ".")) ; it's a class method
                        [(drop 2 tokens) (-> tree (zip/append-child (first tokens)) ; class|var name
                                             (zip/append-child (second tokens)))] ; .
                        [tokens tree])
        [tokens tree] (parse-expressionList (drop 2 tokens) (-> tree
                                                                (zip/append-child (first tokens)) ; subroutine name
                                                                (zip/append-child (second tokens)))) ; (
        [tokens tree] [(drop 1 tokens) (zip/append-child tree (first tokens))]] ; )
    ((dispatch (:value (first tokens))) tokens (zip/up tree))))


(defn parse-doStatement [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a doStatement"
  (let [tree (-> tree
                 (zip/append-child {:type "doStatement" :value []})
                 (zip/down) (zip/rightmost)
                 (zip/append-child (nth tokens 0))) ; do keyword
        [tokens tree] (parse-subroutineCall (drop 1 tokens) tree)]
    ((dispatch (:value (second tokens))) (drop 1 tokens)
     (-> tree (zip/append-child (first tokens)) (zip/up tree)))))   ; ;

(defn parse-returnStatement [tokens tree]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a returnStatement"
  (let [tree (-> tree
                 (zip/append-child {:type "returnStatement" :value []})
                 (zip/down) (zip/rightmost)
                 (zip/append-child (nth tokens 0))) ; return keyword
        [tokens tree] (if-not (= (:value (first tokens)) ";")
                        (parse-expression tokens tree)
                        [tokens tree])]
    ((dispatch (:value (second tokens))) (drop 1 tokens)
     (-> tree (zip/append-child (first tokens)) (zip/up tree)))))   ; ;


(def dispatch {"static" parse-classVarDec
               "field" parse-classVarDec
               "constructor" parse-subroutineDec
               "function" parse-subroutineDec
               "method" parse-subroutineDec
               "var" parse-varDec
               "let" parse-letStatement
               "if" parse-ifStatement
               "while" parse-whileStatement
               "do" parse-doStatement
               "return" parse-returnStatement
               })

(defn parse-tree [filename]
  "Takes a xxx.jack filename, tokenizes it, and outputs the (unzipped) parse tree"
  (->> filename
       (tokenize/tokens)
       (parse-class)
       (zip/root)))


;;; TESTING
(deftest test-parsers
  (let [test-files [{:file "src/compiler/test/class.jack"
                     :cmp "src/compiler/test/class.xml"}
                    {:file "src/compiler/test/classVarDec.jack"
                     :cmp  "src/compiler/test/classVarDec.xml"}
                    ]
        test-cmp "../../../tools/TextComparer.sh"]
    (doseq [x test-files]
      (let [filename (file/make-parse-xml-filename (:file x))]
        (xml/tree-to-file filename (zipper-tree (parse-tree (:file x))))
        (is (= "Comparison ended successfully\n"
               (:out (shell/sh test-cmp (:cmp x) filename))))))))
