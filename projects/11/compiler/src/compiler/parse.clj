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
                (str "Syntax error at line " (:line t) ". Expecting: "
                     [:type exp_types :value exp_values] " - "
                     "Found: " [:type (:type t) :value (:value t)] "\n"))))
        expected (partition 2 expected)
        num_tokens (count expected)
        tokens (take num_tokens tokens)]
    (some identity (map f tokens expected))))

(defn consume [[tokens tree] & expected]
  "Takes a [list of tokens, a tree where to parse the tokens] and a list of
  expected tokens to consume. Returns a [list of tokens not consumed,
  the updated tree]. Updates the tree with append-child"
  (let [num_tokens (count (partition 2 expected))]
    (if-let [e (error? tokens expected)] (throw (UnsupportedOperationException. e))
      [(drop num_tokens tokens)
       (reduce zip/append-child tree (take num_tokens tokens))])))

(defn consume-* [[tokens tree] & expected]
  "Like the function consume but can consume zero or more instances of expected"
    (if (error? tokens expected) [tokens tree]
      (recur (apply consume [tokens tree] expected) expected)))

(defn consume-if [[tokens tree] pred & expected]
  "Takes a [tokens tree] a predicate that test the tokens and some expected
  tokens. If predicate is false returns the unchanged [tokens tree] if true calls
  consume for tokens and the supplied parse function for parse-functions"
  (if-not (pred tokens)
    [tokens tree]
    (loop [[tokens tree] [tokens tree], expected expected]
      (let [[k v & other] expected]
        (case k
          nil [tokens tree]
          :parse-func (recur (v [tokens tree]) other)
          (recur (consume [tokens tree] k v) other))))))

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
  (let [[tokens tree] (tag-down [tokens tree] "parameterList")]
    (if-not (error? tokens [:symbol ")"])
      (move-up [tokens tree])
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

(defn op? [token]
  (contains? #{"+" "-" "*" "/" "&" "|" "<" ">" "="} (:value token)))

(defn parse-expression [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming an expression"
  (declare parse-term)
  (loop [[tokens tree] (-> [tokens tree] (tag-down "expression") (parse-term))]
     (if-not (op? (first tokens))
      (move-up [tokens tree])
      (recur (-> [tokens tree]
                 (consume :symbol ["+" "-" "*" "/" "&" "|" "<" ">" "="])
                 (parse-term))))))

(defn parse-letStatement [[tokens tree]]
  "Takes tokens to parse [and tree so far and returns tokens not consumed and
  updated tree consuming a letStatement"
  (-> [tokens tree]
      (tag-down "letStatement")
      (consume :keyword "let" :identifier "!")
      (consume-if #(not (error? % [:symbol "["])) :symbol "["
                 :parse-func parse-expression :symbol "]")
      (consume :symbol "=")
      (parse-expression)
      (consume :symbol ";")
      (move-up)))

(defn parse-ifStatement [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a ifStatement"
  (-> [tokens tree]
      (tag-down "ifStatement")
      (consume :keyword "if" :symbol "(")
      (parse-expression)
      (consume :symbol ")" :symbol "{")
      (parse-statements)
      (consume :symbol "}")
      (consume-if #(not (error? % [:keyword "else"])) :keyword "else"
                 :symbol "{" :parse-func parse-statements :symbol "}")
      (move-up)))

(defn parse-whileStatement [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a whileStatement"
  (-> [tokens tree]
      (tag-down "whileStatement")
      (consume :keyword "while" :symbol "(")
      (parse-expression)
      (consume :symbol ")" :symbol "{")
      (parse-statements)
      (consume :symbol "}")
      (move-up)))

(defn parse-expressionList [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming an expressionList"
  (let [[tokens tree] (-> [tokens tree] (tag-down "expressionList"))]
    (if-not (error? tokens [:symbol ")"])
      (move-up [tokens tree])
      (loop [[tokens tree] (parse-expression [tokens tree])]
        (if (error? tokens [:symbol ","])
          (move-up [tokens tree])
          (recur (-> [tokens tree] (consume :symbol ",") (parse-expression))))))))

(defn parse-subroutineCall [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a subroutineCall"
  (-> [tokens tree]
      (consume-if #(not (error? % [:identifier "!" :symbol "."]))
                  :identifier "!" :symbol ".")
      (consume :identifier "!" :symbol "(")
      (parse-expressionList)
      (consume :symbol ")")))

(defn parse-doStatement [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a doStatement"
  (-> [tokens tree]
      (tag-down "doStatement")
      (consume :keyword "do")
      (parse-subroutineCall)
      (consume :symbol ";")
      (move-up)))

(defn parse-returnStatement [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a returnStatement"
  (-> [tokens tree]
      (tag-down "returnStatement")
      (consume :keyword "return")
      (consume-if #(error? % [:symbol ";"]) :parse-func parse-expression)
      (consume :symbol ";")
      (move-up)))

(defn parse-term [[tokens tree]]
  "Takes tokens to parse and tree so far and returns tokens not consumed and
  updated tree consuming a term"
  (let [[tokens tree] (tag-down [tokens tree] "term")
        cnsm (partial consume [tokens tree])]
    (move-up
    (case (:type (first tokens))
      ("integerConstant" "stringConstant") (cnsm [:integerConstant :stringConstant] "!")
      "keyword" (cnsm :keyword ["true" "false" "null" "this"])
      "identifier" (cond (not (error? (drop 1 tokens) [:symbol "["]))
                         (-> (cnsm :identifier "!" :symbol "[")
                             (parse-expression)
                             (consume :symbol "]"))
                         (not (error? (drop 1 tokens) [:symbol ["(" "."]]))
                         (parse-subroutineCall [tokens tree])
                         :else (cnsm :identifier "!"))
      "symbol" (if (not (error? tokens [:symbol "("]))
                 (-> (cnsm :symbol "(") (parse-expression) (consume :symbol ")"))
                 (parse-term (cnsm :symbol ["-" "~"])))))))

(def dispatch {"static" parse-classVarDec
               "field" parse-classVarDec
               "constructor" parse-subroutineDec
               "function" parse-subroutineDec
               "method" parse-subroutineDec
               "let" parse-letStatement
               "if" parse-ifStatement
               "while" parse-whileStatement
               "do" parse-doStatement
               "return" parse-returnStatement })

(defn parse-tree [filename]
  "Takes a xxx.jack filename, tokenizes it, and outputs the (unzipped) parse tree
  or an error message if there was a syntax error"
  (let [tokens (tokenize/tokens filename)
        tree (try
               (parse-class tokens)
               (catch UnsupportedOperationException e (str (.getMessage e))))]
    (if (string? tree)
      (println tree)
       (zip/root tree))))


;;; TESTING
(deftest test-parsers-xml
  (let [test-files ["src/compiler/test/class.jack"
                    "src/compiler/test/classVarDec.jack"
                    "src/compiler/test/noExpSquare/Main.jack"
                    "src/compiler/test/noExpSquare/Square.jack"
                    "src/compiler/test/noExpSquare/SquareGame.jack"
                    "src/compiler/test/array/Main.jack"
                    "src/compiler/test/square/Main.jack"
                    "src/compiler/test/square/Square.jack"
                    "src/compiler/test/square/SquareGame.jack"
                    ]
        test-cmp "../../../tools/TextComparer.sh"]
    (doseq [x test-files]
      (let [filename (file/make-parse-xml-filename x)
            cmp-file (str (subs x 0 (- (count x) 4)) "xml")]
        (xml/tree-to-file filename (zipper-tree (parse-tree x)))
        (is (= "Comparison ended successfully\n"
               (:out (shell/sh test-cmp cmp-file filename))))))))

