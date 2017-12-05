(ns compiler.tokenize
 (:require [compiler.file :as file]
           [compiler.xml :as xml]
           [clojure.java.shell :as shell]
           [clojure.test :refer :all]
           [clojure.string :as str]))

(def keywords #{"class" "constructor" "function" "method" "field" "static"
                "var" "int" "char" "boolean" "void" "true" "false" "null"
                "this" "let" "do" "if" "else" "while" "return"})

(def symbols #{"{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">"
               "=" "~"})

(defn _keyword? [t]
  (contains? keywords t))

(defn _symbol? [t]
  (contains? symbols t))

(defn _integerConstant? [t]
  (if-let [n (re-matches #"\d+" t)]
    (<= 0 (bigint n) 32767)))

(defn _stringConstant? [t]
  (re-matches #"\A\"[^\"\n]*\"\z" t))

(defn _identifier? [t]
  (re-matches #"\A[a-zA-Z_.$:][a-zA-Z_.$:\d]*\z" t))

(defn token-type [t]
  (cond (_keyword? t) "keyword"
        (_symbol? t) "symbol"
        (_integerConstant? t) "integerConstant"
        (_stringConstant? t) "stringConstant"
        (_identifier? t) "identifier"
        :else "error"))

(def re-without-comment #"\A([^/]|/(?![/*]))*")
(defn comment-line? [line]
  (contains? #{\/ \*} (first (str/trim line))))
(defn remove-whitespace [line]
  "Takes a line, removes comments and leading/trailing whitespaces"
  (if (comment-line? line) ""
    (->> (first (re-find re-without-comment line))
         (str "") ; to make a nil an empty string
         (str/trim))))

(defn clean [program]
  "Takes a program and removes comments"
  (->> (str/split-lines program)
       (map remove-whitespace)
       (str/join "\n")))

(defn tokens [filename]
  "Takes a xxx.jack filename, tokenizes it, and outputs the list of tokens
  in the form {:type _, :value _}"
  (->> (slurp filename)
       (clean)
       (re-seq #"\w+|[^\w\s\"]|\".*\"")                   ; the tokens
       (map #(hash-map :type (token-type %), :value %)))) ; ({:type _, :value_},...)

;;; TESTING ;;;
(deftest xml-file-test
  (let [test-files [{:file "../Square/Main.jack" :cmp "../Square/MainT.xml"}
                    {:file "../Square/Square.jack" :cmp "../Square/SquareT.xml"}
                    {:file "../Square/SquareGame.jack" :cmp "../Square/SquareGameT.xml"}
                    {:file "../ArrayTest/Main.jack" :cmp "../ArrayTest/MainT.xml"}]
        test-cmp "../../../tools/TextComparer.sh"]
    (doseq [x test-files]
      (let [filename (file/make-token-xml-filename (:file x))]
        (xml/tokens-to-file filename (tokens (:file x)))
        (is (= "Comparison ended successfully\n"
               (:out (shell/sh test-cmp (:cmp x) filename))))))))

(deftest remove-whitespace-test
  (is (= (remove-whitespace " 3/2  /** comment  \n") "3/2"))
  (is (= (remove-whitespace "   * comment  \n") ""))
  (is (= (remove-whitespace "// comment  \n") ""))
  (is (= (remove-whitespace "   3 / 2  // comment  \n") "3 / 2")))
