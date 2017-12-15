(ns compiler.tokenize
 (:require [compiler.file :as file]
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
  "Returns whether the line is a comment line"
  (contains? #{\/ \*} (first (str/trim line))))

(defn remove-whitespace [line]
  "Takes a line, removes comments and leading/trailing whitespaces"
  (if (comment-line? line) ""
    (->> (first (re-find re-without-comment line))
         (str "") ; to make a nil an empty string
         (str/trim))))

(defn remove-quote [x]
  "Takes a quoted string and removes the start and end quote"
  (apply str (drop-last (drop 1 x))))

(defn transform-token [t]
  "Takes a token value and applies any needed transformation"
  (cond (= (:type t) "stringConstant") (assoc t :value (remove-quote (:value t)))
        :else t))

(defn make-token [line-num value]
  "Returns a token as a map of line-num, token type and token value"
  (-> (hash-map :line (inc line-num) :type (token-type value) :value value)
      (transform-token)))

(defn tokens [filename]
  "Takes a .jack filename, tokenizes it, and outputs the list of tokens
  in the form {:type _, :value _ :line _}"
  (transduce (comp
               (map remove-whitespace)
               (map #(re-seq #"\w+|[^\w\s\"]|\".*\"" %))             ; the tokens per line
               (map-indexed (fn [index token-seq] (map (partial make-token index) token-seq))))
             concat
  (->> (slurp filename) (str/split-lines))))

