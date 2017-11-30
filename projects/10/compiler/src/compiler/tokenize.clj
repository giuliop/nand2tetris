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
  (contains? #{\/ \*} (first (str/trim line))))
(defn remove-whitespace [line]
  "Takes a line, removes comments and leading/trailing whitespaces"
  (if (comment-line? line) ""
    (->> (first (re-find re-without-comment line))
         (str "") ; to make a nil an empty string
         (str/trim))))

(defn tokens [clean-program]
  "Takes a program without comments and returns a list of tokens in the form
  [:token-type token]"
  (let [xs (re-seq #"\w+|[^\w\s\"]|\".*\"" clean-program)]
    (map #(vector (token-type %) %) xs)))

(defn clean [program]
  "Takes a program and removes comments"
  (->> (str/split-lines program)
       (map remove-whitespace)
       (str/join "\n")))

(defn remove-quote [x]
  "Takes a string of a string and removes the start and end quote"
  (apply str (drop-last (drop 1 x))))

(defn transform-tokens [token-type token]
  "Takes a token and transform it if needed to conform to xml output"
  (cond (= token-type "stringConstant") (remove-quote token)
        (= token "<")"&lt;"
        (= token ">") "&gt;"
        (= token "&") "&amp;"
        :else token))

(defn write-xml [filename tokens]
  "Takes a filename and a list of tokens and write a filename.xml file with the
  tokens"
  (let [xml-token (fn [token]
                    (let [[t x] token
                          x (transform-tokens t x)]
                      (str "<" t ">" " " x " " "</" t ">")))
        xml (list '("<tokens>") (map xml-token tokens) '("</tokens>"))]
    (file/write filename xml)))

(defn xml-file [filename]
  "Takes a xxx.jack filename, reads the file and write an xxx-tokens.xml file
   with the tokens in the input file"
  (->> (slurp filename)
       (clean)
       (tokens)
       (write-xml (file/make-token-xml-filename filename))))

;;; TESTING ;;;
(deftest xml-file-test
  (let [test-files [{:file "../Square/Main.jack" :cmp "../Square/MainT.xml"}
                    {:file "../Square/Square.jack" :cmp "../Square/SquareT.xml"}
                    {:file "../Square/SquareGame.jack" :cmp "../Square/SquareGameT.xml"}
                    {:file "../ArrayTest/Main.jack" :cmp "../ArrayTest/MainT.xml"}]
        test-cmp "../../../tools/TextComparer.sh"]
    (doseq [x test-files] (xml-file (:file x))
      (is (= "Comparison ended successfully\n"
             (:out (shell/sh test-cmp (:cmp x)
                             (file/make-token-xml-filename (:file x)))))))))

(deftest remove-whitespace-test
  (is (= (remove-whitespace " 3/2  /** comment  \n") "3/2"))
  (is (= (remove-whitespace "   * comment  \n") ""))
  (is (= (remove-whitespace "// comment  \n") ""))
  (is (= (remove-whitespace "   3 / 2  // comment  \n") "3 / 2")))
