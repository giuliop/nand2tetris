(ns compiler.xml
  (:require [compiler.file :as file]
            [clojure.tokenize :as tokenize]
            [clojure.parse :as parse]
            [clojure.zip :as zip]))

;;; xml helper for the tokenizer

(defn transform-value [token-value]
  "Takes a token-value and transform it if needed to conform to xml output"
  (cond (= token-value "<") "&lt;"
        (= token-value ">") "&gt;"
        (= token-value "&") "&amp;"
        :else token-value))

(defn tokens-to-file [filename tokens]
  "Takes a filename and a list of tokens and writes a filename.xml file with the
  tokens"
  (let [xml-token (fn [token]
                    (let [{t :type, v :value} token
                          v (transform-tokens t v)]
                      (str "<" t ">" " " v " " "</" t ">")))
        xml (list '("<tokens>") (map xml-token tokens) '("</tokens>"))]
    (file/write filename xml)))


;;; xml helpers for parse trees

(defn open-tag [text]
  "Outputs <text>"
  (str "<" text ">"))

(defn close-tag [text]
  "Outputs </text>"
  (str "</" text ">"))

(declare xmlize)
(defn xml-branch [loc]
  "Takes a zipped tree at a branch and outputs its xml code"
  (let [node (zip/node loc)]
    (str (open-tag (:type node))
         "\n"
         (xmlize (zip/down loc))
         (close-tag (:type node))
         "\n")))

(defn xml-node [loc]
  "Takes a zipped tree at a node and outputs its xml code"
  (let [node (zip/node loc)]
    (str (open-tag (:type node)) " " (transform-value (:value node)) " "
         (close-tag (:type node)) "\n")))

(defn xmlize [tree]
  "Takes a zipped parse tree and outputs its xml representation"
  (loop [loc tree, xml ""]
    (if loc
      (recur (zip/right loc)
             (str xml (if (zip/branch? loc) (xml-branch loc) (xml-node loc))))
      xml)))

(defn tree-to-file [filename tree]
  "Takes a zipped parse tree an writes its xml representation to filename"
  (let [xml (xmlize tree)]
    (file/write-string filename xml)))


;;; TESTING ;;;

(deftest xml-tokenize-test
  (let [test-files [{:file "../Square/Main.jack" :cmp "../Square/MainT.xml"}
                    {:file "../Square/Square.jack" :cmp "../Square/SquareT.xml"}
                    {:file "../Square/SquareGame.jack" :cmp "../Square/SquareGameT.xml"}
                    {:file "../ArrayTest/Main.jack" :cmp "../ArrayTest/MainT.xml"}]
        test-cmp "../../../tools/TextComparer.sh"]
    (doseq [x test-files]
      (let [filename (file/make-token-xml-filename (:file x))]
        (xml/tokens-to-file filename (tokenize/tokens (:file x)))
        (is (= "Comparison ended successfully\n"
               (:out (shell/sh test-cmp (:cmp x) filename))))))))

(deftest xml-parsers-test
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
        (xml/tree-to-file filename (parse/zipper-tree (parse-tree x)))
        (is (= "Comparison ended successfully\n"
               (:out (shell/sh test-cmp cmp-file filename))))))))
