(ns compiler.xml
  (:require [compiler.file :as file]
            [clojure.zip :as zip]))

;;; xml helper for the tokenizer

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
    (str (open-tag (:type node)) " " (:value node) " "
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
