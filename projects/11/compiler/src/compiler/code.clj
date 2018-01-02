(ns compiler.code
 (:require [compiler.file :as file]
           [compiler.parse :as parse]
           [clojure.test :refer :all]
           [clojure.zip :as zip]
           [clojure.string :as str]))

(defn create-sys-file [path]
  (let [filename (str path "Sys.vm")
        code (str "function Sys.init 0" "\n"
                  "call Main.main 0" "\n"
                  "label WHILE" "\n"
                  "GOTO WHILE" "\n")]
    (file/write-string filename code)))

(defn add-symbol [table sym]
   "Takes a table and adds the new sym to table and returns the table
   type can be one of int, char, boolean, 'classname'
   kind can be one of argument, local, field, static
   cont starts from 0 and increments by one for each kind"
   (let [{:keys [name kind type]} sym
         cont (kind table)]  ; kind is a keyword
     (assoc table name {:type type :kind kind :cont cont} kind (inc cont))))

(defn compile-classVarDec [table tree]
  "Takes a class symbol table and a parse tree of a class var declaration and
  augments the symbol table which returns"
  (let [nodes (:value tree)
        kind (keyword (:value (first nodes)))
        type (:value (second nodes))
        vars (map :value (take-nth 2 (drop 2 nodes)))]
    (transduce (map #(hash-map :name % :kind kind :type type))
               (completing add-symbol) table vars)))

(defn parameters [tree]
  "Takes a tree representing a subroutineDec and returns a list of the paramenters as
  maps with keys :type, :name, and :kind"
  (->> (:value tree)
       (map :value)
       (filter #(not= "," %))
       (partition 2)
       (map #(hash-map :type (first %) :name (second %) :kind "argument"))))

(defn vars-from [body]
  "Takes a tree representing a subroutine body and returns a list of var declarations
  as a hash-map with keys :type, :name, and :kind"
  (transduce (comp
               (filter #(= "varDec" (:type %)))
               (map :value)
               (map (partial drop 1))   ; drop var keyword
               (filter (not= ","))
               (map (fn [varDec]
                      (let [type (first varDec) vars (rest varDec)]
                        (map #(hash-map :type type :name % :kind "local") vars)))))
             concat
             body))

(defn parse-subroutineDec [tree]
  "Takes a tree representing a subroutineDec and retruns a map of its fields"
  (let [nodes (:value tree)
        body (drop 1 (drop-last (:value (nth nodes 6)))) ; removes { and }]
        vars (vars-from body)]
    {:func-type (:value (nth nodes 0))
     :ret-type (:value (nth nodes 1))
     :func-name (:value (nth nodes 2))
     :params (parameters (nth nodes 4))
     :body body
     :vars vars
     :statements (:value (first (drop (count vars) body)))}))

(defn lookup [table var-node]
  "Takes a symbol table and a var node and returns the var map or throws an error"
  (if (nil? table)
    (throw (UnsupportedOperationException.
             (str "Compilation error: unrecognized symbol " (:value var-node)
                  " - line: " (:line var-node))))
    (get table (:value var-node) (lookup (:root table var-node)))))

(defn unary-op [tree]
  "Takes a tree representing an unary-op and returns the string of equivalent vm code"
  (case (:value tree)
    "-" (str "neg" "\n")
    "~" (str "not" "\n")))

(defn binary-op [tree]
  "Takes a tree representing a binary-op and returns the string of equivalent vm code"
  (case (:value tree)
    "+" (str "add" "\n")
    "-" (str "sub" "\n")
    "*" (str "call Math.multiply 2" "\n")
    "/" (str "call Math.divide 2" "\n")
    "&" (str "and" "\n")
    "|" (str "or" "\n")
    ">" (str "gt" "\n")
    "<" (str "lt" "\n")
    "=" (str "eq" "\n")))

(defn push-string-constant [s]
  "Takes a string value and returns a string of vm code that pushes it to the stack"
  (str "push constant " (count s) "\n"
       "call String.new 1"      "\n"
       (transduce (map #(str "push constant " (int %)   "\n"
                             "call String.appendChar 2" "\n"))
                  str s)))

(defn push-keyword [k]
  "Takes a keyword value and returns a string of vm code that pushes it to the stack"
  (case k
    "true" (str "push constant 0" "\n"
                          "not" "\n")
    ("false" "null") (str "push constant 0" "\n")
    "this" (str "push argument 0" "\n")))

(defn lookup-class-name [table]
  "Takes a symbol table and returns the class-name of the table"
  (if (nil? (:root table)) (:classname table)
    (lookup-class-name (:root table))))

(defn push-subroutineCall [table tree]
  "Takes a symbol table and a tree representing a subroutineCall to be pushed to the
  stack and returns the equivalent vm code"
  (let [tokens (:value tree)
        method? (= "." (:value (second tokens)))
        a-var (and method? (try (lookup table (first tokens))
                                (catch UnsupportedOperationException e nil)))
        class-name (cond a-var (:type a-var)
                         method? (:value (first tokens))
                         :else (lookup-class-name table))
        func-name (if method? (:value (nth tokens 2)) (:value (first tokens)))
        exp-list (:value (last (drop-last tokens)))
        num-params (+ (/ (inc (count exp-list)) 2)
                      (if a-var 1 0))]
    (str (when a-var (str "push " (:kind a-var) " " (:cont a-var) "\n"))
         (transduce (comp :value
                          (remove #(= "," %))
                          (map (partial push-expression table)))
                    str exp-list)
         "call " class-name "." func-name " " num-params "\n")))

(defn push-term [table tree]
  "Takes a symbol table and a tree representing a term to be pushed to the stack and
  returns the equivalent vm code"
  (let [{:keys [type value] :as _1st} (nth (:value tree) 0)
        _2nd (nth (:value tree) 1)
        _3rd (nth (:value tree) 2)]
    (case type
      "integerConstant" (str "push constant " value "\n")
      "stringConstant" (push-string-constant value)
      "keyword" (push-keyword value)
      "identifier" (cond (= "(" (:value _2nd)) (push-subroutineCall table tree)
                         (= "[" (:value _2nd)) (push-or-pop-var "push" table value _3rd)
                         :else (push-or-pop-var "push" table value))
      "symbol" (if (= "(" value) (push-expression table _2nd)
                 (str (push-term table _2nd)
                      (unary-op _1st))))))

(defn push-expression [table exp-value]
  "Takes a symbol table and the value of an expression tree to be pushed to the
  stack and returns a string of vm code to that effect"
  (let [[term op & xs] exp-value
        code (push-term table term)]
    (if-not op code
      (str code (push-expression table xs) (binary-op)))))

(defn push-or-pop-var
  ([push-or-pop table var-node]
   "Takes a 'push' or 'pop', a symbol table, and a var map and returns a string
   of vm code that pushes or pops it to the stack"
   (let [{:keys [kind cont]} (lookup table (:value var-node))]
     (if (= :field kind) (str "push argument 0"         "\n"
                              "pop pointer 0"           "\n"
                              push-or-pop " this " cont "\n")
       (str push-or-pop kind cont))))

  ([push-or-pop table var-node array-exp]
   "With a fourth arg uses it as array index"
   (let [{:keys [kind cont]} (lookup table (:value var-node))]
     (if (= :field kind) (str (push-or-pop "push" table var-node)
                              (push-expression table array-exp)
                              "add"
                              "pop pointer 1" "\n"
                              push-or-pop " that 0" "\n")
       (str "push " kind " " cont "\n"
            (push-expression table array-exp)
            "add"
            "pop pointer 1" "\n"
            push-or-pop " that 0" "\n")))))

(defn compile-let [table tree]
  "Takes a function symbol table and a tree representing a let statement and returns
  a string of compiled vm code"
  (let [dest (second tree)
        array-exp (when (= "[" (nth tree 2)) (nth tree 3))
        push-value (push-expression table (:value (last (drop-last tree))))]
    (str push-value
         (if array-exp (push-or-pop-var "pop" table dest array-exp)
           (push-or-pop-var "pop" table dest)))))

(defn compile-if [table tree]
  "Takes a function symbol table and a tree representing an if statement and returns
  a string of compiled vm code"
  (let [pred-exp-value (:value (nth (:value tree) 2))
        if-statements (nth (:value tree) 5)
        if-label (gensym "if-label")
        else-statements (when (> (count (:value tree)) 7) (nth (:value tree) 9))
        else-label (gensym "else-label")]
    (str (push-expression table pred-exp-value)
         "push constant 0" "\n"
         "eq"
         "if-goto " else-label "\n"
         (transduce (map (partial compile-statement table)) str if-statements)
         "goto " if-label "\n"
         "label " else-label "\n"
         (transduce (map (partial compile-statement table)) str else-statements)
         "label " if-label "\n")))

(defn compile-while [table tree]
  "Takes a function symbol table and a tree representing a while statement and
  returns a string of compiled vm code"
  (let [pred-exp-value (:value (nth (:value tree) 2))
        statements (nth (:value tree) 5)
        true-label (gensym "while-true-label")
        false-label (gensym "while-false-label")]
    (str "label " true-label "\n"
         (push-expression table pred-exp-value)
         "push constant 0" "\n"
         "eq"
         "if-goto " false-label "\n"
         (transduce (map (partial compile-statement table)) str statements)
         "goto " true-label "\n"
         "label " false-label "\n")))

(defn compile-do [table tree]
  "Takes a function symbol table and a tree representing a do statement and
  returns a string of compiled vm code"
  (let [func (second (:value tree))]
    (str (push-subroutineCall table func)
         "pop temp 0" "\n")))

(defn compile-return [table tree]
  "Takes a function symbol table and a tree representing a return statement and
  returns a string of compiled vm code"
  (let [ret-exp-value (:value (second (drop-last (:value tree))))]
    (str (if ret-exp-value (push-expression table ret-exp-value)
           (str "push constant 0" "\n"))
         "return" "\n")))

(defn compile-statement [table tree]
  "Takes a function symbol table and a tree representing a statement and returns a
  string of compiled vm code"
  (case (first tree)
    "let" (compile-let table tree)
    "if" (compile-if table tree)
    "while" (compile-while table tree)
    "do" (compile-do table tree)
    "return" (compile-return table tree)))

(defn compile-subroutineDec [classname class-table tree]
  "Takes a classname, a class symbol table and a tree representing a subroutineDec and
  retruns a string of compiled vm code"
  (let [{:keys [func-type ret-type func-name params body vars statements]}
                (parse-subroutineDec tree)
        table (transduce
                     (map #(hash-map :name (:name %) :type (:type %) :kind (keyword (:kind %))))
                     (completing add-symbol)
                     {:root class-table :argument 0 :local 0} (concat params vars))]
    (str "function " classname "." func-name " " (count params) "\n"
         (transduce (map (partial compile-statement table)) str statements)
         )))

(defn compile-class [tree]
  "Takes a tree representing a parsed jack class and returns a string of compiled vm code"
  (let [classname (parse/classname tree)
        class-vars (parse/class-variables tree)
        class-funcs (parse/class-functions tree)
        class-table {:static 0 :field 0 :root nil :classname classname}
        class-table (reduce compile-classVarDec class-table class-vars)]
    (transduce (map (partial compile-subroutineDec classname class-table))
               str class-funcs)))

(deftest test-compile-class
  (let [tree (-> (parse/tree "src/compiler/test/square/Square.jack"))
        class-vars (parse/class-variables tree)
        class-funcs (parse/class-functions tree)
        varDec (first class-vars)
        subroutineDec (first class-funcs)
        sym {:name "x" :kind :field :type "int"}
        {:keys [func-type ret-type func-name params body vars statements]}
                (parse-subroutineDec subroutineDec)]
    (is (= {:static 0 :field 1 "x" {:kind :field :type "int" :cont 0}}
           (add-symbol {:static 0 :field 0} sym)))
    (is (= {:static 0 :field 2 "x" {:kind :field :type "int" :cont 0}
            "y" {:kind :field :type "int" :cont 1} }
           (compile-classVarDec {:static 0 :field 0} varDec)))
    (is (= {:func-type "constructor" :ret-type "Square" :func-name "new"
           :params '({:type "int" :name "Ax" :kind "argument"}
                     {:type "int" :name "Ay" :kind "argument"}
                     {:type "int" :name "Asize" :kind "argument"})
           :vars ()} (dissoc (parse-subroutineDec subroutineDec) :body :statements)))
    ))

(defn generate-vm [filename]
  "Takes a string of jack code and translate it into a string of vm code"
  (let [tree-or-error (parse/tree filename)]
    (if (string? tree-or-error)
      (println "Cannot compile file " filename "\n" tree-or-error)
      (try (compile-class tree-or-error)
           (catch UnsupportedOperationException e (println (.getMessage e)))))))

(defn vm [file-or-dir]
  (create-sys-file (file/path file-or-dir))
  (for [file (file/list-files ".jack" file-or-dir)
        :let [vm-filename (file/rename-to-vm file)
              vm-code (generate-vm file)]]
    (when vm-code
      (file/write-string vm-filename vm-code))))


