(ns compiler.code
 (:require [compiler.file :as file]
           [compiler.parse :as parse]
           [clojure.test :refer :all]
           [clojure.zip :as zip]
           [clojure.string :as str]))

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

(defn lookup [table var-node]
  "Takes a symbol table and a var node and returns the var map or throws an error"
  (if (nil? table)
    (throw (UnsupportedOperationException.
             (str "Compilation error: unrecognized symbol " (:value var-node)
                  " - line: " (:line var-node))))
    (or (get table (:value var-node)) (lookup (:root table) var-node))))

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

(defn push-keyword [table k]
  "Takes a symbol table and a keyword value and returns a string of vm code that
  pushes it to the stack"
  (case k
    "true" (str "push constant 0" "\n"
                "not" "\n")
    ("false" "null") (str "push constant 0" "\n")
    "this" (str "push pointer 0" "\n")))

(defn lookup-class-name [table]
  "Takes a symbol table and returns the class-name of the table"
  (if (nil? (:root table)) (:classname table)
    (recur (:root table))))

(defn lookup-num-class-fields [table]
  "Takes a symbol table and returns the number of fields of the class"
  (if (nil? (:root table)) (:field table)
    (recur (:root table))))

(defn lookup-func-type [table]
  "Takes a symbol table and returns the type of the func we are in"
  (:func-type table))

(declare push-expression)
(declare push-or-pop-var)
(defn push-subroutineCall [table tree]
  "Takes a symbol table and a tree representing a subroutineCall to be pushed to the
  stack and returns the equivalent vm code"
  (let [tokens (:value tree)
        dot? (= "." (:value (second tokens)))
        a-var (and dot? (try (lookup table (first tokens))
                             (catch UnsupportedOperationException e nil)))
        class-name (cond a-var (:type a-var)
                         dot? (:value (first tokens))
                         :else (lookup-class-name table))
        func-name (if dot? (:value (nth tokens 2)) (:value (first tokens)))
        exp-list (:value (last (drop-last tokens)))
        num-params (+ (count (remove #(= "," (:value %)) exp-list))
                      (if (and dot? (not a-var)) 0 1))]
    (str (cond (= :field (:kind a-var)) (str "push this " (:cont a-var) "\n")
               a-var (str "push " (name (:kind a-var)) " " (:cont a-var) "\n")
               (not dot?) (str "push pointer 0" "\n")
               :else "")
         (transduce (comp (map :value)
                          (remove #(= "," %))
                          (map (partial push-expression table)))
                    str exp-list)
         "call " class-name "." func-name " " num-params "\n")))

(defn push-term [table tree]
  "Takes a symbol table and a tree representing a term to be pushed to the stack and
  returns the equivalent vm code"
  (let [{:keys [type value] :as _1st} (nth (:value tree) 0)
        _2nd (nth (:value tree) 1 nil)
        _3rd (nth (:value tree) 2 nil)]
    (case type
      "integerConstant" (str "push constant " value "\n")
      "stringConstant" (push-string-constant value)
      "keyword" (push-keyword table value)
      "identifier" (cond (some #{(:value _2nd)} ["(" "."]) (push-subroutineCall table tree)
                         (= "[" (:value _2nd)) (push-or-pop-var "push" table _1st _3rd)
                         :else (push-or-pop-var "push" table _1st))
      "symbol" (if (= "(" value) (push-expression table (:value _2nd))
                 (str (push-term table _2nd)
                      (unary-op _1st)))
      nil "")))

(defn push-expression [table exp-value]
  "Takes a symbol table and the value of an expression tree to be pushed to the
  stack and returns a string of vm code to that effect"
  (let [[term op & xs] exp-value
        code (push-term table term)]
    (if-not op code
      (str code (push-expression table xs) (binary-op op)))))

(defn push-or-pop-var
  ([push-or-pop table var-node]
   "Takes a 'push' or 'pop', a symbol table, and a var node and returns a string
   of vm code that pushes or pops it to the stack"
   (let [{:keys [kind cont]} (lookup table var-node)]
     (if (= :field kind) (str push-or-pop " this " cont "\n")
       (str push-or-pop " " (name kind) " " cont "\n"))))

  ([push-or-pop table var-node array-exp]
   "With a fourth arg uses it as array index"
   (let [{:keys [kind cont]} (lookup table var-node)]
     (if (= :field kind) (str (push-or-pop "push" table var-node)
                              (push-expression table (:value array-exp))
                              "add" "\n"
                              "pop pointer 1" "\n"
                              push-or-pop " that 0" "\n")
       (str "push " (name kind) " " cont "\n"
            (push-expression table (:value array-exp))
            "add" "\n"
            "pop pointer 1" "\n"
            push-or-pop " that 0" "\n")))))

(defn compile-if [table tree]
  "Takes a function symbol table and a tree representing an if statement and returns
  a string of compiled vm code"
  (declare compile-statement)
  (let [pred-exp-value (:value (nth (:value tree) 2))
        if-statements (:value (nth (:value tree) 5))
        if-end-label (gensym "if-end-label")
        else-statements (when (> (count (:value tree)) 7) (:value (nth (:value tree) 9)))
        else-label (gensym "else-label")]
    (str (push-expression table pred-exp-value)
         "push constant 0" "\n"
         "eq" "\n"
         "if-goto " else-label "\n"
         (transduce (map (partial compile-statement table)) str if-statements)
         "goto " if-end-label "\n"
         "label " else-label "\n"
         (transduce (map (partial compile-statement table)) str else-statements)
         "label " if-end-label "\n")))

(defn compile-let [table tree]
  "Takes a function symbol table and a tree representing a let statement and returns
  a string of compiled vm code"
  (let [tokens (:value tree)
        dest (second tokens)
        array-exp (when (= "[" (:value (nth tokens 2))) (nth tokens 3))
        push-value (push-expression table (:value (last (drop-last tokens))))]
    (str push-value
         (if array-exp (push-or-pop-var "pop" table dest array-exp)
           (push-or-pop-var "pop" table dest)))))

(defn compile-while [table tree]
  "Takes a function symbol table and a tree representing a while statement and
  returns a string of compiled vm code"
  (let [pred-exp-value (:value (nth (:value tree) 2))
        statements (:value (nth (:value tree) 5))
        true-label (gensym "while-true-label")
        false-label (gensym "while-false-label")]
    (str "label " true-label "\n"
         (push-expression table pred-exp-value)
         "push constant 0" "\n"
         "eq" "\n"
         "if-goto " false-label "\n"
         (transduce (map (partial compile-statement table)) str statements)
         "goto " true-label "\n"
         "label " false-label "\n")))

(defn compile-do [table tree]
  "Takes a function symbol table and a tree representing a do statement and
  returns a string of compiled vm code"
  (let [func (drop-last (drop 1 (:value tree)))]
    (str (push-subroutineCall table {:type "subroutineCall" :value func})
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
  (case (:type tree)
    "letStatement" (compile-let table tree)
    "ifStatement" (compile-if table tree)
    "whileStatement" (compile-while table tree)
    "doStatement" (compile-do table tree)
    "returnStatement" (compile-return table tree)
    nil ""))

(defn vars-from [body]
  "Takes a tree representing a subroutine body and returns a list of var declarations
  as a hash-map with keys :type, :name, and :kind"
  (transduce (comp
               (filter #(= "varDec" (:type %)))
               (map :value)
               (map (partial map :value))
               (map (partial drop 1))   ; drop var keyword
               (map (partial drop-last)); drop ; at the end
               (map (partial filter #(not= "," %)))
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
     :statements (:value (first (filter #(= "statements" (:type %)) body)))
     }))

(defn compile-subroutineDec [classname class-table tree]
  "Takes a classname, a class symbol table and a tree representing a subroutineDec and
  retruns a string of compiled vm code"
  (let [{:keys [func-type ret-type func-name params body vars statements]}
              (parse-subroutineDec tree)
        table (as-> {:root class-table :argument 0 :local 0 :func-type func-type} table
                    (assoc table :argument (if (= "method" func-type) 1 0))
                    (transduce (map #(hash-map :name (:name %) :type (:type %)
                                               :kind (keyword (:kind %))))
                               (completing add-symbol) table (concat params vars)))]
    (str "function " classname "." func-name " " (count vars) "\n"
         (when (= "new" func-name)
           (str "push constant " (lookup-num-class-fields class-table) "\n"
                "call Memory.alloc 1" "\n"
                "pop pointer 0" "\n"))
         (when (= "method" func-type)
           (str "push argument 0" "\n"
                "pop pointer 0" "\n"))
         (transduce (map (partial compile-statement table)) str statements))))

(defn compile-class [tree]
  "Takes a tree representing a parsed jack class and returns a string of compiled vm code"
  (let [classname (parse/classname tree)
        class-vars (parse/class-variables tree)
        class-funcs (parse/class-functions tree)
        class-table (as->
                      {:root nil :classname classname :funcs {} :static 0 :field 0} table
                      (reduce compile-classVarDec table class-vars))]
    (transduce (map (partial compile-subroutineDec classname class-table))
               str class-funcs)))

(defn generate-vm [filename]
  "Takes a string of jack code and translate it into a string of vm code"
  (let [tree-or-error (parse/tree filename)]
    (if (string? tree-or-error)
      (println "Cannot compile file " filename "\n" tree-or-error)
      (try (compile-class tree-or-error)
           (catch UnsupportedOperationException e (println (.getMessage e) e))))))

(defn vm [file-or-dir]
  (doseq [file (file/list-files ".jack" file-or-dir)
        :let [vm-filename (file/rename-to-vm file)
              vm-code (generate-vm file)]]
    (when vm-code
      (file/write-string vm-filename vm-code))))


;;; TESTING ;;;
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
    (def b body)
    ))

(deftest test-parse-subroutineDec
  (let [tree (-> (parse/tree "../ConvertToBin/Main.jack"))
        class-vars (parse/class-variables tree)
        class-funcs (parse/class-functions tree)
        subroutineDec (nth class-funcs 1)
        sym {:name "x" :kind :field :type "int"}
        {:keys [func-type ret-type func-name params body vars statements] :as f}
                (parse-subroutineDec subroutineDec)]
    ;(clojure.pprint/pprint f)
    (is (= {:func-type "function" :ret-type "void" :func-name "convert"
            :params '({:name "value" :kind "argument" :type "int"})
            :vars '({:name "mask" :kind "local" :type "int"}
                    {:name "position" :kind "local" :type "int"}
                    {:name "loop" :kind "local" :type "boolean"})}
           (dissoc (parse-subroutineDec subroutineDec) :body :statements)))))

(deftest test-lookup
  (let [table {:root {"x" 1, :root nil} "y" 2}]
    (is (= 2 (lookup table {:value "y"}))
        (= 1 (lookup table {:value "x"})))))

(defn compile-test-files []
  (let [test-dirs [
                   "../Seven/"
                   "../ConvertToBin/"
                   "../Square/"
                   "../Average/"
                   "../Pong/"
                   "../ComplexArrays/"
                   ]]
    (doseq [dir test-dirs]
      ;(println "compiling...." dir)
      (vm dir))))

(deftest refactor-vm
  "Compares reference vm files to new compilation to support refactoring"
  (let [test-programs [
                   {:dir "Seven/" :ref-files ["Main.vm"]}
                   {:dir "ConvertToBin/" :ref-files ["Main.vm"]}
                   {:dir "Square/" :ref-files ["Main.vm" "Square.vm" "SquareGame.vm"]}
                   {:dir "Average/" :ref-files ["Main.vm"]}
                   {:dir "Pong/" :ref-files ["Main.vm" "Bat.vm" "Ball.vm" "PongGame.vm"]}
                   {:dir "ComplexArrays/" :ref-files ["Main.vm"]}]
        gensym-words #{"if-end-label" "else-label" "while-true-label" "while-false-label"}
        is-digit? #(<= (int \0) (int %) (int \9))
        remove-end-digits (fn [word] (apply str (take-while #(not (is-digit? %)) word)))
        word-cmp? (fn [[w1 w2]] (or (= w1 w2)
                                  (let [w1 (remove-end-digits w1)
                                        w2 (remove-end-digits w2)]
                                    (and (= w1 w2)
                                         (every? #(contains? gensym-words %) [w1 w2])))))
        line-cmp? (fn [l1 l2] (let [ws1 (str/split l1 #"\s+")
                                    ws2 (str/split l1 #"\s+")]
                                (every? word-cmp? (partition 2 (interleave ws1 ws2))))) ]
    (doseq [{:keys [dir ref-files]} test-programs
            file ref-files
            :let [new-f (str "../" dir file)
                  ref-f (str "src/compiler/test/reference/" dir file)]]
      ;(println "comparing " new-f " to " ref-f)
      (vm (str "../" dir))
      (is (= nil (file/first-different-line new-f ref-f line-cmp?))))))

