(ns vm2asm.table)

(def init (str "@256" "\n" ; stack pointer init'ed to memory address 256
               "D=A" "\n"
               "@SP" "\n"
               "M=D" "\n"))

(def SP++ (str "@SP"   "\n"
               "M=M+1" "\n"))

(def SP-- (str "@SP"   "\n"
               "M=M-1" "\n"))

(def PUSH-D (str "@SP" "\n"
                 "A=M" "\n"
                 "M=D" "\n"
                 SP++))

(defn LOAD-D [n]
  (str "@" n "\n"
       "D=A" "\n"))

(defn COPY [from to]
  (str "@" from "\n"
       "D=M" "\n"
       "@" to "\n"
       "M=D" "\n"))

(defn COPY-WITH-NEG-OFFSET [from offset to]
  ( str "@" offset "\n"
        "D=A" "\n"
        "@" from
        "D=A-D" "\n"
        "@" to
        "M=D" "\n"))

(defn POP-DA [D-or-A]
  (str SP--
       "@SP" "\n"
       "A=M" "\n"
       D-or-A "=M" "\n"))

(defn POP- [reg arg2]
  (str (LOAD-D arg2)
       "@" reg "\n"
       (when-not (= reg "R5") (str "A=M" "\n")) ; no indirection for TEMP register
       "D=D+A" "\n"
       "@R13" "\n"
       "M=D" "\n"       ; R13 holds the address to pop to
       (POP-DA "D")
       "@R13" "\n"
       "A=M" "\n"
       "M=D" "\n"))

(defn POP-POINTER [arg2]
  (let [reg (case arg2 "0" "THIS", "1" "THAT")]
    (str (POP-DA "D")
         "@" reg "\n"
         "M=D" "\n")))

(defn POP-STATIC [arg2 filename]
  (str (POP-DA "D")
       "@" filename "." arg2 "\n"
       "M=D" "\n"))

(defn pop- [arg1 arg2 filename]
  (case arg1
    "argument" (POP- "ARG" arg2)
    "local" (POP- "LCL" arg2)
    "this" (POP- "THIS" arg2)
    "that" (POP- "THAT" arg2)
    "temp" (POP- "R5" arg2)
    "pointer" (POP-POINTER arg2)
    "static" (POP-STATIC arg2 filename)
    ))

(defn PUSH- [reg arg2]
  (str (LOAD-D arg2)
       "@" reg "\n"
       (when-not (= reg "R5") (str "A=M" "\n")) ; no indirection for TEMP register
       "A=D+A" "\n"
       "D=M" "\n"
       PUSH-D))

(defn PUSH-POINTER [arg2]
  (let [reg (case arg2 "0" "THIS", "1" "THAT")]
    (str "@" reg "\n"
         "D=M" "\n"
         PUSH-D)))

(defn PUSH-STATIC [arg2 filename]
  (str "@" filename "." arg2 "\n"
       "D=M" "\n"
       PUSH-D))

(defn push [arg1 arg2 filename]
  (case arg1
    "constant" (str (LOAD-D arg2) PUSH-D)
    "argument" (PUSH- "ARG" arg2)
    "local" (PUSH- "LCL" arg2)
    "this" (PUSH- "THIS" arg2)
    "that" (PUSH- "THAT" arg2)
    "temp" (PUSH- "R5" arg2)
    "pointer" (PUSH-POINTER arg2)
    "static" (PUSH-STATIC arg2 filename)
    ))

(defn binary-op [op]
  (fn [] (str (POP-DA "D")
              (POP-DA "A")
              "D=A" op "D" "\n"
              PUSH-D)))

(defn unary-op [op]
  (fn [] (str (POP-DA "D")
       "D=" op "D" "\n"
       PUSH-D)))

(defn test-op [op]
  (fn [] (let [asm-op (case op
                        "=" "JEQ"
                        "<" "JLT"
                        ">" "JGT")
               true-label (gensym "true")
               continue-label (gensym "continue")]
           (str (POP-DA "D")
                (POP- "A")
                "D=A-D" "\n"
                "@" true-label "\n"
                "D;" asm-op "\n"
                "D=0" "\n"
                PUSH-D
                "@" continue-label "\n"
                "0;JMP" "\n"
                "(" true-label ")" "\n"
                "D=-1" "\n"
                PUSH-D
                "(" continue-label ")" "\n"))))

(defn build-label [label-name filename]
  (str filename "_" label-name))

*** wrong, need to use funcion name, not filename ***
(defn label [arg1 filename]
    (str "(" (build-label arg1 filename) ")" "\n"))

(defn goto [arg1 filename]
  (str "@" (build-label arg1 filename) "\n"
       "0;JMP" "\n"))

(defn if-goto [arg1 filename]
  (str (POP-DA "D")
       "@" (build-label arg1 filename) "\n"
       "D;JNE" "\n"))

(defn function [func-name num-locals filename]
  (str "(" func-name ")" "\n"
       "D=0" "\n"
       (apply str (repeat (bigdec num-locals) PUSH-D))))

(defn return []
  (let [FRAME "R13"
        RET "R14"]
    (str (COPY "LCL" FRAME)
         (COPY-WITH-NEG-OFFSET FRAME 5 RET)
         (POP- "ARG" 0)
         "@ARG" "\n"  ; make SP=ARG+1
         "D=M" "\n"   ;
         "D=D+1" "\n" ;
         "@SP" "\n"   ;
         "M=D" "\n"   ; SP=ARG+!
         (COPY-WITH-NEG-OFFSET FRAME 1 "THAT")
         (COPY-WITH-NEG-OFFSET FRAME 2 "THIS")
         (COPY-WITH-NEG-OFFSET FRAME 3 "ARG")
         (COPY-WITH-NEG-OFFSET FRAME 4 "LCL")
         "@RET" "\n"
         "0;JMP" "\n")))

(def commands {"push" push
               "pop" pop-
               "add" (binary-op "+")
               "sub" (binary-op "-")
               "neg" (unary-op "-")
               "and" (binary-op "&")
               "or" (binary-op "|")
               "not" (unary-op "!")
               "eq" (test-op "=")
               "gt" (test-op ">")
               "lt" (test-op "<")
               "label" label
               "goto" goto
               "if-goto" if-goto
               "function" function
               ;"call" call
               "return" return
               nil #(str)
               })

