(ns vm2asm.table)

; "Initialization code to append at the beginning of the asm file; it does:
; * init stack pointer SP to memory address 256
; * call the function Sys.init"
(def init-code
 (str "@256" "\n"
      "D=A" "\n"
      "@SP" "\n"
      "M=D" "\n" ; init SP
      (call "Sys.init" 0 {})))

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

(defn COPY [from-M to-M]
  "Copies the content of from-M to to-M"
  (str "@" from-M "\n"
       "D=M" "\n"
       "@" to-M "\n"
       "M=D" "\n"))

(defn COPY-WITH-NEG-OFFSET [from-M offset to-M]
  "Copies content of (content in from-M - offset) into to-M"
  (str "@" offset "\n"
       "D=A" "\n"
       "@" from-M "\n"
       "A=M-D" "\n"
       "D=M" "\n"
       "@" to-M "\n"
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
       "@R15" "\n"
       "M=D" "\n"       ; R15 holds the address to pop to
       (POP-DA "D")
       "@R15" "\n"
       "A=M" "\n"
       "M=D" "\n"))

(defn POP-POINTER [arg2]
  (let [reg (case arg2 "0" "THIS", "1" "THAT")]
    (str (POP-DA "D")
         "@" reg "\n"
         "M=D" "\n")))

(defn POP-STATIC [arg2 file-name]
  (str (POP-DA "D")
       "@" file-name "." arg2 "\n"
       "M=D" "\n"))

(defn pop- [arg1 arg2 meta-data]
  (case arg1
    "argument" (POP- "ARG" arg2)
    "local" (POP- "LCL" arg2)
    "this" (POP- "THIS" arg2)
    "that" (POP- "THAT" arg2)
    "temp" (POP- "R5" arg2)
    "pointer" (POP-POINTER arg2)
    "static" (POP-STATIC arg2 (:file-name meta-data))
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

(defn PUSH-STATIC [arg2 file-name]
  (str "@" file-name "." arg2 "\n"
       "D=M" "\n"
       PUSH-D))

(defn PUSH-M [M]
  "Pushes the content of M to the stack"
  (str "@" M "\n"
       "D=M" "\n"
       PUSH-D))

(defn push [arg1 arg2 meta-data]
  (case arg1
    "constant" (str (LOAD-D arg2) PUSH-D)
    "argument" (PUSH- "ARG" arg2)
    "local" (PUSH- "LCL" arg2)
    "this" (PUSH- "THIS" arg2)
    "that" (PUSH- "THAT" arg2)
    "temp" (PUSH- "R5" arg2)
    "pointer" (PUSH-POINTER arg2)
    "static" (PUSH-STATIC arg2 (:file-name meta-data))
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
                (POP-DA "A")
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

(defn build-internal-label [label-name meta-data]
  (str (:func-name meta-data) "$" label-name))

(defn label [arg1 meta-data]
    (str "(" (build-internal-label arg1 meta-data) ")" "\n"))

(defn goto [arg1 meta-data]
  (str "@" (build-internal-label arg1 meta-data) "\n"
       "0;JMP" "\n"))

(defn if-goto [arg1 meta-data]
  (str (POP-DA "D")
       "@" (build-internal-label arg1 meta-data) "\n"
       "D;JNE" "\n"))

(defn function [func-name num-locals meta-data]
  (let [num-locals (bigdec num-locals)]
    (str "(" func-name ")" "\n"
         (if (< 0 num-locals)
           (str "D=0" "\n" (apply str (repeat num-locals PUSH-D)))
           ""))))

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
         "M=D" "\n"   ; SP=ARG+1
         (COPY-WITH-NEG-OFFSET FRAME 1 "THAT")
         (COPY-WITH-NEG-OFFSET FRAME 2 "THIS")
         (COPY-WITH-NEG-OFFSET FRAME 3 "ARG")
         (COPY-WITH-NEG-OFFSET FRAME 4 "LCL")
         "@" RET "\n"
         "A=M" "\n"
         "0;JMP" "\n")))

(defn call [called-f num-args meta-data]
  (let [return-address (str (gensym (:func-name meta-data)))]
    (str (LOAD-D return-address)
         PUSH-D
         (PUSH-M "LCL")
         (PUSH-M "ARG")
         (PUSH-M "THIS")
         (PUSH-M "THAT")

         "@" (+ 5 (bigdec num-args)) "\n"
         "D=A" "\n"
         "@SP" "\n"
         "D=M-D" "\n"
         "@ARG" "\n"
         "M=D" "\n"   ; ARG = SP-nArgs-5

         (COPY "SP" "LCL")
         "@" called-f "\n"
         "0;JMP" "\n"
         "(" return-address ")" "\n")))

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
               "call" call
               "return" return
               nil #(str)
               })

