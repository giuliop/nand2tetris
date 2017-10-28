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

(defn POP- [dest]
  (str SP--
       "@SP" "\n"
       "A=M" "\n"
       dest "=M" "\n"))

(defn LOAD-D [n]
  (str "@" n "\n"
       "D=A" "\n"))

(defn push [arg1 arg2]
  (case arg1
    "constant" (str (LOAD-D arg2)
                    PUSH-D)
    ))

(defn binary-op [op]
  (fn [] (str (POP- "D")
              (POP- "A")
              "D=A" op "D" "\n"
              PUSH-D)))

(defn unary-op [op]
  (fn [] (str (POP- "D")
       "D=" op "D" "\n"
       PUSH-D)))

(defn test-op [op]
  (fn [] (let [asm-op (case op
                        "=" "JEQ"
                        "<" "JLT"
                        ">" "JGT")
               true-label (gensym "true")
               continue-label (gensym "continue")]
           (str (POP- "D")
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
               nil #(str)
               })



