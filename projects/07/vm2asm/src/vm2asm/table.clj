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
  (str "@SP" "\n"
       "A=M" "\n"
       dest "=M" "\n"
       SP--))

(defn LOAD-D [n]
  (str "@" n "\n"
       "D=A" "\n"))

(defn push [arg1 arg2]
  (case arg1
    "constant" (str (LOAD-D arg2)
                    PUSH-D)
    ))

(def commands {"push" push
               "add" #(str (POP- "D")
                           (POP- "A")
                           "D=D+A" "\n"
                           PUSH-D)
               nil #(str)
               })



