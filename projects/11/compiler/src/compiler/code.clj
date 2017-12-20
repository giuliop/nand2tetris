(ns compiler.code
 (:require [compiler.file :as file]
           [compiler.tokenize :as tokenize]
           [compiler.parse :as parse]
           [clojure.test :refer :all]
           [clojure.zip :as zip]
           [clojure.string :as str]))

(defn generate-vm [code]
  )

(defn vm [file-or-dir]
  (for [f (file/list-files ".jack" file-or-dir)
        :let [vm-f (file/rename-to-vm f)]]
    (->> (slurp f)
        (generate-vm)
        (file/write-string vm-f))))
