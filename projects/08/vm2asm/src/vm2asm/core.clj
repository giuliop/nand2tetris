(ns vm2asm.core
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [vm2asm.file :as file]
            [vm2asm.parse :as parse]
            [vm2asm.table :as table]))

(defn translate-line [meta-data vm-line]
  "Takes a vm string and meta-data with file-name and function-name and
  returns its translation into an asm string"
  (let [{:keys [func-name file-name]} meta-data
        {:keys [cmd arg1 arg2]} (parse/tokenize vm-line)
        cmd-f (get table/commands cmd)]
    (cond arg2 (cmd-f arg1 arg2 meta-data)
          arg1 (cmd-f arg1 meta-data)
          :else (cmd-f))))

(defn translate-function [vm-function]
  (let [{:keys [func-name file-name vm-lines]} vm-func]
    (conj (map (partial translate-line (dissoc vm-func :vm-lines) vm-lines)
          table/init-code)))

(defn vm-to-asm [file-or-dir]
  "Takes a directory containing .vm files (or a single .vm file) and outputs
  a single .asm file names as the dir (or the file)"
  (->> (file/list-vm-files file-or-dir)
       (map (parse/functions-from-file)
       (map translate-function)
       (file/write (file/rename-to-asm file-or-dir)))))

