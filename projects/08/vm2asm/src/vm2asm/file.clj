(ns vm2asm.file
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

(defn write [file-name asm-code-coll]
  "Takes a filename and a collection of asm code strings and writes them to file"
  (with-open [w (clojure.java.io/writer file-name)]
    (doseq [code asm-code-coll
            :when (not (empty? code))]
      (.write w code))))

(defn vm-file? [file-name]
  (= ".vm" (apply str (take-last 3 file-name))))

(defn rename-to-asm [file-or-dir]
  "If given a name ending in .vm returns a name with the suffix changed
  to .asm, otherwise appends .vm to the name"
  (let [radix (last (re-find #"(.*)\.vm" file-or-dir))]
    (if (nil? radix) (str file-or-dir ".asm")
      (str radix ".asm"))))

(defn list-vm-files [file-or-dir]
  "If given a file name returns it if it is a .vm file, if given a
  directory name returns the names of all the .vm files in it"
  (when (.exists (io/file file-or-dir))
    (if (.isDirectory (io/file file-or-dir))
      (filter vm-file? (.list (io/file file-or-dir)))
      (when (vm-file? file-or-dir) (into [] file-or-dir)))))

(defn radix [filename]
  "Takes a filename and remove directory path and extension"
  (let [radix (last (re-matches #"^(?:.*/)?(.*?)(?:\.[^.]*$)?" filename))]
    radix))

(deftest radix-test
  (is (= "radix" (radix "radix")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix.")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix.ext")))
  (is (= "radix.notext" (radix "../dir1/dir2/dir3/radix.notext.ext"))))
