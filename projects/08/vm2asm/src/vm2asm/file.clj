(ns vm2asm.file
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

(defn write [file-name code-coll-of-coll]
  "Takes a filename and a collection of collections of strings and writes them
  to file"
  (with-open [w (clojure.java.io/writer file-name)]
    (doseq [code-coll code-coll-of-coll
            code code-coll
            :when (not (empty? code))]
      (.write w code))))

(defn vm-file? [file-name]
  (= ".vm" (apply str (take-last 3 file-name))))

(defn add-slash-dir [dir-name]
  (if (= \/ (last dir-name)) dir-name
                    (str dir-name "/")))

(defn rename-to-asm [file-or-dir-name]
  "If given a filename ending in .vm returns a name with the suffix changed
  to .asm, otherwise appends .vm to the name (putting it inside the directory if
  the input is a directory)"
  (let [radix (last (re-find #"(.*)\.vm" file-or-dir-name))
        lastdir (last (re-find #"([^/]*)/?$" file-or-dir-name))]
    (cond radix (str radix ".asm")
          (.isDirectory (io/file file-or-dir-name))
             (add-path-to-filename file-or-dir-name (str lastdir ".asm"))
          :else (str file-or-dir-name ".asm"))))

(defn add-path-to-filename [path file-name]
    (str (add-slash-dir path) file-name))

(defn remove-path-form-filename [file-name]
  (last (re-find #"([^/]*.vm)$" file-name)))

(defn list-vm-files [file-or-dir-name]
  "If given a file name returns it if it is a .vm file, if given a
  directory name returns the names of all the .vm files in it"
  (when (.exists (io/file file-or-dir-name))
    (if (.isDirectory (io/file file-or-dir-name))
      (map (partial add-path-to-filename file-or-dir-name)
           (filter vm-file? (.list (io/file file-or-dir-name))))
      (when (vm-file? file-or-dir-name) (list file-or-dir-name)))))

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
