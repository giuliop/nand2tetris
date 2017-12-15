(ns compiler.file
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))


(defn write-string [file-name string]
  "Takes a filename and a string and writes it to file"
  (with-open [w (clojure.java.io/writer file-name)]
      (.write w string )))

(defn write [file-name colls]
  "Takes a filename and a collection of collections of strings and writes them
  to file adding end of lines for each string"
  (with-open [w (clojure.java.io/writer file-name)]
    (doseq [coll colls
            code coll
            :when (not (empty? code))]
      (.write w (str code "\n")))))

(defn make-token-xml-filename [jack-filename]
  "Takes a xxx.jack filename and returns a xxx-tokens.xml filename"
  (str (last (re-find #"(.*)\.jack" jack-filename)) "-tokens.xml"))

(defn make-parse-xml-filename [jack-filename]
  "Takes a xxx.jack filename and returns a xxx-parse.xml filename"
  (str (last (re-find #"(.*)\.jack" jack-filename)) "-parse.xml"))




(defn vm-file? [file-name]
  (= ".vm" (apply str (take-last 3 file-name))))

(defn add-slash-dir [dir-name]
  (if (= \/ (last dir-name)) dir-name
                    (str dir-name "/")))

(defn add-path-to-filename [path file-name]
    (str (add-slash-dir path) file-name))

(defn remove-path-from-vm-filename [file-name]
  (last (re-find #"([^/]*.vm)$" file-name)))

(defn rename-to-asm [file-or-dir-name]
  "If given a filename ending in .vm returns a name with the suffix changed
  to .asm, otherwise appends .vm to the name (putting it inside the directory if
  the input is a directory)"
  (let [radix-vm-file (last (re-find #"(.*)\.vm" file-or-dir-name))
        lastdir (last (re-find #"([^/]*)/?$" file-or-dir-name))]
    (cond radix-vm-file (str radix-vm-file ".asm")
          (.isDirectory (io/file file-or-dir-name))
             (add-path-to-filename file-or-dir-name (str lastdir ".asm"))
          :else (str file-or-dir-name ".asm"))))

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
