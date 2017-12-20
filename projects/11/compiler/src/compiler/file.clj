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

(defn rename-to-vm [file-or-dir-name]
  "If given a filename ending in .jack returns a name with the suffix changed
  to .vm, otherwise appends .vm to the name"
  (let [radix-jack-file (last (re-find #"(.*)\.jack" file-or-dir-name))]
    (if radix-jack-file (str radix-jack-file ".vm")
      (str file-or-dir-name ".vm"))))

(defn ext-file? [ext file-name]
  (= ext (apply str (take-last (count ext) file-name))))

(defn add-slash-dir [dir-name]
  (if (= \/ (last dir-name)) dir-name
                    (str dir-name "/")))

(defn add-path-to-filename [path file-name]
    (str (add-slash-dir path) file-name))

(defn list-files [ext file-or-dir-name]
  "If given a file name returns it if its extension is ext, if given a
  directory name returns the names of all the .ext files in it"
  (let [ext-file? (partial ext-file? ext)]
    (when (.exists (io/file file-or-dir-name))
      (if (.isDirectory (io/file file-or-dir-name))
        (map (partial add-path-to-filename file-or-dir-name)
             (filter ext-file? (.list (io/file file-or-dir-name))))
            (when (ext-file? file-or-dir-name) (list file-or-dir-name))))))






(defn remove-path-from-vm-filename [file-name]
  (last (re-find #"([^/]*.vm)$" file-name)))

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
