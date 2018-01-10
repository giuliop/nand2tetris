(ns compiler.file
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn radix [filename]
  "Takes a filename and remove directory path and extension"
  (let [radix (last (re-matches #"^(?:.*/)?(.*?)(?:\.[^.]*$)?" filename))]
    radix))

(defn path [filename]
  (let [path (last (re-matches #"^(.*/)?(?:.*?)(?:\.[^.]*$)?" filename))]
    path))

(defn first-different-line
  "Takes two files and compares whether they are identical line by line returning
  nil if so or an error message otherwise; if supplied with a line comparator
  uses that for line comparison"
  ([f1 f2]
   (first-different-line f1 f2 =))
  ([f1 f2 line-cmp?]
   (let [stage-file #(->> % (slurp) (str/split-lines))
         lines-1 (stage-file f1)
         lines-2 (stage-file f2)
         error-msg (fn [line-num]
                     (str "Line " line-num " is different" "\n"
                              f1 " -> " (get lines-1 (dec line-num)) "\n"
                              f2 " -> " (get lines-2 (dec line-num)) "\n"))]
     (loop [x lines-1, y lines-2, line-num 1]
       (cond (every? nil? [x y]) nil
             (or (not (line-cmp? (first x) (first y)))
                 (and (nil? x) y)
                 (and (nil? y) x)) (do (println (first x) (first y) line-num) (error-msg line-num))
             :else (recur (next x) (next y) (inc line-num)))))))


(deftest radix-test
  (is (= "radix" (radix "radix")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix.")))
  (is (= "radix" (radix "../dir1/dir2/dir3/radix.ext")))
  (is (= "radix.notext" (radix "../dir1/dir2/dir3/radix.notext.ext"))))

(deftest path-test
  (is (= nil (path "radix")))
  (is (= "../dir1/dir2/dir3/" (path "../dir1/dir2/dir3/radix")))
  (is (= "../dir1/dir2/dir3/" (path "../dir1/dir2/dir3/radix.")))
  (is (= "../dir1/dir2/dir3/" (path "../dir1/dir2/dir3/radix.ext")))
  (is (= "../dir1/dir2/dir3/" (path "../dir1/dir2/dir3/radix.notext.ext"))))
