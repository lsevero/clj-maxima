(ns clj-maxima.core
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.string :as str]
            [abclj.core :as cl]
            )
  (:import [java.nio.file Files FileSystem FileSystems Paths Path LinkOption]
           [java.util.zip ZipInputStream ZipEntry]
           [java.net URI]
           ))

(def ^:const +maxima-resource+ "maxima-abcl.zip")

(def ^:private +maxima-input-stream+ (-> +maxima-resource+ io/resource  io/input-stream))

(def +maxima-folder+
  (Paths/get (System/getProperty "java.io.tmpdir")
             (into-array String ["maxima-abcl"])))

(def +maxima-defsystem+
  (str +maxima-folder+ "/defsystem.lisp"))

(defn- unzip
  "Unzips zip archive stream and write all contents to dir"
  [stream ^Path dir]
  (with-open [input (ZipInputStream. stream)]
    (loop [^ZipEntry entry (.getNextEntry input)]
      (when (some? entry)
        (let [^String entry-name (.getName entry)
              resolved-path (->> entry-name
                                 ^Path (.resolve dir)
                                 (.normalize))]
          (when (not (.startsWith resolved-path dir))
            (throw (RuntimeException. (str "Entry with an illegal path: " entry-name))))
          (if (.isDirectory entry)
            (-> resolved-path .toFile .mkdirs)
            (do (-> resolved-path .getParent .toFile .mkdirs)
                (io/copy input (.toFile resolved-path)))))
        (recur (.getNextEntry input))))))

(defn- folder-empty?
  [^Path p]
  (-> (Files/list p) .findAny .isPresent not))

(defn- folder-exist?
  [^Path p]
  (Files/exists p (into-array LinkOption [])))

(defonce ^:private prepare-maxima
  (do (cond
        (not (folder-exist? +maxima-folder+)) (do
                                                (log/debug "Unpacking maxima...")
                                                (unzip +maxima-input-stream+ +maxima-folder+)
                                                (log/debug "Maxima was extracted successfully"))
        (and (folder-exist? +maxima-folder+)
             (folder-empty? +maxima-folder+)) (do
                                                      (log/debug "Maxima folder is empty, deleting it and extracting...")
                                                      (-> +maxima-folder+ str io/file io/delete-file)
                                                      (unzip +maxima-input-stream+ +maxima-folder+)
                                                      (log/debug "Maxima was extracted successfully"))
        :else (log/debug "Maxima is already extracted, ready to use."))
      (cl/cl-load +maxima-defsystem+)
      (cl/with-cl `(mk:add-registry-location ~(str +maxima-folder+)))
      (cl/with-cl '(funcall (intern (symbol-name :operate-on-system) :mk) "maxima" :load :verbose nil))
      (log/info "Maxima imported to the Common Lisp environment successfully.")
      :ok
      ))

(defn meval
  "Eval a maxima string.
  Returns a list of Maxima lisp symbols"
  [^String s]
  (cl/cl-evaluate (str "#$ " s " $")))

(let [cl-displa (cl/getfunction 'maxima/displa)]
  (defn displa
    "Receives a list of maxima lisp symbols, pretty print the argument in human readable format.
    Similar to the results you get on a Maxima REPL.
    
    Returns cl-nil.
    "
    [coll]
    (cl/funcall cl-displa coll)))

(def mevald
  "Eval a maxima string, then convert it to human readable format." 
  (comp displa meval))

(def meval->clj
  "Eval a maxima string, then convert the result to a clojure/java class"
  (comp cl/cl->clj meval))

(defmacro mset
  "Create variables in the maxima environment. Equivalent to `msetq` maxima function."
  [sym value]
  (if (str/starts-with? (str sym) "$")
    (let [current-package (-> '*package* ^org.armedbear.lisp.Package (cl/getvar) .getName keyword)]
      (cl/with-cl
        '(in-package :maxima)
        `(msetq ~sym ~value)
        `(in-package ~current-package))
      value)
    (throw (ex-info "Maxima symbols need to start with a '$'" {:sym sym}))))

