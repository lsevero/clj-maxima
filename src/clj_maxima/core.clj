(ns clj-maxima.core
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk postwalk-demo]]
            [clojure.test :refer [is]]
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

(def ^:const +maxima-defsystem+
  (str +maxima-folder+ (System/getProperty "file.separator") "defsystem.lisp"))

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
      (let [current-package (-> '*package* ^org.armedbear.lisp.Package (cl/getvar) .getName keyword)]
        (cl/with-cl
          '(in-package :maxima)
          `(setf *maxima-tempdir* ~(System/getProperty "java.io.tmpdir"))
          `(in-package ~current-package)))
      (log/info "Maxima imported to the Common Lisp environment successfully.")
      (cl/cl-load-resource "clj-maxima-utils.lisp")
      :ok
      ))

(defn meval
  "Eval a maxima string.
  Applies the ABCLJ Clojurifiable protocol, so any primitive lisp data (Common lisp ints, doubles, floats, strings) will be converted to its java counterpart,
  notice that the Clojurifiable protocol does not convert cl-cons natively.
  To disable the automatic cl->clj convertion set the :cl param to true.
  "
  [^String s & {:keys [cl] :or {cl false}}]
  (let [f (if cl identity cl/cl->clj)]
    (-> (str "#$ " s " $")
        cl/cl-evaluate
        f)))

(let [cl-displa (cl/getfunction 'clj-maxima-utils/displa-to-string)]
  (defn displa
    "Receives a list of maxima lisp symbols, pretty print the argument in human readable format.
    Similar to the results you get on a Maxima REPL.
    If it does no receive a lisp list will return the argument.

    Returns the pretty printed string
    "
    [coll]
    (if (cl/cl-obj? coll)
      (cl/cl->clj (cl/funcall cl-displa coll))
      coll)))

(def mevald
  "Eval a maxima string, then convert it to human readable format and return it.
  " 
  (comp displa meval))

(def mevalp
  "Eval a maxima string, then convert it to human readable format and print it.
  Useful on the REPL.
  " 
  (comp println displa meval))

(def displap
  "Pretty print a maxima expression and print"
  (comp println displa))

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

(defn ->maxima
  "If it is a symbol adds the namespace 'maxima' to a clojure symbol.
  If it is a sequential recursively adds the namespace 'maxima' to all symbols.
  if it is neither will return the argument
  useful to build cl-cons"
  [s]
  (cond (symbol? s) (symbol "maxima" (name s))
        (sequential? s) (postwalk #(if (symbol? %)
                                     (->maxima %)
                                     %)
                                  s)
        :else s))

(defn mlist
  "Create maxima lists using variadic arguments, use (apply mlist [...]) to convert sequentials."
  [& args]
  (let [ans [['mlist nil]]]
    (cl/cl-cons (->maxima (conj (into ans args) nil)))))

(def ^:const +mexpr-mappings+
  {'+ 'mplus
   '/ 'rat
   '* 'mtimes
   '** 'mexpt
   '. 'mnctimes
   '= 'mequal
   (symbol "#") 'mnotequal
   (symbol "^") 'mexpt
   (symbol "^^") 'mncexpt
   ; We skip some steps when building maxima expressions directly through CL conses.
   ; Some native functions will not be applied and will be treated as variables, we need to evaluate them to their '''real''' value.
   '$log '%log
   '$sin '%sin
   '$cos '%cos
   '$tan '%tan
   '$cot '%cot
   '$sec '%sec 
   '$csc '%csc
   '$sinh '%sinh 
   '$cosh '%cosh 
   '$tanh '%tanh
   '$coth '%coth 
   '$sech '%sech 
   '$csch '%csch
   '$asin '%asin 
   '$acos '%acos
   '$atan '%atan
   '$acot '%acot
   '$asec '%asec
   '$acsc '%acsc
   '$asinh '%asinh 
   '$acosh '%acosh 
   '$atanh '%atanh
   '$acoth '%acoth 
   '$asech '%asech
   '$acsch '%acsch
   '$round '%round 
   '$truncate '%truncate
   '$plog '%plog
   '$signum '%signum 
   '$gamma '%gamma
   })

(defn mexpr
  "Create maxima expressions using clojure primitives
  A aditional map of mappings can be passed which will be merged with +mexpr-mappings+.
  Notice that there is no '-' operator in the lisp syntax, the '-' operator on maxima syntax is always evaluated to ((mtimes) -1 $x)
  "
  ([coll mappings]
   (-> (postwalk (fn [node]
                         (cond (symbol? node) (if-let [ans (get (merge +mexpr-mappings+ mappings) node)]
                                                ans
                                                node)
                               (sequential? node) (let [[head & tail] node
                                                        ans [[head nil]]]
                                                    (conj (into ans tail) nil))
                               :else node
                               )) coll)
       ->maxima
       cl/cl-cons))
  ([coll]
   (mexpr coll {})))

(defn funcall 
  "A shortcut to (cl/funcall (cl/getfunction 'maxima/f) ... )
  Will automatically add the namespace 'maxima' to the symbol and automatically convert all arguments to the common lisp correspondent class, if it is a clojure sequential, will apply mexpr.
  If the function does do exists will raise an exception.
  "
  [s & args]
  (letfn [(dispatch [x]
            (if (sequential? x)
              (mexpr x)
              (-> x ->maxima cl/clj->cl)))]
   (let [f (try (-> s ->maxima cl/getfunction)
               (catch Exception e
                 (throw (ex-info (str "The function " s " does not exist on package :MAXIMA" {:ex e
                                                                                              :function s})))))]
    (cl/cl->clj (apply cl/funcall f (map dispatch args))))))

(comment (displa (mlist 1 2 3))
         (prn (meval "a ^^ b"))
         (postwalk-demo '[/ [* 1 2 3] 99])
         (prn (meval " sin (x/(x^2 + x)) = exp ((log(x) + 1)^2 - log(x)^2)"))
         (prn (meval "a#b"))
         (cl/funcall)
         (prn (cl/funcall (cl/getfunction 'maxima/$diff)
                          (mexpr '[/ [* 1 2 3] 99])
                          (cl/cl-symbol 'maxima/$x)))
         (print (displa (funcall '$diff '[** $x 3] '$x)))
         (funcall '$plot2d '[** $x 3] (mlist '$x -10 10))
         (displap (funcall '$solve '[= 0 [+ 1 [** $%e [* $x $%i]]]] x))
         (displap (funcall '$solve '[= 0 [* [+ [$f $x] -1] [$asin [$cos [* 3 $x]]]]] '$x))
         (prn (mexpr '[* [+ [$f $x] -1] [$asin [$cos [* 3 $x]]]]))
         (prn (meval "asin (cos (3*x))*(f(x) - 1)"))
         (displap (meval "solve(asin (cos (3*x))*(f(x) - 1)=0,x)"))
         (mevalp "%e**(%i*%pi)")
         
         )
