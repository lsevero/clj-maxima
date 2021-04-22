(defpackage :clj-maxima-utils
  (:use :cl)
  (:import-from :maxima :maxima-display))

(in-package :clj-maxima-utils)

(defun displa-to-string (form)
  "Call maxima::displa and return its result as a string"
  (with-output-to-string (str)
    (maxima-display form :stream str)))
