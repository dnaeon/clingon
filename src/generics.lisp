(in-package :cl-user)
(defpackage :clingon.generics
  (:use :cl)
  (:export
   :find-short-option
   :find-long-option
   :parse-option))
(in-package :clingon.generics)

(defgeneric find-short-option (object name &key)
  (:documentation "Returns the short option with the given NAME, or NIL otherwise"))

(defgeneric find-long-option (object name &key)
  (:documentation "Returns the long option with the given NAME, or NIL otherwise"))

(defgeneric parse-option (kind object &key)
  (:documentation "Parses an option of the given KIND"))
