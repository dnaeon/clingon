(in-package :cl-user)
(defpackage :clingon.options
  (:use :cl)
  (:export
   :option
   :option-short-name
   :option-long-name
   :option-help
   :option-env-vars
   :option-initial-value
   :option-reduce-fn
   :option-finalize-fn
   :option-key
   :option-category
   :option-value))
(in-package :clingon.options)

(defclass option ()
  ((short-name
    :initarg :short-name
    :initform nil
    :reader option-short-name
    :documentation "Short option name")
   (long-name
    :initarg :long-name
    :initform nil
    :reader option-long-name
    :documentation "Long option name")
   (help
    :initarg :help
    :initform (error "Must specify help message")
    :reader option-help
    :documentation "Usage documentation of the option")
   (env-vars
    :initarg :env-vars
    :initform nil
    :reader option-env-vars
    :documentation "List of env vars which can set the option value")
   (initial-value
    :initarg :initial-value
    :initform nil
    :reader option-initial-value
    :documentation "Initial value for the option")
   (reduce-fn
    :initarg :reduce-fn
    :initform (error "Must specify a reduce function")
    :reader option-reduce-fn
    :documentation "A function taking two arguments of the prev and current option value")
   (finalize-fn
    :initarg :finalize-fn
    :initform #'identity
    :reader option-finalize-fn
    :documentation "A function to finalize the option value. Takes a single argument.")
   (key
    :initarg :key
    :initform (error "Must specify option key")
    :reader option-key
    :documentation "Key used to associate the option with it's value")
   (category
    :initarg :category
    :initform nil
    :reader option-category
    :documentation "Option category name")
   (value
    :initarg :value
    :initform nil
    :accessor option-value
    :documentation "Computed value after finalizing the option"))
  (:documentation "A class representing a command-line option"))
