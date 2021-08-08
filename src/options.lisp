(in-package :cl-user)
(defpackage :clingon.options
  (:use :cl)
  (:import-from
   :clingon.generics
   :initialize-option
   :finalize-option
   :make-option)
  (:import-from
   :clingon.conditions
   :invalid-option
   :missing-required-option-value)
  (:export
   :option
   :option-short-name
   :option-long-name
   :option-required-p
   :option-parameter
   :option-help
   :option-env-vars
   :option-initial-value
   :option-key
   :option-category
   :option-value
   :option-is-set-p
   :initialize-option
   :finalize-option
   :derive-option-value
   :make-option
   :end-of-options-p
   :short-option-p
   :long-option-p
   :option-boolean
   :option-boolean-true
   :option-boolean-false
   :option-counter
   :option-list))
(in-package :clingon.options)

(defgeneric initialize-option (option &key)
  (:documentation "Initializes an option, e.g. sets initial option value"))

(defgeneric finalize-option (option &key)
  (:documentation "Finalizes an option, e.g. performs any value transformations"))

(defgeneric derive-option-value (option value &key)
  (:documentation "Derives a new value for the option based on the given VALUE"))

(defgeneric make-option (kind &rest rest)
  (:documentation "Creates a new option of the given kind"))

(defparameter *end-of-options-marker*
  "--"
  "A marker specifying the end of options")

(defun end-of-options-p (arg)
  "A predicate which returns T if the given argument specifies end of options"
  (string= arg *end-of-options-marker*))

(defun short-option-p (arg)
  "A predicate which returns T if the given argument is a short option"
  (and (> (length arg) 1)
       (char= #\- (aref arg 0))
       (char/= #\- (aref arg 1))))

(defun long-option-p (arg)
  "A predicate which returns T if the given argument is a long option"
  (and (> (length arg) 2)
       (char= #\- (aref arg 0))
       (char= #\- (aref arg 1))))

(defclass option ()
  ((parameter
    :initarg :parameter
    :initform nil
    :reader option-parameter
    :documentation "Option takes a parameter identified by the given name")
   (required
    :initarg :required
    :initform nil
    :reader option-required-p
    :documentation "Mark the option as required. Only valid if the option takes a parameter")
   (short-name
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
   (is-set-p
    :initarg :is-set-p
    :initform nil
    :accessor option-is-set-p
    :documentation "Predicate which returns T if the option was set")
   (value
    :initarg :value
    :initform nil
    :accessor option-value
    :documentation "Computed value after finalizing the option"))
  (:documentation "A class representing a command-line option"))

(defmethod print-object ((option option) stream)
  (print-unreadable-object (option stream :type t)
    (format stream "short=~A long=~A"
	    (option-short-name option)
	    (option-long-name option))))

(defmethod make-option ((kind (eql :generic)) &rest rest)
  "Creates a generic option"
  (apply #'make-instance 'option rest))

(defmethod initialize-instance :after ((option option) &key)
  ;; Test for required short/long names
  (with-slots (short-name long-name) option
    (unless (or short-name long-name)
      (error 'invalid-option :item option
			     :reason (format nil "option must specify a short and/or long name"))))
  ;; Required option must have a parameter associated with it
  (when (and (option-required-p option)
	     (not (option-parameter option)))
    (error 'invalid-option :item option
			   :reason (format nil "required option must have a parameter associated with it")))
  ;; Required option must not have a default value associated with it.
  ;; However, it can still be initialized through other means,
  ;; e.g. environment variables.
  (when (and (option-required-p option)
	     (option-initial-value option))
    (error 'invalid :item option
		    :reason "required option may not have a default value")))

(defmethod initialize-option ((option option) &key)
  "Initialize the value of the option.

  Environment variables take precedence over any
  initial-value configured for the option.

  The first environment variable that resolves to a
  non-NIL result will be used to set the option."
  (setf (option-is-set-p option) nil)
  (let* ((env-vars (option-env-vars option))
	 (value-from-env (some #'uiop:getenvp env-vars))
	 (value (or value-from-env (option-initial-value option))))
    (setf (option-value option) value)
    (when value
      (setf (option-is-set-p option) t))))

(defmethod derive-option-value ((option option) arg &key)
  arg)

(defmethod finalize-option ((option option) &key)
  "Finalizes the value of the option"
  (option-value option))

(defclass option-boolean (option)
  ()
  (:default-initargs
   :parameter "VALUE"
   :initial-value :true)
  (:documentation "An option which represents a boolean flag"))

(defmethod make-option ((kind (eql :boolean)) &rest rest)
  (apply #'make-instance 'option-boolean rest))

(defmethod derive-option-value ((option option-boolean) arg &key)
  (let ((arg (string-downcase arg)))
    (cond
      ((string= "1" arg) :true)
      ((string= "true" arg) :true)
      (t :false))))

(defmethod finalize-option ((option option-boolean) &key)
  (ecase (option-value option)
    (:true t)
    (:false nil)))

(defclass option-boolean-true (option-boolean)
  ()
  (:default-initargs
   :parameter nil
   :initial-value :true)
  (:documentation "A boolean option which always returns true"))

(defmethod make-option ((kind (eql :boolean/true)) &rest rest)
  (apply #'make-instance 'option-boolean-true rest))

(defmethod derive-option-value ((option option-boolean-true) arg &key)
  (declare (ignore arg))
  :true)

(defclass option-boolean-false (option-boolean)
  ()
  (:default-initargs
   :parameter nil
   :initial-value :false)
  (:documentation "A boolean option which always returns false"))

(defmethod make-option ((kind (eql :boolean/false)) &rest rest)
  (apply #'make-instance 'option-boolean-false rest))

(defmethod derive-option-value ((option option-boolean-false) arg &key)
  (declare (ignore arg))
  :false)

(defclass option-counter (option)
  ()
  (:default-initargs
   :initial-value 0)
  (:documentation "An option which increments every time it is set"))

(defmethod make-option ((kind (eql :counter)) &rest rest)
  (apply #'make-instance 'option-counter rest))

(defmethod derive-option-value ((option option-counter) arg &key)
  (declare (ignore arg))
  (1+ (option-value option)))

(defclass option-list (option)
  ()
  (:default-initargs
   :initial-value nil
   :parameter "ITEM")
  (:documentation "An option which collects values into a list"))

(defmethod make-option ((kind (eql :list)) &rest rest)
  (apply #'make-instance 'option-list rest))

(defmethod derive-option-value ((option option-list) arg &key)
  (cons arg (option-value option)))

(defmethod finalize-option ((option option-list) &key)
  (nreverse (option-value option)))
