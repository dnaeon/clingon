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
   :option-required
   :option-parameter
   :option-help
   :option-env-vars
   :option-initial-value
   :option-reduce-fn
   :option-finalize-fn
   :option-key
   :option-category
   :option-value
   :initialize-option
   :finalize-option
   :end-of-options-p
   :short-option-p
   :long-option-p))
(in-package :clingon.options)

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
    :reader option-required
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
   (reduce-fn
    :initarg :reduce-fn
    :initform (error "Must specify a reduce function")
    :reader option-reduce-fn
    :documentation "If the option takes a parameter this should be a
function, which accepts two arguments -- prev and current value. If
the option does not accept a parameter the function should receive a
single argument -- the current value of the option.")
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

(defmethod print-object ((option option) stream)
  (print-unreadable-object (option stream :type t)
    (format stream "short=~A long=~A"
	    (option-short-name option)
	    (option-long-name option))))

(defun make-option (&rest rest)
  "Create a new instance of OPTION"
  (apply #'make-instance 'option rest))

(defmethod initialize-instance :after ((option option) &key)
  ;; Test for required short/long names
  (with-slots (short-name long-name key) option
    (unless (or short-name long-name)
      (error 'invalid-option :item option
			     :reason (format nil "option must specify a short and/or long name"))))
  ;; Required option must have a parameter associated with it
  (when (and (option-required option)
	     (not (option-parameter option)))
    (error 'invalid-option :item option
			   :reason (format nil "required option must have a parameter associated with it")))
  ;; Required option must not have a default value associated with it.
  ;; However, it can still be initialized through other means,
  ;; e.g. environment variables.
  (when (and (option-required option)
	     (option-initial-value option))
    (error 'invalid :item option
		    :reason "required option may not have a default value")))

(defmethod initialize-option ((option option) &key)
  "Initialize the value of the option.

  Environment variables take precedence over any
  initial-value configured for the option.

  The first environment variable that resolves to a
  non-NIL result will be used to set the option."
  (let* ((env-vars (option-env-vars option))
	 (value-from-env (some #'uiop:getenvp env-vars))
	 (value (or value-from-env (option-initial-value option))))
    (setf (option-value option) value)))

(defmethod finalize-option ((option option) &key)
  "Finalizes the option and sets it's value to the
  result of invoking the :finalize-fn function"
  (let ((reduce-fn (option-reduce-fn option))
	(finalize-fn (option-finalize-fn option))
	(value (option-value option)))
    ;; A required option value was not provided
    (when (and (not value) (option-required option))
      (restart-case (error 'missing-required-option-value :item option)
        (supply-value (value)
          :report "Supply value for the option"
          :interactive (lambda ()
                         (format *query-io* "New value: ")
                         (force-output *query-io*)
                         (list (read-line *query-io*)))
	  (setf (option-value option)
		(funcall reduce-fn nil value))
	  (return-from finalize-option
	    (finalize-option option)))))
    ;; Finalize the option's value
    (setf (option-value option)
	  (funcall finalize-fn value))))
