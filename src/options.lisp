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
   :option-counter-step
   :option-list
   :option-list-separator
   :option-choice
   :option-choices
   :option-integer
   :option-integer-radix))
(in-package :clingon.options)

(defgeneric initialize-option (option &key)
  (:documentation "Initializes an option, e.g. sets initial option value"))

(defgeneric finalize-option (option &key)
  (:documentation "Finalizes an option, e.g. performs any value transformations"))

(defgeneric derive-option-value (option value &key)
  (:documentation "Derives a new value for the option based on the given string VALUE"))

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
  (unless (keywordp (option-key option))
    (error 'invalid-option :item option
			   :reason "key must be a keyword"))

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
    (error 'invalid-option :item option
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
  ((step
    :initarg :step
    :initform 1
    :reader option-counter-step
    :documentation "Numeric value to increase the counter with"))
  (:default-initargs
   :initial-value 0)
  (:documentation "An option which increments every time it is set"))

(defmethod make-option ((kind (eql :counter)) &rest rest)
  (apply #'make-instance 'option-counter rest))

(defmethod derive-option-value ((option option-counter) arg &key)
  (declare (ignore arg))
  (+ (option-value option) (option-counter-step option)))

(defclass option-list (option)
  ((separator
    :initarg :separator
    :initform #\,
    :reader option-list-separator
    :documentation "Character used to separate items in a list represented as a string"))
  (:default-initargs
   :initial-value nil
   :parameter "ITEM")
  (:documentation "An option which collects values into a list"))

(defmethod make-option ((kind (eql :list)) &rest rest)
  (apply #'make-instance 'option-list rest))

(defmethod initialize-option ((option option-list) &key)
  "Initializes a list option. If the option has been initialized
  via environment variables, the initial value for the list would
  be represented as a string. This method will ensure that if the
  option is initialized from a string source it is represented as
  a valid list before deriving any other values for the option."
  ;; Make sure we call our parent initialization method first to set
  ;; things up.
  (call-next-method)

  ;; Nothing to initialize further
  (unless (option-value option)
    (return-from initialize-option))

  (let ((value (option-value option))
	(separator (option-list-separator option)))
    (setf (option-value option)
	(etypecase value
	  (list (reverse value))
	  (string (nreverse (mapcar (lambda (x)
				      (string-trim #(#\ ) x))
				    (split-sequence:split-sequence separator value))))))))

(defmethod derive-option-value ((option option-list) arg &key)
  (cons arg (option-value option)))

(defmethod finalize-option ((option option-list) &key)
  (nreverse (option-value option)))

(defun parse-integer-or-lose (value &key (radix 10))
  (when (integerp value)
    (return-from parse-integer-or-lose value))

  (let ((int (parse-integer value :radix radix :junk-allowed t)))
    (unless int
      (error 'option-parse-error :reason (format nil "Cannot parse ~A as integer" value)))
    int))

(defclass option-integer (option)
  ((radix
    :initarg :radix
    :initform 10
    :reader option-integer-radix))
  (:default-initargs
   :parameter "INT")
  (:documentation "An option class to represent an integer"))

(defmethod make-option ((kind (eql :integer)) &rest rest)
  (apply #'make-instance 'option-integer rest))

(defmethod initialize-option ((option option-integer) &key)
  "Initializes the integer option. In case the option was
  first initialized by other means, such as environment variables,
  we make sure that the provided value is a valid integer."
  (call-next-method)

  ;; Nothing to initialize further
  (unless (option-value option)
    (return-from initialize-option))

  (let ((value (option-value option)))
    (setf (option-value option)
	  (etypecase value
	    (integer value)
	    (string (parse-integer-or-lose value :radix (option-integer-radix option)))))))

(defmethod derive-option-value ((option option-integer) arg &key)
  (parse-integer-or-lose arg :radix (option-integer-radix option)))

(defclass option-list-integer (option-list)
  ((radix
    :initarg :radix
    :initform 10
    :reader option-integer-radix))
  (:documentation "An option which collects integers into a list"))

(defmethod make-option ((kind (eql :list/integer)) &rest rest)
  (apply #'make-instance 'option-list-integer rest))

(defmethod initialize-option ((option option-list-integer) &key)
  (call-next-method)
  (unless (option-value option)
    (return-from initialize-option))

  (setf (option-value option)
	(mapcar (lambda (x)
		  (etypecase x
		    (integer x)
		    (string (parse-integer-or-lose x :radix (option-integer-radix option)))))
		(option-value option))))

(defmethod derive-option-value ((option option-list-integer) arg &key)
  (cons (parse-integer-or-lose arg :radix (option-integer-radix option))
	(option-value option)))

(defclass option-choice (option)
  ((items
    :initarg :items
    :initform (error "Must specify available items")
    :reader option-choice-items
    :documentation "The available choices"))
  (:default-initargs
   :parameter "CHOICE")
  (:documentation "An option which allows selecting an item from a predefined list"))

(defmethod make-option ((kind (eql :choice)) &rest rest)
  (apply #'make-instance 'option-choice rest))

(defmethod initialize-option ((option option-choice) &key)
  ;; Set things up
  (call-next-method)

  ;; Nothing to be done further
  (unless (option-value option)
    (return-from initialize-option))

  ;; Derive a new value based on the already set initialized value
  (let ((current (option-value option)))
    (setf (option-value option) (derive-option-value option current))))

(defmethod derive-option-value ((option option-choice) arg &key)
  (let ((items (option-choice-items option)))
    (unless (member arg items :test #'string=)
      (error 'option-parse-error :reason (format nil "Invalid choice: must be one of ~A" items))))
  arg)
