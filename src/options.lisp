;; Copyright (c) 2021 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :clingon.options
  (:use :cl)
  (:import-from
   :clingon.utils
   :join-list)
  (:import-from
   :clingon.conditions
   :invalid-option
   :missing-required-option-value
   :option-derive-error)
  (:export
   :*end-of-options-marker*
   :option
   :option-short-name
   :option-long-name
   :option-required-p
   :option-parameter
   :option-description
   :option-category
   :option-env-vars
   :option-initial-value
   :option-key
   :option-value
   :option-is-set-p
   :option-hidden-p
   :option-persistent-p
   :initialize-option
   :finalize-option
   :derive-option-value
   :make-option
   :option-usage-details
   :option-description-details
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
   :option-integer-radix
   :option-list-integer
   :option-choice
   :option-choice-items
   :option-enum
   :option-enum-items
   :option-switch
   :option-switch-on-states
   :option-switch-off-states
   :option-filepath
   :option-list-filepath
   :parse-integer-or-lose))
(in-package :clingon.options)

(defgeneric initialize-option (option &key)
  (:documentation "Initializes an option, e.g. sets initial option value"))

(defgeneric finalize-option (option &key)
  (:documentation "Finalizes an option, e.g. performs any value transformations"))

(defgeneric derive-option-value (option value &key)
  (:documentation "Derives a new value for the option based on the given string VALUE"))

(defgeneric make-option (kind &rest rest)
  (:documentation "Creates a new option of the given kind"))

(defgeneric option-usage-details (kind object &key)
  (:documentation "Returns the usage details for the option as a
  string. The returned string will be used for formatting and
  displaying the option as part of help pages."))

(defgeneric option-description-details (kind object &key)
  (:documentation "Returns a formatted and probably enriched content
  of the option's description"))

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
   (description
    :initarg :description
    :initform (error "Must specify description")
    :reader option-description
    :documentation "Short description of the option")
   (category
    :initarg :category
    :initform ""
    :reader option-category
    :documentation "Category for the option. Options with the same category will be grouped together")
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
   (is-set-p
    :initarg :is-set-p
    :initform nil
    :accessor option-is-set-p
    :documentation "Predicate which returns T if the option was set")
   (hidden
    :initarg :hidden
    :initform nil
    :reader option-hidden-p
    :documentation "Whether or not this option will be hidden on the usage pages")
   (persistent
    :initarg :persistent
    :initform nil
    :reader option-persistent-p
    :documentation "Whether or not this option is persistent across sub-commands")
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

(defmethod option-usage-details ((kind (eql :default)) (option option) &key)
  (with-output-to-string (s)
    (cond
      ;; Short and long names are defined
      ((and (option-short-name option) (option-long-name option))
       (format s "-~A, --~A" (option-short-name option) (option-long-name option)))
      ;; We only have a short name defined
      ((option-short-name option)
       (format s "-~A" (option-short-name option)))
      ;; Long name defined only, align it properly
      (t
       (format s "~vA--~A" 4 #\Space (option-long-name option))))
    (when (option-parameter option)
      (format s " <~A>" (option-parameter option)))))

(defmethod option-description-details ((kind (eql :default)) (option option) &key)
  (with-output-to-string (s)
    (format s "~A" (option-description option))
    (when (option-initial-value option)
      (format s " [default: ~A]" (option-initial-value option)))
    (when (option-env-vars option)
      (let ((vars (mapcar (lambda (var) (format nil "$~A" var)) (option-env-vars option))))
        (format s " [env: ~A]" (join-list vars ", "))))))

(defmethod option-usage-details ((kind (eql :zsh-option-spec)) (option option) &key)
  (with-output-to-string (s)
    (cond
      ;; Short and long names are defined
      ((and (option-short-name option) (option-long-name option))
       (format s "{-~A,--~A}" (option-short-name option) (option-long-name option)))
      ;; Short name only
      ((option-short-name option)
       (format s "-~A" (option-short-name option)))
      (t
       ;; Long name only
       (format s "--~A" (option-long-name option))))))

(defmethod option-description-details ((kind (eql :zsh-option-spec)) (option option) &key)
  (with-output-to-string (s)
    (format s "'[~A]'" (option-description option))
    (when (option-parameter option)
      (format s ":~A" (option-parameter option)))))

;;;;
;;;; Generic options
;;;;

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

;;;;
;;;; String options
;;;;

(defclass option-string (option)
  ()
  (:default-initargs
   :parameter "VALUE")
  (:documentation "An option which represents a string"))

(defmethod make-option ((kind (eql :string)) &rest rest)
  (apply #'make-instance 'option-string rest))

(defclass option-filepath (option)
  ()
  (:default-initargs
   :parameter "PATH")
  (:documentation "An option which represents a filepath"))

(defmethod make-option ((kind (eql :filepath)) &rest rest)
  (apply #'make-instance 'option-filepath rest))

(defmethod derive-option-value ((option option-filepath) arg &key)
  (pathname arg))

(defmethod option-description-details ((kind (eql :zsh-option-spec)) (option option-filepath) &key)
  ;; Use the `_files' function for completing file paths
  (with-output-to-string (s)
    (write-string (call-next-method) s)
    (format s ":_files")))

;;;;
;;;; Boolean options
;;;;

(defclass option-boolean (option)
  ()
  (:default-initargs
   :parameter "VALUE")
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
   :parameter nil)
  (:documentation "A boolean option which always returns true"))

(defmethod make-option ((kind (eql :boolean/true)) &rest rest)
  (apply #'make-instance 'option-boolean-true rest))

(defmethod make-option ((kind (eql :flag)) &rest rest)
  (apply #'make-instance 'option-boolean-true rest))

(defmethod derive-option-value ((option option-boolean-true) arg &key)
  (declare (ignore arg))
  :true)

(defclass option-boolean-false (option-boolean)
  ()
  (:default-initargs
   :parameter nil)
  (:documentation "A boolean option which always returns false"))

(defmethod make-option ((kind (eql :boolean/false)) &rest rest)
  (apply #'make-instance 'option-boolean-false rest))

(defmethod derive-option-value ((option option-boolean-false) arg &key)
  (declare (ignore arg))
  :false)

;;;;
;;;; Counter options
;;;;

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

(defmethod option-usage-details ((kind (eql :zsh-option-spec)) (option option-counter) &key)
  "Counter options may be repeated on the command-line"
  (with-output-to-string (s)
    (format s "\\*")
    (write-string (call-next-method) s)))

;;;;
;;;; List options
;;;;

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
  (setf (option-value option) (nreverse (option-value option))))

(defmethod option-usage-details ((kind (eql :zsh-option-spec)) (option option-list) &key)
  "List options may be repeated on the command-line"
  (with-output-to-string (s)
    (format s "\\*")
    (write-string (call-next-method) s)))

(defclass option-list-filepath (option-filepath option-list)
  ()
  (:default-initargs
   :parameter "PATH")
  (:documentation "An option which represents a list of filepaths"))

(defmethod make-option ((kind (eql :list/filepath)) &rest rest)
  (apply #'make-instance 'option-list-filepath rest))

;;;;
;;;; Integer options
;;;;

(defun parse-integer-or-lose (value &key (radix 10))
  (when (integerp value)
    (return-from parse-integer-or-lose value))

  (let ((int (parse-integer value :radix radix :junk-allowed t)))
    (unless int
      (error 'option-derive-error :reason (format nil "Cannot parse ~A as integer" value)))
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

;;;;
;;;; Choice/enum options
;;;;

(defclass option-choice (option)
  ((items
    :initarg :items
    :initform (error "Must specify available items")
    :reader option-choice-items
    :documentation "The available choices"))
  (:default-initargs
   :parameter "CHOICE")
  (:documentation "An option which allows selecting an item from a predefined list"))

(defmethod option-description-details ((kind (eql :default)) (option option-choice) &key)
  (with-output-to-string (s)
    (write-string (call-next-method) s)
    (let ((choices (option-choice-items option)))
      (format s " [choices: ~A]" (join-list choices ", ")))))

(defmethod option-description-details ((kind (eql :zsh-option-spec)) (option option-choice) &key)
  (with-output-to-string (s)
    (write-string (call-next-method) s)
    (let ((choices (option-choice-items option)))
      (format s ":'(~A)'" (join-list choices #\Space)))))

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
      (error 'option-derive-error :reason (format nil "Invalid choice: must be one of ~A" items))))
  arg)

(defclass option-enum (option)
  ((items
    :initarg :items
    :initform (error "Must specify available variants")
    :reader option-enum-items
    :documentation "The enum variants and their associated values"))
  (:default-initargs
   :parameter "VARIANT")
  (:documentation "An option which represents an enum with variants and associated values"))

(defmethod option-description-details ((kind (eql :default)) (option option-enum) &key)
  (with-output-to-string (s)
    (write-string (call-next-method) s)
    (let ((choices (mapcar #'car (option-enum-items option))))
      (format s " [choices: ~A]" (join-list choices ", ")))))

(defmethod option-description-details ((kind (eql :zsh-option-spec)) (option option-enum) &key)
  (with-output-to-string (s)
    (write-string (call-next-method) s)
    (let ((choices (mapcar #'car (option-enum-items option))))
      (format s ":'(~A)'" (join-list choices #\Space)))))

(defmethod make-option ((kind (eql :enum)) &rest rest)
  (apply #'make-instance 'option-enum rest))

(defmethod initialize-option ((option option-enum) &key)
  (call-next-method)
  (unless (option-value option)
    (return-from initialize-option))

  (let ((current (option-value option)))
    (setf (option-value option) (derive-option-value option current))))

(defmethod derive-option-value ((option option-enum) arg &key)
  (let* ((items (option-enum-items option))
         (pair (find arg items :key #'car :test #'string=)))
    (unless pair
      (error 'option-derive-error
             :reason (format nil "Invalid choice: must be one of ~A" (mapcar #'car items))))
    (cdr pair)))

(defclass option-switch (option-boolean)
  ((on-states
    :initarg :on-states
    :initform '("on" "yes" "true" "enable" "1")
    :reader option-switch-on-states
    :documentation "The list of states considered to `activate' the switch")
   (off-states
    :initarg :off-states
    :initform '("off" "no" "false" "disable" "0")
    :reader option-switch-off-states
    :documentation "The list of states considered to `deactivate' the switch"))
  (:default-initargs
   :parameter "STATE")
  (:documentation "An option which represents a switch with a state"))

(defmethod option-description-details ((kind (eql :zsh-option-spec)) (option option-switch) &key)
  (with-output-to-string (s)
    (write-string (call-next-method) s)
    (let ((on-states (option-switch-on-states option))
          (off-states (option-switch-off-states option)))
      (format s ":'(~A)'" (join-list (append on-states off-states) #\Space)))))

(defmethod initialize-option ((option option-switch) &key)
  "Initializes the switch option kind"
  (call-next-method)
  (unless (option-value option)
    (return-from initialize-option))

  ;; Derive a new value if we have an initial value
  (let ((current (option-value option)))
    (setf (option-value option)
          (derive-option-value option current))))

(defmethod derive-option-value ((option option-switch) arg &key)
  (cond
    ((member arg (option-switch-on-states option) :test #'string=) :true)
    ((member arg (option-switch-off-states option) :test #'string=) :false)
    (t
     (error 'option-derive-error :reason (format nil "Invalid switch state: ~A" arg)))))

(defmethod make-option ((kind (eql :switch)) &rest rest)
  (apply #'make-instance 'option-switch rest))
