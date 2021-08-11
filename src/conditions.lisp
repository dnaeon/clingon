(in-package :cl-user)
(defpackage :clingon.conditions
  (:use :cl)
  (:export
   :circular-dependency
   :circular-dependency-items
   :duplicate-options
   :duplicate-option-kind
   :duplicate-option-items
   :duplicate-option-name
   :duplicate-commands
   :duplicate-command-items
   :unknown-option
   :unknown-option-name
   :unknown-option-kind
   :unknown-option-p
   :missing-option-argument
   :missing-option-argument-name
   :missing-option-argument-kind
   :missing-option-argument-p
   :invalid-option
   :invalid-option-item
   :invalid-option-reason
   :missing-required-option-value
   :missing-required-option-value-item
   :missing-required-option-value-command
   :option-derive-error
   :option-derive-error-reason
   :option-derive-error-p))
(in-package :clingon.conditions)

(define-condition option-derive-error (simple-error)
  ((reason
    :initarg :reason
    :initform (error "Must specify reason")
    :reader option-derive-error-reason
    :documentation "Reason for which deriving a value failed"))
  (:report (lambda (condition stream)
	     (format stream "~A" (option-derive-error-reason condition))))
  (:documentation "A condition which is signalled when deriving an option's value has failed"))

(defun option-derive-error-p (value)
  (typep value 'option-derive-error))

(define-condition missing-required-option-value (simple-error)
  ((item
    :initarg :item
    :initform (error "Must specify option item")
    :reader missing-required-option-value-item
    :documentation "The option item which requires a value")
   (command
    :initarg :command
    :initform (error "Must specify command")
    :reader missing-required-option-value-command
    :documentation "The command to which the option is associated"))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Required option not set")))
  (:documentation "A condition which is signalled when a required option value was not set"))

(define-condition circular-dependency (simple-error)
  ((items
    :initarg :items
    :initform (error "Must specify items")
    :reader circular-dependency-items))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Circular dependency detected")))
  (:documentation "A condition which is signalled when a circular dependency is detected"))

(define-condition duplicate-options (simple-error)
  ((kind
    :initarg :kind
    :initform (error "Must specify option kind")
    :reader duplicate-option-kind)
   (items
    :initarg :items
    :initform (error "Must specify option items")
    :reader duplicate-option-items)
   (name
    :initarg :name
    :initform (error "Must specify option name")
    :reader duplicate-option-name))
  (:report (lambda (condition stream)
             (format stream "Duplicate option ~A of kind ~A found"
                     (duplicate-option-name condition)
                     (duplicate-option-kind condition))))
  (:documentation "A condition which is signalled when a command provides duplicate options"))

(define-condition duplicate-commands (simple-error)
  ((items
    :initarg :items
    :initform (error "Must specify duplicate items")
    :reader duplicate-command-items))
  (:report (lambda (condition stream)
             (format stream "There are ~A duplicate commands found"
		     (length (duplicate-command-items conditions)))))
   (:documentation "A condition which is signalled when a command provides duplicate sub-commands"))

(define-condition unknown-option (error)
  ((name
    :initarg :name
    :initform (error "Must specify option name")
    :reader unknown-option-name)
   (kind
    :initarg :kind
    :initform (error "Must specify option kind")
    :reader unknown-option-kind))
  (:report (lambda (condition stream)
             (format stream "Unknown option ~A of kind ~A"
                     (unknown-option-name condition)
                     (unknown-option-kind condition))))
  (:documentation "A condition which is signalled when an unknown option is seen"))

(defun unknown-option-p (value)
  (typep value 'unknown-option))

(define-condition missing-option-argument (simple-error)
  ((name
    :initarg :name
    :initform (error "Must specify option name")
    :reader missing-option-argument-name)
   (kind
    :initarg :kind
    :initform (error "Must specify option kind")
    :reader missing-option-argument-kind))
  (:report (lambda (condition stream)
             (format stream "Missing argument for option ~A of kind ~A"
                     (missing-option-argument-name condition)
                     (missing-option-argument-kind condition))))
  (:documentation "A condition which is signalled when an option expects an argument, but none was provided"))

(defun missing-option-argument-p (value)
  (typep value 'missing-option-argument))

(define-condition invalid-option (simple-error)
  ((item
    :initarg :item
    :initform (error "Must specify option item")
    :reader invalid-option-item
    :documentation "The option which is identified as invalid")
   (reason
    :initarg :reason
    :initform (error "Must specify reason")
    :reader invalid-option-reason
    :documentation "The reason why this option is invalid"))
  (:report (lambda (condition stream)
             (format stream "Invalid option: ~A" (invalid-option-reason condition))))
  (:documentation "A condition which is signalled when an option is identified as invalid"))
