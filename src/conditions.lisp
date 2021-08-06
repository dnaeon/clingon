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
   :duplicate-command-name
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
   :invalid-option-reason))
(in-package :clingon.conditions)

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
    :reader duplicate-command-items)
   (name
    :initarg :name
    :initform (error "Must specify command name")
    :reader duplicate-command-name))
  (:report (lambda (condition stream)
	     (format stream "Duplicate commands ~A found" (duplicate-command-name condition))))
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
    :initarg :option
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
