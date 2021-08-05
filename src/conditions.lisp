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
   :unknown-option-p))
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
    :reader duplicate-option-kind)
   (items
    :initarg :items
    :reader duplicate-option-items)
   (name
    :initarg :name
    :reader duplicate-option-name))
  (:report (lambda (condition stream)
	     (format stream "Duplicate option ~A of kind ~A found"
		     (duplicate-option-name condition)
		     (duplicate-option-kind condition))))
  (:documentation "A condition which is signalled when a command provides duplicate options"))

(define-condition duplicate-commands (simple-error)
  ((items
    :initarg :items
    :reader duplicate-command-items)
   (name
    :initarg :name
    :reader duplicate-command-name))
  (:report (lambda (condition stream)
	     (format stream "Duplicate commands ~A found" (duplicate-command-name condition))))
  (:documentation "A condition which is signalled when a command provides duplicate sub-commands"))

(define-condition unknown-option (error)
  ((name
    :initarg :name
    :reader unknown-option-name)
   (kind
    :initarg :kind
    :reader unknown-option-kind))
  (:report (lambda (condition stream)
	     (format stream "Unknown option ~A of kind ~A"
		     (unknown-option-name condition)
		     (unknown-option-kind condition))))
  (:documentation "A condition which is signalled when an unknown option is seen"))

(defun unknown-option-p (value)
  (typep value 'unknown-option))
