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
   :missing-option-argument-item
   :missing-option-argument-command
   :missing-option-argument-p
   :invalid-option
   :invalid-option-item
   :invalid-option-reason
   :missing-required-option-value
   :missing-required-option-value-item
   :missing-required-option-value-command
   :option-derive-error
   :option-derive-error-reason
   :option-derive-error-p
   :base-error
   :exit-error
   :exit-error-code))
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
             (let ((items (duplicate-command-items condition)))
               (format stream
                       "Detected ~A duplicate command names/aliases.~2%~
                       The following commands have been identified as ~
                       providing duplicate names/aliases.~2%~
                       ~A~%"
                       (length items) items))))
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
  ((item
    :initarg :item
    :initform (error "Must specify option item")
    :reader missing-option-argument-item)
   (command
    :initarg :command
    :initform (error "Must specify command")
    :reader missing-option-argument-command))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Missing argument for option")))
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

(define-condition base-error (simple-error)
  ()
  (:documentation "A base condition to be used for app specific errors"))

(define-condition exit-error (base-error)
  ((code
    :initarg :code
    :initform (error "Must specify exit code")
    :reader exit-error-code
    :documentation "The exit code to be returned to the operating system"))
  (:documentation "A condition representing an error with associated exit code"))
