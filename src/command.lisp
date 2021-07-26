(in-package :cl-user)
(defpackage :clingon.command
  (:use :cl)
  (:import-from
   :clingon.context
   :make-context
   :context-arguments
   :initialize-context
   :finalize-context)
  (:export
   :argv
   :command
   :command-name
   :command-options
   :command-handler
   :command-sub-commands
   :command-parent
   :make-command))
(in-package :clingon.command)

(defun argv ()
  "Returns the list of command-line arguments"
  (uiop:command-line-arguments))

(defclass command ()
  ((name
    :initarg :name
    :initform (error "Must specify command name")
    :reader command-name
    :documentation "Command name")
   (options
    :initarg :options
    :initform nil
    :reader command-options
    :documentation "Command options")
   (handler
    :initarg :handler
    :initform nil
    :reader command-handler
    :documentation "Handler to be invoked. A function taking a single argument - the context.")
   (sub-commands
    :initarg :sub-commands
    :initform nil
    :reader command-sub-commands
    :documentation "Any sub-commands of the command")
   (parent
    :initarg :parent
    :initform nil
    :accessor command-parent
    :documentation "Parent command. This one will be automatically set on creation."))
  (:documentation "A class to represent a command to be handled"))

(defmethod initialize-instance :after ((command command) &key)
  ;; Configure the parent for any of the sub-commands.
  (dolist (sub (command-sub-commands command))
    (setf (command-parent sub) command)))
