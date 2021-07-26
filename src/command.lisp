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
   :command-parents-list
   :make-command
   :command-full-path
   :find-sub-command))
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

(defmethod command-parents-list ((command command) &key)
  "Returns the list of parent commands for the given command"
  (loop :for parent = (command-parent command) :then (command-parent parent)
	:while parent
	:collect parent))

(defmethod find-sub-command ((command command) name &key)
  "Returns the sub-command with the given name"
  (find name (command-sub-commands command) :key #'command-name :test #'string=))

(defmethod command-full-path ((command command) &key)
  "Returns the full path to the command"
  (let ((result (command-parents-list command)))
    (push command result)
    (nreverse (mapcar #'command-name result))))

(defun make-command (&rest rest)
  "Creates a new COMMAND instance"
  (apply #'make-instance 'command rest))
