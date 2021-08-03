(in-package :cl-user)
(defpackage :clingon.command
  (:use :cl)
  (:import-from
   :clingon.context
   :context
   :make-context
   :make-child-context
   :context-arguments
   :context-initial-argv
   :initialize-context
   :finalize-context)
  (:import-from
   :clingon.generics
   :parse-option)
  (:import-from
   :clingon.options
   :end-of-options-p
   :short-option-p
   :long-option-p
   :make-option
   :option-short-name
   :option-long-name)
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
   :find-sub-command
   :run
   :parse-command-line))
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


(define-condition circular-dependency (error)
  ((items
    :accessor circular-dependency-items
    :initarg :items))
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Circular dependency found"))))

(define-condition duplicate-options (error)
  ((kind
    :accessor duplicate-options-kind
    :initarg :kind)
   (items
    :accessor duplication-options-items
    :initarg :items))
  (:report (lambda (condition stream)
	     (format stream "Duplicate options of kind ~A found"
		     (duplicate-options-kind condition)))))

(defmethod initialize-instance :after ((command command) &key)
  ;; Configure the parent for any of the sub-commands.
  (dolist (sub (command-sub-commands command))
    (setf (command-parent sub) command)))

(defmethod command-parents-list ((command command) &key)
  "Returns the list of parent commands for the given command"
  (loop :for parent = (command-parent command) :then (command-parent parent)
	:while parent
	:when (member parent visited :test #'equal) :do
	  (error 'circular-dependency :items visited)
	:collect parent :into visited
	:finally (return visited)))

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

(defmethod run ((command command) &key arguments)
  "Runs the specified top-level command"
  (let ((arguments (or arguments (argv))))
    (multiple-value-bind (cmd ctx) (parse-command-line command arguments)
      (unless (command-handler cmd)
	(error "No handler registered for command ~A" (command-name cmd)))
      (funcall (command-handler cmd) ctx))))

(defmethod parse-command-line ((command command) arguments)
  "Parses the command-line arguments for the command"
  (let* ((options (command-options command))
	 (context (make-context :initial-argv arguments :options options)))
    (parse-command-line% command context)))

(defmethod parse-command-line% ((command command) (context context))
  (initialize-context context)
  (loop :for arg = (first (context-initial-argv context)) :while arg :do
    (cond
      ;; End of options.
      ((end-of-options-p arg)
       (parse-option :consume-all-arguments context))
      ;; Short options.
      ((short-option-p arg)
       (parse-option :short context))
      ;; Long options.
      ((long-option-p arg)
       (parse-option :long context))
      ;; Free arguments and sub-commands.
      (t
       ;; If this this the first free argument and that argument
       ;; happens to be the name of a sub-command stop processing here
       ;; and then dispatch processing of the remaining arguments to the
       ;; respective sub-command.
       (let* ((empty-ctx-arguments-p (null (context-arguments context)))
	      (sub-command (and empty-ctx-arguments-p (find-sub-command command arg)))
	      (new-context (and sub-command (make-child-context context))))
	 (cond
	   ;; We've got a sub-command, dispatch further processing to it.
	   (sub-command
	    ;; Remove the sub-command name from the arguments
	    (pop (context-initial-argv new-context))
	    (finalize-context context)
	    (return-from parse-command-line%
	      (parse-command-line% sub-command new-context)))
	   ;; We've got a free argument
	   (t
	    (parse-option :free-argument context))))))
    :finally (finalize-context context))
  (values command context))
