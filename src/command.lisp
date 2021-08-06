(in-package :cl-user)
(defpackage :clingon.command
  (:use :cl)
  (:import-from
   :clingon.conditions
   :circular-dependency
   :duplicate-options
   :duplicate-commands)
  (:import-from
   :clingon.context
   :context
   :make-context
   :make-child-context
   :context-arguments
   :context-initial-argv)
  (:import-from
   :clingon.generics
   :parse-option
   :make-option
   :initialize-context
   :finalize-context)
  (:import-from
   :clingon.options
   :end-of-options-p
   :short-option-p
   :long-option-p
   :make-option
   :option-short-name
   :option-long-name)
  (:import-from
   :clingon.utils
   :argv
   :walk)
  (:export
   :command
   :command-name
   :command-options
   :command-handler
   :command-sub-commands
   :command-parent
   :command-lineage
   :make-command
   :command-full-path
   :find-sub-command
   :run
   :with-commands-walk
   :parse-command-line))
(in-package :clingon.command)

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

(defmethod print-object ((command command) stream)
  (print-unreadable-object (command stream :type t)
    (format stream "name=~A options=~A sub-commands=~A"
	    (command-name command)
	    (length (command-options command))
	    (length (command-sub-commands command)))))

(defmethod initialize-instance :after ((command command) &key)
  ;; Configure the parent for any of the sub-commands.
  (dolist (sub (command-sub-commands command))
    (setf (command-parent sub) command)))

(defmethod ensure-unique-options ((command command))
  "Ensures that the given COMMAND does not contain duplicate options"
  (loop :for (option . remaining) :on (command-options command) :while option :do
    (let* ((short-name (option-short-name option))
	   (long-name (option-long-name option))
	   (short-items (remove-if-not
			 (lambda (x)
			   (char= short-name (option-short-name x)))
			 remaining))
	   (long-items (remove-if-not
			(lambda (x)
			  (string= long-name (option-long-name x)))
			remaining)))
      (when (> (length short-items) 0)
	(error 'duplicate-options :kind :short :name short-name :items (cons option short-items)))
      (when (> (length long-items) 0)
	(error 'duplicate-options :kind :long :name long-name :items (cons option long-items)))))
  t)

(defmethod ensure-unique-sub-commands ((command command))
  "Ensure that the given COMMAND does not contain duplicate sub-commands"
  (loop :for (sub-command . remaining) :on (command-sub-commands command) :while sub-command :do
    (let* ((sub-command-name (command-name sub-command))
	   (sub-command-items (remove-if-not
			       (lambda (item)
				 (string= sub-command-name (command-name item)))
			       remaining)))
      (when (> (length sub-command-items) 0)
	(error 'duplicate-commands :items (cons sub-command sub-command-items)))))
  t)

(defmethod command-lineage ((command command))
  "Returns the lineage of the command up to the root"
  (loop :for c = command :then (command-parent c)
	:while c
	:when (member c visited :test #'equal) :do
	  (error 'circular-dependency :items visited)
	:collect c :into visited
	:finally (return visited)))

(defmethod find-sub-command ((command command) name &key)
  "Returns the sub-command with the given name"
  (find name (command-sub-commands command) :key #'command-name :test #'string=))

(defmethod command-full-path ((command command) &key)
  "Returns the full path to the command"
  (let ((lineage (command-lineage command)))
    (nreverse (mapcar #'command-name lineage))))

(defmacro with-commands-walk ((command top-level) &body body)
  "Walks over each command starting from TOP-LEVEL and evaluates BODY"
  `(let ((nodes (walk ,top-level #'command-sub-commands :order :dfs)))
     (dolist (,command nodes)
       ,@body)))

(defmethod validate-top-level-command ((top-level command))
  "Validates the top-level command and it's sub-commands"
  (with-commands-walk (cmd top-level)
    (ensure-unique-sub-commands cmd)
    (ensure-unique-options cmd))
  t)

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
  (validate-top-level-command command)
  (let* ((options (command-options command))
	 (context (make-context :initial-argv arguments :options options)))
    (parse-command-line% command context)))

(defmethod parse-command-line% ((command command) (context context))
  (initialize-context context)
  (loop :for arg = (first (context-initial-argv context)) :while arg
	:for empty-ctx-arguments-p = (null (context-arguments context))
	:for sub-command = (and empty-ctx-arguments-p (find-sub-command command arg))
	:for new-context = (and sub-command (make-child-context context))
	:do
	   (cond
	     ;; End of options.
	     ((end-of-options-p arg) (parse-option :consume-all-arguments context))
	     ;; Short options.
	     ((short-option-p arg) (parse-option :short context))
	     ;; Long options.
	     ((long-option-p arg) (parse-option :long context))
	     ;; Sub-commands.
	     ;; The sub-command will be non-nil if it is the first
	     ;; free argument we are processing and that argument
	     ;; happens to be the name of a sub-command.
	     ;; At this point we stop processing in the current
	     ;; context and pass to the sub-command for further
	     ;; processing.
	     (sub-command
	      ;; Remove the sub-command name from the arguments
	      (pop (context-initial-argv new-context))
	      (finalize-context context)
	      (return-from parse-command-line%
		(parse-command-line% sub-command new-context)))
	     ;; Free arguments
	     (t
	      (parse-option :free-argument context)))
	:finally (finalize-context context))
  (values command context))
