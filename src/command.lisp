(in-package :cl-user)
(defpackage :clingon.command
  (:use :cl)
  (:import-from
   :clingon.conditions
   :circular-dependency
   :duplicate-options
   :duplicate-commands
   :missing-required-option-value
   :unknown-option
   :missing-option-argument)
  (:import-from
   :clingon.options
   :end-of-options-p
   :short-option-p
   :long-option-p
   :make-option
   :option-short-name
   :option-long-name
   :option-required-p
   :option-is-set-p
   :option-key
   :option-value
   :option-parameter
   :option-help
   :initialize-option
   :finalize-option
   :derive-option-value)
  (:import-from
   :clingon.utils
   :argv
   :walk)
  (:export
   :find-short-option
   :find-long-option
   :parse-option
   :initialize-command
   :finalize-command
   :command
   :command-name
   :command-options
   :command-handler
   :command-sub-commands
   :command-parent
   :command-lineage
   :command-arguments
   :make-command
   :command-full-path
   :find-sub-command
   :run
   :with-commands-walk
   :parse-command-line
   :getopt
   :opt-is-set-p))
(in-package :clingon.command)

(defgeneric find-short-option (object name &key)
  (:documentation "Returns the short option with the given NAME, or NIL otherwise"))

(defgeneric find-long-option (object name &key)
  (:documentation "Returns the long option with the given NAME, or NIL otherwise"))

(defgeneric parse-option (kind object &key)
  (:documentation "Parses an option of the given KIND"))

(defgeneric initialize-command (command &key)
  (:documentation "Initializes a command"))

(defgeneric finalize-command (command &key)
  (:documentation "Finalizes a command and derives the set of reduced options"))

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
    :documentation "A function taking a single argument, which is an instance of COMMAND and it's environment")
   (sub-commands
    :initarg :sub-commands
    :initform nil
    :reader command-sub-commands
    :documentation "Sub-commands for the command")
   (parent
    :initarg :parent
    :initform nil
    :accessor command-parent
    :documentation "Parent command. This one will be automatically set during instantiation.")
   (args-to-parse
    :initarg :args-to-parse
    :initform nil
    :accessor command-args-to-parse
    :documentation "Arguments to be parsed based on the command options")
   (arguments
    :initarg :arguments
    :initform nil
    :accessor command-arguments
    :documentation "Discovered free arguments after parsing the options")
   (reduced-options
    :initarg :reduced-options
    :initform (make-hash-table :test #'equal)
    :accessor command-reduced-options
    :documentation "The set of reduced option values, after finalizing the command"))
  (:documentation "A class to represent a command to be handled"))

(defun make-command (&rest rest)
  "Creates a new COMMAND instance"
  (apply #'make-instance 'command rest))

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

(defmethod initialize-command ((command command) &key)
  "Initializes the command and the options associated with it."
  (setf (command-reduced-options command) (make-hash-table :test #'equal))
  (setf (command-arguments command) nil)
  (dolist (option (command-options command))
    (initialize-option option)))

(defmethod finalize-command ((command command) &key)
  "Finalizes the command and derives the reduced set of options"
  (setf (command-args-to-parse command) nil)
  (let ((result (command-reduced-options command)))
    (setf (command-arguments command)
          (nreverse (command-arguments command)))
    ;; Finalize the options
    (dolist (option (command-options command))
      ;; Option is required and was not set
      (when (and (option-required-p option)
		 (not (option-is-set-p option)))
	(error 'missing-required-option-value :item option :command command))
      ;; Option is finalized, only if it has been set
      (when (option-is-set-p option)
	(finalize-option option)
	(setf (gethash (option-key option) result) (option-value option))))))

(defmethod find-short-option ((command command) name &key)
  "Finds the short option with the given name"
  (find name (command-options command) :key #'option-short-name :test #'char=))

(defmethod find-long-option ((command command) name &key)
  "Finds the long option with the given name"
  (find name (command-options command) :key #'option-long-name :test #'string=))

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

(defmethod parse-option ((kind (eql :consume-all-arguments)) (command command) &key)
  "Consumes all arguments after the end-of-options flag"
  (pop (command-args-to-parse command)) ;; Drop the end-of-options (`--') argument
  (loop :for arg = (pop (command-args-to-parse command)) :while arg :do
    (push arg (command-arguments command))))

(defmethod parse-option ((kind (eql :free-argument)) (command command) &key)
  "Consume the option and treat it as a free argument"
  (let ((arg (pop (command-args-to-parse command))))
    (push arg (command-arguments command))))

(defmethod parse-option ((kind (eql :short)) (command command) &key)
  "Parses a short option"
  (let* ((arg (pop (command-args-to-parse command)))
         (short-name (aref arg 1))
         (short-name-full (subseq arg 0 2))
         (option (find-short-option command short-name)))
    ;; Unknown option
    (unless option
      ;; Push remaining options, if the argument contains collapsed
      ;; options. For example the arg `-abcd' where `a' is an unknown
      ;; option would push `-bcd' for further processing.
      (when (> (length arg) 2)
        (push (format nil "-~A" (subseq arg 2)) (command-args-to-parse command)))
      (restart-case (error 'unknown-option :kind :short :name short-name-full)
        (discard-option ()
          :report "Discard the unknown option"
          (return-from parse-option))
        (treat-as-argument ()
          :report "Treat the unknown option as a free argument"
          (push short-name-full (command-arguments command))
          (return-from parse-option))
        (supply-new-value (value)
          :report "Supply a new value to be parsed"
          :interactive (lambda ()
                         (format *query-io* "New option to parse: ")
                         (force-output *query-io*)
                         (list (read-line *query-io*)))
          (push value (command-args-to-parse command))
          (return-from parse-option))))
    ;; Valid option
    (setf (option-is-set-p option) t)
    (let ((current-value (option-value option))
          (reduce-fn (option-reduce-fn option)))
      (cond
        ;; Option takes a parameter
        ((option-parameter option)
         (let ((optarg (if (> (length arg) 2)
                           (subseq arg 2) ;; -xfoo
                           (pop (command-args-to-parse command))))) ;; -x foo
           ;; Missing argument for an option
           (unless optarg
             (restart-case (error 'missing-option-argument :name short-name-full :kind :short)
               (discard-option ()
                 :report "Discard the option"
                 (return-from parse-option))
               (supply-argument (value)
                 :report "Supply argument for the option"
                 :interactive (lambda ()
                                (format *query-io* "Argument for ~A option: " short-name-full)
                                (force-output *query-io*)
                                (list (read-line *query-io*)))
                 (setf optarg value))))
           (setf (option-value option)
                 (funcall reduce-fn current-value optarg))))
        ;; Option does not take an argument
        (t
         (setf (option-value option)
               (funcall reduce-fn current-value))
         ;; Options may be collapsed into a single argument,
         ;; e.g. `-abc'. For options which which do not accept
         ;; arguments, but we still have remaining tokens we should
         ;; push them for further processing.
         (when (> (length arg) 2)
           (let ((next-option (format nil "-~A" (subseq arg 2))))
             (push next-option (command-args-to-parse command)))))))))

(defmethod parse-option ((kind (eql :long)) (command command))
  "Parses a long option"
  (let* ((arg (pop (command-args-to-parse command)))
         (equals-position (position #\= arg))
         (long-name (subseq arg 2 equals-position))
         (long-name-full (format nil "--~A" long-name))
         (option (find-long-option command long-name)))
    ;; Unknown option
    (unless option
      (restart-case (error 'unknown-option :kind :long :name long-name-full)
        (discard-option ()
          :report "Discard the unknown option"
          (return-from parse-option))
        (treat-as-argument ()
          :report "Treat the unknown option as a free argument"
          (push long-name-full (command-arguments command))
          (return-from parse-option))
        (supply-new-value (value)
          :report "Supply a new value to be parsed"
          :interactive (lambda ()
                         (format *query-io* "New option to parse: ")
                         (force-output *query-io*)
                         (list (read-line *query-io*)))
          (push value (command-args-to-parse command))
          (return-from parse-option))))
    ;; Valid option
    (setf (option-is-set-p option) t)
    (let* ((current-value (option-value option))
           (reduce-fn (option-reduce-fn option)))
      (cond
        ;; Option takes a parameter
        ((option-parameter option)
         (let ((optarg (if equals-position
                           (subseq arg (1+ equals-position))       ;; --arg=foo
                           (pop (command-args-to-parse command))))) ;; --arg foo
           ;; Missing argument for the option
           (unless optarg
             (restart-case (error 'missing-option-argument :name long-name-full :kind :long)
               (discard-option ()
                 :report "Discard the option"
                 (return-from parse-option))
               (supply-argument (value)
                 :report "Supply argument for the option"
                 :interactive (lambda ()
                                (format *query-io* "Argument for ~A option: " long-name-full)
                                (force-output *query-io*)
                                (list (read-line *query-io*)))
                 (setf optarg value))))
           (setf (option-value option)
                 (funcall reduce-fn current-value optarg))))
        ;; Option does not take a parameter
        (t
         (setf (option-value option)
               (funcall reduce-fn current-value)))))))

(defmethod run ((top-level command) &optional arguments)
  "Runs the specified top-level command"
  (let* ((arguments (or arguments (argv)))
	 (cmd (parse-command-line top-level arguments)))
    (unless (command-handler cmd)
      (error "No handler registered for command ~A" (command-name cmd)))
    (funcall (command-handler cmd) cmd)))

(defmethod parse-command-line ((top-level command) arguments)
  "Parses the command-line arguments for the command"
  (validate-top-level-command top-level)
  (setf (command-args-to-parse top-level) arguments)
  (parse-command-line% top-level))

(defmethod parse-command-line% ((command command))
  (initialize-command command)
  (loop
    :for arg = (first (command-args-to-parse command)) :while arg
    :for empty-arguments-p = (null (command-arguments command))
    :for sub-command = (and empty-arguments-p (find-sub-command command arg))
    :do
       (cond
	 ;; End of options.
	 ((end-of-options-p arg) (parse-option :consume-all-arguments command))
	 ;; Short options.
	 ((short-option-p arg) (parse-option :short command))
	 ;; Long options.
	 ((long-option-p arg) (parse-option :long command))
	 ;; Sub-commands.
	 ;; The sub-command will be non-nil if it is the first
	 ;; free argument we are processing and that argument
	 ;; happens to be the name of a sub-command.
	 ;; At this point we stop processing arguments in the
	 ;; current command and pass it to the sub-command
	 ;; for further processing.
	 (sub-command
	  ;; Configure the arguments for parsing for the sub-command
	  (setf (command-args-to-parse sub-command)
		(rest (command-args-to-parse command)))
	  ;; Finalize the current command before passing it to
	  ;; the next sub-command for processing.
	  (finalize-command command)
	  (return-from parse-command-line%
	    (parse-command-line% sub-command)))
	 ;; Free arguments
	 (t
	  (parse-option :free-argument command)))
    :finally (finalize-command command))
  command)
