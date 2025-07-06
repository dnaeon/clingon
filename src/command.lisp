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
(defpackage :clingon.command
  (:use :cl)
  (:import-from
   :with-user-abort
   :with-user-abort
   :user-abort)
  (:import-from
   :split-sequence
   :split-sequence)
  (:import-from
   :clingon.conditions
   :circular-dependency
   :duplicate-options
   :duplicate-commands
   :missing-required-option-value
   :missing-required-option-value-item
   :missing-required-option-value-command
   :unknown-option
   :missing-option-argument
   :missing-option-argument-item
   :missing-option-argument-command
   :option-derive-error-p
   :base-error
   :exit-error
   :exit-error-code)
  (:import-from
   :clingon.options
   :option
   :end-of-options-p
   :short-option-p
   :long-option-p
   :make-option
   :option-short-name
   :option-long-name
   :option-description
   :option-category
   :option-required-p
   :option-is-set-p
   :option-hidden-p
   :option-persistent-p
   :option-key
   :option-value
   :option-parameter
   :initialize-option
   :finalize-option
   :derive-option-value
   :option-usage-details
   :option-description-details)
  (:import-from
   :clingon.utils
   :argv
   :walk
   :join-list
   :exit
   :group-by
   :hashtable-keys)
  (:export
   :find-option
   :parse-option
   :initialize-command
   :finalize-command
   :handle-error
   :command
   :command-name
   :command-options
   :command-handler
   :command-pre-hook
   :command-post-hook
   :command-sub-commands
   :command-parent
   :command-lineage
   :command-arguments
   :command-authors
   :command-version
   :command-description
   :command-long-description
   :command-license
   :command-usage
   :command-args-to-parse
   :command-examples
   :command-aliases
   :make-command
   :command-full-path
   :command-full-name
   :command-tree
   :command-is-top-level-p
   :with-command-tree
   :find-sub-command
   :run
   :parse-command-line
   :getopt
   :getopt*
   :opt-is-set-p
   :opt-is-set-p*
   :treat-as-argument
   :discard-option
   :validate-top-level-command
   :print-usage
   :print-usage-and-exit
   :print-version
   :print-version-and-exit
   :print-documentation
   :print-options-usage
   :print-sub-commands-info
   :command-usage-string
   :visible-options
   :persistent-options
   :inherited-options
   :apply-hooks
   :*default-help-flag*
   :*default-version-flag*
   :*default-bash-completions-flag*
   :*default-options*))
(in-package :clingon.command)

(defgeneric handle-error (condition)
  (:documentation "Handles the condition. This generic function will be called by
clingon for conditions which sub-class the CLINGON:BASE-ERROR
condition. The CLINGON:BASE-ERROR condition is the base class for app
specific errors."))

(defmethod handle-error ((error exit-error))
  (exit (exit-error-code error)))

(defgeneric find-option (kind object name)
  (:documentation "Returns the option of the given KIND and NAME, or NIL otherwise"))

(defgeneric parse-option (kind object)
  (:documentation "Parses an option of the given KIND"))

(defgeneric initialize-command (command)
  (:documentation "Initializes a command"))

(defgeneric finalize-command (command)
  (:documentation "Finalizes a command and derives the set of reduced options"))

(defgeneric print-usage (command stream &key)
  (:documentation "Prints the usage information of the command"))

(defgeneric print-version (command stream &key)
  (:documentation "Prints the version information of the command"))

(defgeneric print-documentation (kind command stream &key)
  (:documentation "Prints the documentation of the given top-level command"))

(defgeneric apply-hooks (kind command)
  (:documentation "Applies any hooks associated with the given COMMAND"))

(defgeneric ensure-unique-options (command)
  (:documentation "Ensures that the given COMMAND does not contain
  duplicate options. Signals a condition on error"))

(defgeneric ensure-unique-sub-commands (command)
  (:documentation "Ensures that the given COMMAND does not contain
  duplicate sub-command names. Signals a condition on error"))

(defgeneric command-lineage (command)
  (:documentation "Returns the lineage of the command up to the root"))

(defgeneric command-is-top-level-p (command)
  (:documentation "Returns T, if the command is a top-level command,
  NIL otherwise"))

(defgeneric find-sub-command (command name)
  (:documentation "Returns the sub-command with the given name or
  alias"))

(defgeneric command-full-path (command)
  (:documentation "Returns the full path to the command as a list"))

(defgeneric command-full-name (command)
  (:documentation "Returns a string representing the full name of the
  command"))

(defgeneric command-tree (command)
  (:documentation "Returns the nodes representing the command's tree"))

(defgeneric validate-top-level-command (command)
  (:documentation "Validates the top-level command and it's sub-commands"))

(defgeneric run (command &optional arguments)
  (:documentation "Runs the specific command"))

(defgeneric parse-command-line (command arguments)
  (:documentation "Parses the arguments given to the command and
  returns the most-specific sub-command which matched"))

(defgeneric getopt (command opt-key &optional default)
  (:documentation "Returns the value of the option identified by OPT-KEY, or DEFAULT if
not found, or not set."))

(defgeneric getopt* (command opt-key &optional default)
  (:documentation "Returns the value of the option identified by OPT-KEY from the first
  command in the lineage, for which the option is defined and is set,
  or DEFAULT if none of the commands in the lineage provides the
  option, and is set."))

(defgeneric opt-is-set-p (command opt-key)
  (:documentation "Returns T, if the option identified by OPT-KEY is set, NIL otherwise."))

(defgeneric opt-is-set-p* (command opt-key)
  (:documentation "Returns T, if the option identified by OPT-KEY is
  set in any command from the lineage, NIL otherwise."))

(defgeneric visible-options (command)
  (:documentation "Returns the list of visible options"))

(defgeneric persistent-options (command)
  (:documentation "Returns the list of persistent options for the command"))

(defgeneric inherited-options (command)
  (:documentation "Returns the list of options, which will be inherited by COMMAND"))

(defgeneric print-options-usage (command stream &key)
  (:documentation "Prints the usage information about options to the
  given stream"))

(defgeneric print-sub-commands-info (command stream &key)
  (:documentation "Prints a summary of the sub-commands available for
  the command"))

(defparameter *default-help-flag*
  (make-option :flag
               :description "display usage information and exit"
               :long-name "help"
               :key :clingon.help.flag)
  "The default `--help' flag")

(defparameter *default-version-flag*
  (make-option :flag
               :description "display version and exit"
               :long-name "version"
               :key :clingon.version.flag)
  "The default `--version' flag")

(defparameter *default-bash-completions-flag*
  (make-option :flag
               :hidden t
               :description "generate bash completions"
               :long-name "bash-completions"
               :key :clingon.bash-completions.flag)
  "The default `--bash-completions' flag")

(defparameter *default-options*
  (list *default-help-flag*
        *default-version-flag*
        *default-bash-completions-flag*)
  "A list of default options to add to each sub-command")

(defparameter *zsh-compfunc-with-sub-commands*
  (format nil "~
_~~A() {
    local line state
    local -a curr_cmd_options=(
~~A
    )

    _arguments -C -S -s \\
               $curr_cmd_options \\
               \"1: :->cmds\" \\
               \"*::arg:->args\"

    case \"$state\" in
        cmds)
            _values \\
~~A
            ;;
        args)
            case $line[1] in
~~A
            esac
            ;;
    esac
}~2&")
  "Template for a Zsh completion function which contains sub-commands")

(defparameter *zsh-compfunc-without-sub-commands*
  (format nil "~
_~~A() {
    local -a curr_cmd_options=(
~~A
    )

    _arguments -C -S -s \\
               $curr_cmd_options
}~2&")
  "Template for a Zsh completion function without sub-commands")

(defclass command ()
  ((name
    :initarg :name
    :initform (error "Must specify command name")
    :reader command-name
    :documentation "Command name")
   (options
    :initarg :options
    :initform nil
    :accessor command-options
    :documentation "Command options")
   (handler
    :initarg :handler
    :initform nil
    :reader command-handler
    :documentation "A function which accepts a single argument. The
     argument is an instance of the COMMAND class, which provides the
     context and environment for options.")
   (pre-hook
    :initarg :pre-hook
    :initform nil
    :reader command-pre-hook
    :documentation "A pre-hook is a function which will be invoked
    before the command handler is executed. The function must accept a
    single argument, which is an instance of the COMMAND class.")
   (post-hook
    :initarg :post-hook
    :initform nil
    :reader command-post-hook
    :documentation "A post-hook is a function which will be invoked
    after the command handler is executed. The function must accept a
    single argument, which is an instance of the COMMAND class.")
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
   (context
    :initarg :context
    :initform (make-hash-table :test #'equal)
    :accessor command-context
    :documentation "The context contains the reduced set of options for the command")
   (version
    :initarg :version
    :initform nil
    :reader command-version
    :documentation "Version of the command")
   (authors
    :initarg :authors
    :initform nil
    :reader command-authors
    :documentation "Authors of the command")
   (license
    :initarg :license
    :initform nil
    :reader command-license
    :documentation "License for the command")
   (description
    :initarg :description
    :initform nil
    :reader command-description
    :documentation "Short description of what the command does")
   (long-description
    :initarg :long-description
    :initform nil
    :reader command-long-description
    :documentation "Long description of what the command does")
   (examples
    :initarg :examples
    :initform nil
    :reader command-examples
    :documentation "A list of examples describing how to use the command")
   (aliases
    :initarg :aliases
    :initform nil
    :reader command-aliases
    :documentation "Aliases of the command")
   (usage
    :initarg :usage
    :initform nil
    :reader command-usage
    :documentation "Usage information for the command"))
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
    (setf (command-parent sub) command))

  ;; Add default options to each command
  (dolist (default-opt *default-options*)
    (push default-opt (command-options command))))

(defmethod initialize-command ((command command))
  "Initializes the command and the options associated with it."
  ;; Re-initialize context and arguments
  (setf (command-context command) (make-hash-table :test #'equal))
  (setf (command-arguments command) nil)

  ;; Inherit persistent options from parent commands
  (loop :with inherited-opts = (inherited-options command)
        :with curr-opts = (command-options command)
        :with opts = (remove-if (lambda (item)
                                  (member item curr-opts :test #'equalp))
                                inherited-opts)
        :for option :in opts :do
          (push option (command-options command)))

  ;; Initialize options
  (dolist (option (command-options command))
    (initialize-option option))
  t)

(defmethod finalize-command ((command command))
  "Finalizes the command and derives the reduced set of option values"
  (setf (command-args-to-parse command) nil)
  (let ((context (command-context command)))
    (setf (command-arguments command)
          (nreverse (command-arguments command)))
    ;; Finalize the options
    (dolist (option (command-options command))
      ;; Option is finalized, only if it has been set
      (when (option-is-set-p option)
        (setf (option-value option)
              (finalize-option option))
        (setf (gethash (option-key option) context) (option-value option)))))

  ;; Special cases for some default flags
  (cond
    ((getopt command :clingon.bash-completions.flag)
     (print-documentation :bash-completions command t)
     (error 'exit-error :code 0))
    ((getopt command :clingon.version.flag)
     ;; If this is a sub-command and we don't have a version string,
     ;; then try to find a version string from some of the parent
     ;; commands.
     (let ((cmd (some (lambda (item)
                        (and (command-version item) item))
                      (command-lineage command))))
       (when cmd
         (print-version cmd t))
       (error 'exit-error :code 0)))
    ((getopt command :clingon.help.flag)
     (print-usage command t)
     (error 'exit-error :code 0)))

  ;; Verify required options
  (let ((required-options (remove-if-not #'option-required-p (command-options command))))
    (dolist (option required-options)
      (unless (option-is-set-p option)
        (error 'missing-required-option-value :item option :command command)))))

(defmethod find-option ((kind (eql :short)) (command command) name)
  "Finds the option with the given short name"
  (find name (command-options command)
        :key (lambda (item) (or (option-short-name item) #\Nul))
        :test #'char=))

(defmethod find-option ((kind (eql :long)) (command command) name)
  "Finds the option with the given long name"
  (find name (command-options command) :key #'option-long-name :test #'string=))

(defmethod find-option ((kind (eql :by-key)) (command command) opt-key)
  "Finds the option identified by OPT-KEY"
  (find opt-key (command-options command) :key #'option-key :test #'equal))

(defmethod ensure-unique-options ((command command))
  "Ensures that the given COMMAND does not contain duplicate options"
  (loop :for (option . remaining) :on (command-options command) :while option :do
    (let* ((s-name (option-short-name option))
           (l-name (option-long-name option))
           (s-duplicates (and s-name (remove-if-not
                                      (lambda (x)
                                        (and (option-short-name x) (char= s-name (option-short-name x))))
                                      remaining)))
           (l-duplicates (and l-name (remove-if-not
                                      (lambda (x)
                                        (string= l-name (option-long-name x)))
                                      remaining))))
      (when (> (length s-duplicates) 0)
        (error 'duplicate-options :kind :short :name s-name :items (cons option s-duplicates)))
      (when (> (length l-duplicates) 0)
        (error 'duplicate-options :kind :long :name l-name :items (cons option l-duplicates)))))
  t)

(defmethod ensure-unique-sub-commands ((command command))
  "Ensure that the given COMMAND does not contain duplicate sub-command names"
  (loop :for (sub-command . remaining) :on (command-sub-commands command) :while sub-command :do
    (let* ((sub-names (cons (command-name sub-command)
                            (command-aliases sub-command)))
           (duplicates (remove-if-not
                        (lambda (other)
                          (let ((other-names (cons (command-name other)
                                                   (command-aliases other))))
                            (intersection sub-names other-names :test #'string=)))
                        remaining)))
      (when (> (length duplicates) 0)
        (error 'duplicate-commands :items (cons sub-command duplicates)))))
  t)

(defmethod command-lineage ((command command))
  "Returns the lineage of the command up to the root"
  (loop :for c = command :then (command-parent c)
        :while c
        :when (member c visited :test #'equal) :do
          (error 'circular-dependency :items visited)
        :collect c :into visited
        :finally (return visited)))

(defmethod command-is-top-level-p ((top-level command))
  "Returns T if the command is a top-level command, NIL otherwise"
  (= 1 (length (command-lineage top-level))))

(defmethod find-sub-command ((command command) name)
  "Returns the sub-command with the given name or alias"
  (find-if (lambda (sub-command)
             (or (string= name (command-name sub-command))
                 (member name (command-aliases sub-command) :test #'string=)))
           (command-sub-commands command)))

(defmethod command-full-path ((command command))
  "Returns the full path to the command"
  (let ((lineage (command-lineage command)))
    (nreverse (mapcar #'command-name lineage))))

(defmethod command-full-name ((command command))
  "Returns a string representing the full name of the command"
  (let ((full-path (command-full-path command)))
    (join-list full-path #\Space)))

(defmethod command-tree ((top-level command))
  "Collects the nodes representing the command's tree starting from TOP-LEVEL"
  (walk top-level #'command-sub-commands :order :dfs))

(defmacro with-command-tree ((node top-level) &body body)
  "Evaluates BODY for each node in the command's tree starting from TOP-LEVEL"
  (let ((tree (gensym)))
    `(let ((,tree (command-tree ,top-level)))
       (dolist (,node ,tree)
         ,@body))))

(defmethod validate-top-level-command ((top-level command))
  "Validates the top-level command and it's sub-commands"
  (with-command-tree (node top-level)
    (ensure-unique-sub-commands node)
    (ensure-unique-options node))
  t)

(defmethod parse-option ((kind (eql :consume-all-arguments)) (command command))
  "Consumes all arguments after the end-of-options flag"
  (pop (command-args-to-parse command)) ;; Drop the end-of-options (`--') argument
  (loop :for arg = (pop (command-args-to-parse command)) :while arg :do
    (push arg (command-arguments command))))

(defmethod parse-option ((kind (eql :free-argument)) (command command))
  "Consume the option and treat it as a free argument"
  (let ((arg (pop (command-args-to-parse command))))
    (push arg (command-arguments command))))

(defmethod handle-unknown-option-with-restarts ((command command) kind full-name)
  "Provides possible restarts when an unknown option is detected"
  (restart-case (error 'unknown-option :kind kind :name full-name)
    (discard-option ()
      :report "Discard the unknown option")
    (treat-as-argument ()
      :report "Treat the unknown option as a free argument"
      (push full-name (command-arguments command)))
    (supply-new-value (value)
      :report "Supply a new value to be parsed"
      :interactive (lambda ()
                     (format *query-io* "New option to parse: ")
                     (force-output *query-io*)
                     (list (read-line *query-io*)))
      (push value (command-args-to-parse command)))))

(defmethod derive-option-with-restarts ((command command) (option option) optarg)
  "Provides possible restarts when deriving an option's value"
  (let* ((short-name (option-short-name option))
         (long-name (option-long-name option))
         ;; Pick either one of the short or long option names here
         (full-name (or (and short-name (format nil "-~A" short-name))
                        (and long-name (format nil "--~A" long-name)))))
    (restart-case (setf (option-value option) (derive-option-value option optarg))
      (discard-option ()
        :report "Discard the option"
        :test option-derive-error-p
        (setf (option-is-set-p option) nil))
      (supply-new-value (value)
        :report "Supply new argument for the option"
        :test option-derive-error-p
        :interactive (lambda ()
                       (format *query-io* "New value for ~A option: " full-name)
                       (force-output *query-io*)
                       (list (read-line *query-io*)))
        (setf (option-is-set-p option) nil)
        (push (cond
                ((and short-name (option-parameter option)) (format nil "~A~A" full-name value))
                ((and long-name (option-parameter option)) (format nil "~A=~A" full-name value))
                (t (format nil "~A" full-name)))
              (command-args-to-parse command))))))

(defmethod handle-missing-argument-with-restarts ((command command) (option option))
  "Provides possible restarts when an option requires an argument, but none was provided"
  (let* ((short-name (option-short-name option))
         (long-name (option-long-name option))
         ;; Pick either one of the short or long option names here
         (full-name (or (and short-name (format nil "-~A" short-name))
                        (and long-name (format nil "--~A" long-name)))))
    (restart-case (error 'missing-option-argument :item option :command command)
      (discard-option ()
        :report "Discard the option"
        (setf (option-is-set-p option) nil)
        nil)
      (supply-argument (value)
        :report "Supply argument for the option"
        :interactive (lambda ()
                       (format *query-io* "Argument for ~A option: " full-name)
                       (force-output *query-io*)
                       (list (read-line *query-io*)))
        value))))

(defmethod parse-option ((kind (eql :short)) (command command))
  "Parses a short option"
  (let* ((arg (pop (command-args-to-parse command)))
         (short-name (aref arg 1))
         (short-name-full (subseq arg 0 2))
         (option (find-option :short command short-name))
         (optarg nil))
    ;; Unknown option
    (unless option
      ;; Push remaining options, if the argument contains collapsed
      ;; options. For example the arg `-abcd' where `a' is an unknown
      ;; option would push `-bcd' for further processing.
      (when (> (length arg) 2)
        (push (format nil "-~A" (subseq arg 2)) (command-args-to-parse command)))
      (handle-unknown-option-with-restarts command kind short-name-full)
      ;; We are done here, let the parser handle any new input on the
      ;; next iteration.
      (return-from parse-option))

    ;; We've got a valid option
    (setf (option-is-set-p option) t)
    ;; Option takes a parameter, make sure we've got an argument from
    ;; which to derive a value and set `optarg' accordingly
    (when (option-parameter option)
      (if (> (length arg) 2)
          (setf optarg (subseq arg 2)) ;; -xfoo
          (setf optarg (pop (command-args-to-parse command)))) ;; -x foo
      ;; Handle missing argument for the option
      (unless optarg
        (setf optarg (handle-missing-argument-with-restarts command option))
        (unless optarg
          (return-from parse-option))))

    ;; Options may be collapsed into a single argument,
    ;; e.g. `-abc'. For options which which do not accept
    ;; arguments, but we still have remaining tokens we should
    ;; push them for further processing.
    (when (and (> (length arg) 2)
               (not (option-parameter option)))
      (push (format nil "-~A" (subseq arg 2)) (command-args-to-parse command)))

    (derive-option-with-restarts command option optarg)))

(defmethod parse-option ((kind (eql :long)) (command command))
  "Parses a long option"
  (let* ((arg (pop (command-args-to-parse command)))
         (equals-position (position #\= arg))
         (long-name (subseq arg 2 equals-position))
         (long-name-full (format nil "--~A" long-name))
         (option (find-option :long command long-name))
         (optarg nil))
    ;; Unknown option
    (unless option
      (handle-unknown-option-with-restarts command kind long-name-full)
      ;; We are done here, let the parser handle any new input on the
      ;; next iteration.
      (return-from parse-option))

    ;; Valid option
    (setf (option-is-set-p option) t)
    ;; Option takes a parameter, make sure we've got an argument from
    ;; which to derive a value and set `optarg' accordingly
    (when (option-parameter option)
      ;; Option takes a parameter
      (if equals-position
          (setf optarg (subseq arg (1+ equals-position))) ;; --arg=foo
          (setf optarg (pop (command-args-to-parse command)))) ;; --arg foo
      ;; Handle missing argument for the option
      (when (or (string= optarg "") (null optarg))
        (setf optarg (handle-missing-argument-with-restarts command option))
        (unless optarg
          (return-from parse-option))))

    (derive-option-with-restarts command option optarg)))

(defmethod apply-hooks ((kind (eql :pre)) (command command))
  "Applies any pre-hooks associated with the command's lineage
  starting from the least-specific node up to the most-specific one."
  (dolist (cmd (reverse (command-lineage command)))
    (when (command-pre-hook cmd)
      (funcall (command-pre-hook cmd) cmd))))

(defmethod apply-hooks ((kind (eql :post)) (command command))
  "Applies the post-hooks associated with command's lineage
   starting from the most-specific node down to the least-specific one"
  (dolist (cmd (command-lineage command))
    (when (command-post-hook cmd)
      (funcall (command-post-hook cmd) cmd))))

(defmethod run ((top-level command) &optional arguments)
  "Runs the specified top-level command"
  (handler-case
      (let* ((arguments (or arguments (argv)))
             (cmd (parse-command-line top-level arguments)))
        (unless (command-handler cmd)
          (error "No handler registered for command '~A'" (command-full-name cmd)))
        (apply-hooks :pre cmd)
        (with-user-abort (funcall (command-handler cmd) cmd))
        (apply-hooks :post cmd)
        (exit 0))
    ;; Missing required options
    (missing-required-option-value (condition)
      (let ((option (missing-required-option-value-item condition))
            (failed-cmd (missing-required-option-value-command condition)))
        (format *error-output* "~&Required option ~A not set.~%See '~A --help' for more details.~&"
                (string-trim #(#\Space) (option-usage-details :default option))
                (command-full-name failed-cmd))
        (exit 64))) ;; EX_USAGE
    ;; Missing argument for an option
    (missing-option-argument (condition)
      (let ((option (missing-option-argument-item condition))
            (failed-cmd (missing-option-argument-command condition)))
        (format *error-output* "~&No value specified for ~A option.~%See '~A --help' for more details.~&"
                (string-trim #(#\Space) (option-usage-details :default option))
                (command-full-name failed-cmd))
        (exit 64))) ;; EX_USAGE
    ;; SIGINT detected
    (user-abort ()
      (format *error-output* "~&Received SIGINT, exiting.~&")
      (exit 130))
    ;; App-specific errors
    (base-error (condition)
      (handle-error condition))
    ;; Anything else
    (error (condition)
      (format *error-output* "~&~A~&" condition)
      (exit 1))))

(defmethod parse-command-line ((top-level command) arguments)
  "Parses the arguments for the given top-level command and
   returns the most-specific command that is matched against the
   given arguments. The returned command contains the environment
   for the command handler to be executed with already populated
   options."
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

(defmethod getopt ((command command) opt-key &optional default)
  "Returns the value of the option identified by OPT-KEY by traversing
   the lineage of the given COMMAND, starting from the most-specific
   to least-specific command. If the option is not found, or not set,
   then GETOPT will return the DEFAULT value.

   GETOPT should be called for commands, which have been finalized.

   GETOPT always returns results for the requested OPT-KEY from the
   most-specific command.

   If multiple commands from the lineage provide the same option,
   which is identified by the same OPT-KEY, then the result from
   GETOPT is always the one from the most-specific command.

   Consider this example command-line:

   $ app --my-opt=global-val FOO-CMD --my-opt=foo-val BAR-CMD --my-opt=bar-value

   In above example `--my-opt' option is defined for the top-level
   command, and for the two sub-commands -- `FOO-CMD' and `BAR-CMD'
   respectively.

   If `--my-opt' uses the same OPT-KEY to identify the option in all
   three commands, e.g. `:my-opt', then the result from GETOPT will
   always be for the most-specific command, which in above example is
   `BAR-CMD'. In that case the result from GETOPT will be `bar-value'.

   Consider this additional example, in which case we still have the
   same option defined on all three commands, but this time we don't
   specify explicitely a value when invoking the `BAR-CMD'.

   $ app --my-opt=global-val FOO-CMD --my-opt=foo-val BAR-CMD

   The result in this case would be DEFAULT, because `:my-opt' is defined
   for `BAR-CMD', but is not set.

   If you need to be able to lookup options defined in global
   commands, it is considered a good practice that you always
   namespace your option keys. Using the same example as before, that
   would mean that you will use different keys for each option in the
   different commands, e.g. `:global-cmd/my-opt', `:foo-cmd/my-opt'
   and `:bar-cmd/my-opt' might be used as the keys for `--my-opt' in
   the different commands.

   If you really need to use the same OPT-KEY in multiple commands,
   and you only care about getting the value for an option from any of
   them then you might want to use GETOPT*. GETOPT* will return the
   value of OPT-KEY from the first command in the lineage, for which
   the option is defined, and is set.

   Using the last command-line as an example again.

   $ app --my-opt=global-val FOO-CMD --my-opt=foo-val BAR-CMD

   If we use GETOPT* in above example, then the result from it would
   be `:foo-val', as this is the first command in the lineage, for
   which the option is defined, and is set.

   GETOPT and GETOPT* return three values:

   0: the value of the option, if the option is defined and set, DEFAULT otherwise.
   1: T if the option was set, or NIL otherwise
   2: the command which provided the option value"
  (dolist (cmd (command-lineage command))
    ;; Option is defined for the command, and is set.
    (multiple-value-bind (value exists-p) (gethash opt-key (command-context cmd))
      (when exists-p
        (return-from getopt (values value exists-p cmd))))
    ;; Option is defined for the command, but is not set.
    (when (find-option :by-key cmd opt-key)
      (return-from getopt (values default nil cmd))))
  ;; Option not found.
  (values default nil nil))

(defmethod getopt* ((command command) opt-key &optional default)
  "GETOPT* works similarly to GETOPT, but traverses the lineage until it
   finds a command for which OPT-KEY is defined, and is set."
  (dolist (cmd (command-lineage command))
    ;; Option is defined for the command, and is set.
    (multiple-value-bind (opt-value exists-p) (gethash opt-key (command-context cmd))
      (when exists-p
        (return-from getopt* (values opt-value exists-p cmd)))))
  (values default nil nil))

(defmethod opt-is-set-p ((command command) opt-key)
  "Returns T, if the option identified by OPT-KEY is defined for the
  command, NIL otherwise."
  (multiple-value-bind (value is-set-p cmd) (getopt command opt-key)
    (declare (ignore value cmd))
    is-set-p))

(defmethod opt-is-set-p* ((command command) opt-key)
  "Returns T, if the option identified by OPT-KEY is defined for any
  of commands in the lineage, or NIL otherwise"
  (multiple-value-bind (value is-set-p cmd) (getopt* command opt-key)
    (declare (ignore value cmd))
    is-set-p))

(defmethod visible-options ((command command))
  "Returns the list of visible options for the given command"
  (remove-if #'option-hidden-p (command-options command)))

(defmethod persistent-options ((command command))
  "Returns the list of persistent options for the given command"
  (remove-if-not #'option-persistent-p (command-options command)))

(defmethod inherited-options ((command command))
  "Returns the list of persistent options, which will be inherited by COMMAND"
  (loop :with lineage = (rest (command-lineage command))
        :for parent :in lineage
        :appending (persistent-options parent) :into result
        :finally (return (remove-duplicates result :test #'equalp))))

(defun treat-as-argument (condition)
  "A handler which can be used to invoke the `treat-as-argument' restart"
  (invoke-restart (find-restart 'treat-as-argument condition)))

(defun discard-option (condition)
  "A handler which can be used to invoke the `discard-option' restart"
  (invoke-restart (find-restart 'discard-option condition)))

(defmethod print-options-usage ((command command) stream &key (wrap-at-width 70))
  "Prints the usage information about the options for the given command"
  (let* ((opts (visible-options command))
         (usages (mapcar (lambda (o) (option-usage-details :default o)) opts))
         (width (+ 4 (apply #'max (mapcar #'length usages))))
         (opt-groups (group-by opts #'option-category))
         (group-names (sort (hashtable-keys opt-groups) #'string<)))
    (loop :for group-name :in group-names
          :for group-opts = (gethash group-name opt-groups)
          :for sorted-opts = (sort group-opts
                                   #'string<
                                   :key (lambda (o) (option-usage-details :default o)))
          :do
             ;; Special case for the default option category/group
             (when (string/= group-name "")
               (format stream "~%~A:~&" group-name))
             (loop :for opt :in sorted-opts :do
               (let* ((usage (option-usage-details :default opt))
                      (desc (option-description-details :default opt))
                      (lines (split-sequence #\Newline (bobbin:wrap desc wrap-at-width))))
                 (format stream "  ~A" (option-usage-details :default opt))
                 (format stream "~vA~A~&" (- width (length usage) 2) #\Space (first lines))
                 (dolist (remaining (rest lines))
                   (format stream "~vA~A~&" width #\Space remaining))))))
  (format stream "~%"))

(defmethod print-sub-commands-info ((command command) stream &key (wrap-at-width 70))
  "Prints a summary of the sub-commands available for the command"
  (let* ((sub-commands (command-sub-commands command))
         (names-with-aliases (mapcar (lambda (x)
                                       (cons (command-name x) (command-aliases x)))
                                     sub-commands))
         (names (mapcar (lambda (x) (join-list x ", ")) names-with-aliases))
         (descriptions (mapcar #'command-description sub-commands))
         (width (+ 4 (apply #'max (mapcar #'length names)))))
    (loop :for (name desc) :in (mapcar #'list names descriptions) :do
      (let ((lines (split-sequence #\Newline (bobbin:wrap desc wrap-at-width))))
        (format stream "  ~A" name)
        (format stream "~vA~A~&" (- width (length name) 2) #\Space (first lines))
        (dolist (remaining (rest lines))
          (format stream "~vA~A~&" width #\Space remaining)))))
  (format stream "~%"))

(defmethod command-usage-string ((command command))
  "Returns the usage string for the given command"
  (let ((prev-cmd-name (and (command-parent command)
                            (command-full-name (second (command-lineage command))))))
    (cond
      ;; The command provides it's own usage info
      ((command-usage command)
       (format nil "~A ~A" (command-full-name command) (command-usage command)))
      ;; The command provides sub-commands and has a parent
      ((and (command-sub-commands command) (command-parent command))
       (format nil "~A [global-options] ~A [<command>] [command-options] [arguments ...]"
               prev-cmd-name
               (command-name command)))
      ;; The command provides sub-commands and is a top-level command
      ((and (command-sub-commands command) (command-is-top-level-p command))
       (format nil "~A [global-options] [<command>] [command-options] [arguments ...]"
               (command-full-name command)))
      ;; The command is a sub-command of another command
      ((command-parent command)
       (format nil "~A [global-options] ~A [options] [arguments ...]"
               ;; Print the path leading up to the command itself
               prev-cmd-name
               (command-name command)))
      ;; Default usage info
      (t
       (format nil "~A [options] [arguments ...]" (command-full-name command))))))

(defmethod print-usage ((command command) stream &key (wrap-at 70))
  (initialize-command command)
  (format stream "NAME:~%")
  (format stream "  ~A~@[ - ~A~]~2%" (command-full-name command) (command-description command))

  (format stream "USAGE:~%")
  (format stream "  ~A~2%" (command-usage-string command))

  (when (command-long-description command)
    (let ((lines (split-sequence #\Newline
                                 (bobbin:wrap (command-long-description command) wrap-at))))
      (dolist (line lines)
        (format stream "  ~A~%" line)))
    (format stream "~%"))

  (when (command-options command)
    (format stream "OPTIONS:~%")
    (print-options-usage command stream))

  (when (command-sub-commands command)
    (format stream "COMMANDS:~%")
    (print-sub-commands-info command stream))

  (when (command-examples command)
    (format stream "EXAMPLES:~2%")
    (dolist (example (command-examples command))
      (let* ((description (car example))
             (code (cdr example))
             (lines (split-sequence #\Newline (bobbin:wrap description wrap-at))))
        (dolist (line lines)
          (format stream "  ~A~%" line))
        (format stream "~%")
        (dolist (line (split-sequence #\Newline code))
          (format T "    ~A~%" line))
        (format stream "~%"))))

  (when (command-authors command)
    (format stream "AUTHORS:~%")
    (dolist (author (command-authors command))
      (format stream "  ~A~%" author))
    (format stream "~%"))

  (when (command-license command)
    (format stream "LICENSE:~%")
    (let ((lines (split-sequence #\Newline (bobbin:wrap (command-license command) wrap-at))))
      (dolist (line lines)
        (format stream "  ~A~%" line)))
    (format stream "~%")))

(defmethod print-usage-and-exit ((command command) stream)
  (print-usage command stream)
  (error 'exit-error :code 64)) ;; EX_USAGE

(defmethod print-version ((command command) stream &key)
  (when (command-version command)
    (format stream
            "~A version ~A~&"
            (command-full-name command)
            (command-version command))))

(defmethod print-version-and-exit ((command command) stream)
  (print-version command stream)
  (error 'exit-error :code 0))

(defmethod print-documentation ((kind (eql :bash-completions)) (command command) stream &key)
  "Prints the bash completions for the given command"
  (dolist (sub (command-sub-commands command))
    (format stream "~A~%" (command-name sub))
    (when (command-aliases sub)
      (format stream "~A~%" (join-list (command-aliases sub) #\Newline))))

  (dolist (opt (visible-options command))
    (when (option-short-name opt)
      (format stream "-~A~%" (option-short-name opt)))
    (when (and (option-long-name opt) (option-parameter opt))
      (format stream "--~A=~%" (option-long-name opt)))
    (when (and (not (option-parameter opt)) (option-long-name opt))
      (format stream "--~A~%" (option-long-name opt)))))

(defmethod print-documentation ((kind (eql :markdown)) (top-level command) stream &key (wrap-at 80))
  "Prints the documentation for the given TOP-LEVEL command in Markdown format"
  (with-command-tree (node top-level)
    ;; Initialize command, so that options get propagated
    (initialize-command node)

    ;; Command name
    (format stream "# ~A~2%" (command-full-name node))

    ;; Print description
    (cond
      ;; Print long description
      ((command-long-description node)
       (let ((lines (split-sequence #\Newline
                                    (bobbin:wrap (command-long-description node) wrap-at))))
         (dolist (line  lines)
           (format stream "~A~%" line))
         (format stream "~%")))
      ;; Print short description only
      (t (format stream "`~A` -- ~A~2%" (command-full-name node) (command-description node))))

    ;; Usage info
    (format stream "## Usage~2%")
    (format stream "``` shell~%~A~%```~2%" (command-usage-string node))

    ;; Options
    (when (command-options node)
      (format stream "## Options~2%")
      (format stream "`~A` accepts the following options:~2%" (command-full-name node))
      (format stream "``` shell~%")
      (print-options-usage node stream)
      (format stream "```~2%"))

    ;; Sub-commands
    (when (command-sub-commands node)
      (format stream "## Sub Commands~2%")
      (format stream "`~A` provides the following sub commands:~2%" (command-full-name node))
      (format stream "``` shell~%")
      (print-sub-commands-info node stream)
      (format stream "```~2%"))

    ;; Examples
    (when (command-examples node)
      (format stream "## Examples~2%")
      (dolist (example (command-examples node))
        (let* ((description (car example))
               (code (cdr example))
               (lines (split-sequence #\Newline (bobbin:wrap description wrap-at))))
          (dolist (line lines)
            (format stream "~A~%" line))
          (format stream "~%")
          (format stream "``` shell~%~A~%```~2%" code))))

    ;; Authors
    (when (command-authors node)
      (format stream "## Authors~2%")
      (dolist (author (command-authors node))
        (format stream "* ~A~%" author))
      (format stream "~%"))

    ;; License
    (when (command-license node)
      (format stream "## License~2%")
      (let ((lines (split-sequence #\Newline (bobbin:wrap (command-license node) wrap-at))))
        (dolist (line lines)
          (format stream "~A~%" line)))
      (format stream "~%"))))

(defmethod print-documentation ((kind (eql :org)) (top-level command) stream &key (wrap-at 80))
  "Prints the documentation for the given TOP-LEVEL command in Org Mode format"
  (with-command-tree (node top-level)
    ;; Initialize command, so that options get propagated
    (initialize-command node)

    ;; Command name
    (format stream "* ~A~2%" (command-full-name node))

    ;; Print description
    (cond
      ;; Print long description
      ((command-long-description node)
       (let ((lines (split-sequence #\Newline
                                    (bobbin:wrap (command-long-description node) wrap-at))))
         (dolist (line  lines)
           (format stream "~A~%" line))
         (format stream "~%")))
      ;; Print short description only
      (t (format stream "=~A= -- ~A~2%" (command-full-name node) (command-description node))))

    ;; Usage info
    (format stream "** Usage~2%")
    (format stream "#+begin_src shell~%~A~%#+end_src~2%" (command-usage-string node))

    ;; Options
    (when (command-options node)
      (format stream "** Options~2%")
      (format stream "=~A= accepts the following options:~2%" (command-full-name node))
      (format stream "#+begin_src shell~%")
      (print-options-usage node stream)
      (format stream "#+end_src~2%"))

    ;; Sub-commands
    (when (command-sub-commands node)
      (format stream "** Sub Commands~2%")
      (format stream "=~A= provides the following sub commands:~2%" (command-full-name node))
      (format stream "#+begin_src shell~%")
      (print-sub-commands-info node stream)
      (format stream "#+end_src~2%"))

    ;; Examples
    (when (command-examples node)
      (format stream "** Examples~2%")
      (dolist (example (command-examples node))
        (let* ((description (car example))
               (code (cdr example))
               (lines (split-sequence #\Newline (bobbin:wrap description wrap-at))))
          (dolist (line lines)
            (format stream "~A~%" line))
          (format stream "~%")
          (format stream "#+begin_src shell~%~A~%#+end_src~2%" code))))

    ;; Authors
    (when (command-authors node)
      (format stream "** Authors~2%")
      (dolist (author (command-authors node))
        (format stream "- ~A~%" author))
      (format stream "~%"))

    ;; License
    (when (command-license node)
      (format stream "** License~2%")
      (let ((lines (split-sequence #\Newline (bobbin:wrap (command-license node) wrap-at))))
        (dolist (line lines)
          (format stream "~A~%" line)))
      (format stream "~%"))))

(defmethod print-documentation ((kind (eql :mandoc)) (top-level command) stream &key (wrap-at 80))
  "Generates section 1 man pages in the mdoc(7) format.  Documentation
available at https://man.openbsd.org/mdoc.7"
  ;; FIXME: Other man sections are "context, implementation notes,
  ;; return values, files, exit status, diagnostics, errors, see also,
  ;; standards, history, caveats, bugs, and security considerations";
  ;; not sure how we'd go about generating them, but it would be good
  ;; to do so.
  (labels ((wrap-paragraph (par)
             (split-sequence #\Newline (bobbin:wrap par wrap-at)))
           (timestamp ()
             (let ((now (multiple-value-list (decode-universal-time (get-universal-time)))))
               (format NIL "~A-~A-~A" (nth 5 now) (nth 4 now) (nth 3 now)))))
    (format stream ".Dd ~A~%~
                  .Dt ~:@(~A~) 1~%~
                  .Os~%~
                  .Sh NAME~%~
                  .Nm ~:*~A~%~
                  .Nd ~A~%"
            (timestamp)
            (command-full-name top-level)
            (command-description top-level))

    (format stream ".Sh SYNOPSIS~%")
    (with-command-tree (node top-level)
      (initialize-command node)
      (format stream ".Nm ~A~%" (command-full-name node))
      (loop :for opt :in (command-options node)
            :unless (option-hidden-p opt)
              :do (format stream ".Op Fl~:[~:; ~:*~A,~]~
                                 ~:[~:; -~:*~A~]~
                                 ~:[~:; Ar ~:*~A~]~%"
                          (option-short-name opt)
                          (option-long-name opt)
                          (option-parameter opt)))
      ;; FIXME: As it stands there is not really any good way to
      ;; display the command's arguments; the closest thing is
      ;; `command-usage' with the huge downside that clingon wants you
      ;; to specify arguments in there too, which clashes really badly
      ;; with the rest of the synopsis.  Need to ask advice on how
      ;; best to approach this.
      (when (command-usage node)
        (format stream ".Op Ar ~A~%" (command-usage node))))

    ;; FIXME: It would be good to group options by category, but I'm
    ;; not sure how to do it without inserting subsections at the same
    ;; level as subcommand names, which isn't ideal
    (format stream ".Sh DESCRIPTION~%")
    (with-command-tree (node top-level)
      (initialize-command node)
      (unless (eq node top-level)
        (format stream ".Ss ~A~A~A~%"
                (command-full-name node)
                (if (command-aliases node)
                    (format NIL " (alias~:*~[es~;~:;es~]: ~{~(~A~)~^, ~})"
                            (length (command-aliases node))
                            (command-aliases node))
                    "")
                (if (command-usage node)
                    (format NIL " ~A" (command-usage node))
                    "")))
      (let* ((desc (if (command-long-description node)
                       (command-long-description node)
                       (command-description node))))
        (format stream ".Pp~%")
        (loop :for line :in (wrap-paragraph desc) :do (format stream "~A~%" line)))
      (format stream ".Bl -tag -width Ds~%")
      (loop :for opt :in (command-options node)
            :unless (option-hidden-p opt)
              :do (format stream ".It Fl~:[~:; ~:*~A,~]~
                                     ~:[~:; -~:*~A~]~
                                     ~:[~:; Ar ~:*~A~]~
                                     ~:[~:; Ev (env: ~:*~{~A~^, ~})~]~
                                     ~:[~:; Dv (default: ~:*~A)~]~
                                     ~:[~:; Sy REQUIRED ~]~%~
                                ~{~A~%~}"
                         (option-short-name opt)
                         (option-long-name opt)
                         (if (eql (class-of opt)
                                  (find-class 'clingon.options:option-enum))
                             (format NIL "(options: ~{~A~^, ~})"
                                     (loop :for kv :in (clingon.options:option-enum-items opt)
                                           :collect (car kv)))
                             (option-parameter opt))
                         (clingon.options:option-env-vars opt)
                         (clingon.options:option-initial-value opt)
                         (option-required-p opt)
                         (wrap-paragraph (option-description opt))))
      (format stream ".El~%"))

    (format stream ".Sh ENVIRONMENT~%")
    (with-command-tree (node top-level)
      (initialize-command node)
      (let ((vars (loop :for opt :in (command-options node)
                        :when (clingon.options:option-env-vars opt)
                          :collect opt)))
        (when vars
          (unless (eq node top-level)
            (format stream ".Ss ~A~%" (command-full-name node)))
          (format stream ".Bl -tag -width DS~%")
          (loop :for opt :in vars
                :do (format stream ".It ~{Ev ~A~^, ~} ~
                                        Fl~:[~:; ~:*~A,~]~
                                          ~:[~:; -~:*~A~]~
                                          ~:[~:; Ar ~:*~A~]~
                                        ~:[~:; Dv (default: ~:*~A)~]~
                                        ~:[~:; Sy REQUIRED ~]~%~
                                    ~{~A~%~}"
                            (clingon.options:option-env-vars opt)
                            (option-short-name opt)
                            (option-long-name opt)
                            (option-parameter opt)
                            (clingon.options:option-initial-value opt)
                            (option-required-p opt)
                            (wrap-paragraph (option-description opt))))
          (format stream ".El~%"))))

    (format stream ".Sh EXAMPLES~%")
    (with-command-tree (node top-level)
      (initialize-command node)
      (when (and (not (eq node top-level)) (command-examples node))
        (format stream ".Ss ~A~%" (command-full-name node)))
      (loop :for (desc . code) :in (command-examples node)
            :do (format stream ".Pp~%~{~A~%~}" (wrap-paragraph desc))
                (format stream ".Bd -literal -offset indent~%")
                (format stream "~A~%" code)
                (format stream ".Ed~%")))

    (format stream ".Sh AUTHORS~%~{.An ~A~%~}" (command-authors top-level))))

(defmethod zsh-sub-command-items ((command command))
  "Returns the sub-command items, which will be populated in the Zsh completion function"
  (unless (command-sub-commands command)
    (return-from zsh-sub-command-items nil))
  (let ((items nil))
    ;; The command itself
    (push (format nil "\"~A command\"" (command-full-name command)) items)
    ;; The sub-commands
    (dolist (sub (command-sub-commands command))
      (push (format nil "\"~A[~A]\"" (command-name sub) (command-description sub)) items)
      ;; Aliases of each sub-command
      (dolist (alias (command-aliases sub))
        (push (format nil "\"~A[~A]\"" alias (format nil "alias for '~A'" (command-name sub))) items)))
    (nreverse items)))

(defmethod zsh-sub-command-dispatch-items ((command command))
  "Returns a list of of command-name -> function-name dispatch strings,
   which will be used for populating into the Zsh completion function."
  (let ((items nil))
    (dolist (sub (command-sub-commands command))
      ;; The sub-command itself
      (let* ((name (command-name sub))
             (full-path (command-full-path sub))
             (func-name (join-list full-path "_")))
        (push
         (format nil "~16A~A)~&~20A_~A~&~20A;;" #\Space name #\Space func-name #\Space)
         items)
        ;; Each alias of the sub-command
        (dolist (alias (command-aliases sub))
          (push
           (format nil "~16A~A)~&~20A_~A~&~20A;;" #\Space alias #\Space func-name #\Space)
           items))))
    (nreverse items)))

(defmethod print-documentation ((kind (eql :zsh-completions)) (top-level command) stream &key)
  "Prints the Zsh completion script for the given top-level command"
  (format stream "#compdef _~A ~A~&#~&"
          (join-list (command-full-path top-level) "_")
          (command-name top-level))
  (format stream "# Install this file to ~~/.zsh-completions and edit your ~~/.zshrc file~&")
  (format stream "# in order to include the following lines.~&#~&")
  (format stream "# fpath=(~~/.zsh-completions $fpath)~&#~&")
  (format stream "# autoload -U compinit~&")
  (format stream "# compinit~2&")
  (dolist (node (reverse (command-tree top-level)))
    (initialize-command node)
    (let* ((full-path (command-full-path node))
           (func-name (join-list full-path "_"))
           (opt-specs (mapcar (lambda (opt)
                                (format nil "~8A~A~A" #\Space
                                        (option-usage-details :zsh-option-spec opt)
                                        (option-description-details :zsh-option-spec opt)))
                              (visible-options node)))
           (sub-command-items (mapcar (lambda (sub)
                                        (format nil "~16A~A" #\Space sub))
                                      (zsh-sub-command-items node)))
           (sub-dispatch-items (zsh-sub-command-dispatch-items node)))
      (if sub-command-items
          (format stream *zsh-compfunc-with-sub-commands*
                  func-name
                  (join-list opt-specs #\Newline)
                  (join-list sub-command-items (format nil " \\~&"))
                  (join-list sub-dispatch-items #\Newline))
          (format stream *zsh-compfunc-without-sub-commands* func-name (join-list opt-specs #\Newline))))))

(defmethod print-documentation ((kind (eql :dot)) (top-level command) stream &key)
  "Prints the tree representation for the given command in Dot format"
  (format stream "digraph G {~%")
  (format stream "  node [color=lightblue fillcolor=lightblue fontcolor=black shape=record style=\"filled, rounded\"];~%")
  (loop :for node :in (command-tree top-level)
        :for parent = (command-parent node)
        :do
           (when parent
             (format stream "  \"~A\" -> \"~A\";~%" (command-name parent) (command-name node))))
  (format stream "}~%"))
