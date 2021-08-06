(in-package :cl-user)
(defpackage :clingon.context
  (:use :cl)
  (:import-from
   :clingon.conditions
   :circular-dependency
   :unknown-option
   :missing-option-argument)
  (:import-from
   :clingon.options
   :option-key
   :option-value
   :option-parameter
   :option-short-name
   :option-long-name
   :option-reduce-fn
   :option-is-set-p
   :initialize-option
   :finalize-option)
  (:import-from
   :clingon.generics
   :find-short-option
   :find-long-option
   :parse-option
   :initialize-context
   :finalize-context)
  (:export
   :context
   :context-parent
   :context-initial-argv
   :context-arguments
   :context-options
   :context-reduced-options
   :context-lineage
   :make-context
   :initialize-context
   :finalize-context
   :make-child-context))
(in-package :clingon.context)

(defclass context ()
  ((parent
    :initarg :parent
    :initform nil
    :reader context-parent
    :documentation "A parent context")
   (initial-argv
    :initarg :initial-argv
    :initform (error "Must specify initial argv")
    :accessor context-initial-argv
    :documentation "Initial arguments for the context. These will be consumed during parsing")
   (arguments
    :initarg :arguments
    :initform nil
    :accessor context-arguments
    :documentation "Discovered free arguments")
   (options
    :initarg :options
    :initform nil
    :accessor context-options
    :documentation "Command-line options for the context")
   (reduced-options
    :initarg :reduced-options
    :initform (make-hash-table :test #'equal)
    :accessor context-reduced-options
    :documentation "Reduced options, which are set when finalizing a context"))
  (:documentation "A context class represents the environment in which a command runs"))

(defmethod print-object ((context context) stream)
  (print-unreadable-object (context stream :type t)
    (format stream "options=~A" (length (context-options context)))))

(defun make-context (&rest rest)
  "Creates a new CONTEXT instance"
  (apply #'make-instance 'context rest))

(defmethod initialize-context ((context context) &key)
  "Initializes the context."
  (dolist (option (context-options context))
    (initialize-option option)))

(defmethod finalize-context ((context context) &key)
  "Finalizes the context and derives the reduced set of options"
  (setf (context-initial-argv context) nil)
  (let ((result (context-reduced-options context)))
    (setf (context-arguments context)
          (nreverse (context-arguments context)))
    (dolist (option (context-options context))
      (when (option-is-set-p option)
	(finalize-option option)
	(setf (gethash (option-key option) result) (option-value option))))))

(defmethod find-short-option ((context context) name &key)
  (find name (context-options context) :key #'option-short-name :test #'char=))

(defmethod find-long-option ((context context) name &key)
  (find name (context-options context) :key #'option-long-name :test #'string=))

(defmethod make-child-context ((context context))
  "Makes a child context from the given context"
  (make-context :initial-argv (copy-list (context-initial-argv context))
                :parent context))

(defmethod context-lineage ((context context))
  "Returns the context lineage"
  (loop :for c = context :then (context-parent c)
        :while c
        :when (member c visited :test #'equal) :do
          (error 'circular-dependency :items visited)
        :collect c :into visited
        :finally (return visited)))

(defmethod parse-option ((kind (eql :consume-all-arguments)) (context context) &key)
  "Consumes all arguments after the end-of-options flag"
  (pop (context-initial-argv context)) ;; Drop the end-of-options (`--') argument
    (loop :for arg = (pop (context-initial-argv context)) :while arg :do
      (push arg (context-arguments context))))

(defmethod parse-option ((kind (eql :free-argument)) (context context) &key)
  "Consume the option and treat it as a free argument"
  (let ((arg (pop (context-initial-argv context))))
    (push arg (context-arguments context))))

(defmethod parse-option ((kind (eql :short)) (context context) &key)
  "Parses a short option from the arguments of the context"
  (let* ((arg (pop (context-initial-argv context)))
         (short-name (aref arg 1))
         (short-name-full (subseq arg 0 2))
         (option (find-short-option context short-name)))
    ;; Unknown option
    (unless option
      ;; Push remaining options, if the argument contains collapsed
      ;; options. For example the arg `-abcd' where `a' is an unknown
      ;; option would push `-bcd' for further processing.
      (when (> (length arg) 2)
        (push (format nil "-~A" (subseq arg 2)) (context-initial-argv context)))
      (restart-case (error 'unknown-option :kind :short :name short-name-full)
        (discard-option ()
          :report "Discard the unknown option"
          (return-from parse-option))
        (treat-as-argument ()
          :report "Treat the unknown option as a free argument"
          (push short-name-full (context-arguments context))
          (return-from parse-option))
        (supply-new-value (value)
          :report "Supply a new value to be parsed"
          :interactive (lambda ()
                         (format *query-io* "New option to parse: ")
                         (force-output *query-io*)
                         (list (read-line *query-io*)))
          (push value (context-initial-argv context))
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
                           (pop (context-initial-argv context))))) ;; -x foo
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
             (push next-option (context-initial-argv context)))))))))

(defmethod parse-option ((kind (eql :long)) (context context) &key)
  "Parses a long option from the arguments of the context"
  (let* ((arg (pop (context-initial-argv context)))
         (equals-position (position #\= arg))
         (long-name (subseq arg 2 equals-position))
         (long-name-full (format nil "--~A" long-name))
         (option (find-long-option context long-name)))
    ;; Unknown option
    (unless option
      (restart-case (error 'unknown-option :kind :long :name long-name-full)
        (discard-option ()
          :report "Discard the unknown option"
          (return-from parse-option))
        (treat-as-argument ()
          :report "Treat the unknown option as a free argument"
          (push long-name-full (context-arguments context))
          (return-from parse-option))
        (supply-new-value (value)
          :report "Supply a new value to be parsed"
          :interactive (lambda ()
                         (format *query-io* "New option to parse: ")
                         (force-output *query-io*)
                         (list (read-line *query-io*)))
          (push value (context-initial-argv context))
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
                           (pop (context-initial-argv context))))) ;; --arg foo
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
