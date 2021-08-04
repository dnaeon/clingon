(in-package :cl-user)
(defpackage :clingon.context
  (:use :cl)
  (:import-from
   :clingon.options
   :option-key
   :option-value
   :option-parameter
   :option-short-name
   :option-long-name
   :option-reduce-fn
   :initialize-option
   :finalize-option)
  (:import-from
   :clingon.generics
   :find-short-option
   :find-long-option
   :parse-option)
  (:export
   :context
   :context-parent
   :context-initial-argv
   :context-arguments
   :context-options
   :context-reduced-options
   :make-context
   :initialize-context
   :finalize-context
   :make-child-context))
(in-package :clingon.context)

(defgeneric initialize-context (context &key)
  (:documentation "Initializes a context"))

(defgeneric finalize-context (context &key)
  (:documentation "Finalizes a context"))

(define-condition unknown-option (error)
  ((name
    :initarg :name
    :accessor unknown-option-name)
   (kind
    :initarg :kind
    :accessor unknown-option-kind))
  (:report (lambda (condition stream)
	     (format stream "Unknown option ~A of kind ~A"
		     (unknown-option-name condition)
		     (unknown-option-kind condition))))
  (:documentation "A condition which is signalled when an unknown option is seen"))

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
      (finalize-option option)
      (setf (gethash (option-key option) result) (option-value option)))))

(defmethod find-short-option ((context context) name &key)
  (find name (context-options context) :key #'option-short-name :test #'char=))

(defmethod find-long-option ((context context) name &key)
  (find name (context-options context) :key #'option-long-name :test #'string=))

(defmethod make-child-context ((context context))
  "Makes a child context from the given context"
  (make-context :initial-argv (copy-list (context-initial-argv context))
		:parent context))

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
         (option (find-short-option context short-name)))
    (unless option
      (error 'unknown-option :kind :short :name (format nil "-~A" short-name)))
    (let ((current-value (option-value option))
          (reduce-fn (option-reduce-fn option)))
      (cond
	;; Option takes a parameter
	((option-parameter option)
	 (let ((optarg (if (> (length arg) 2)
			   (subseq arg 2) ;; -xfoo
			   (pop (context-initial-argv context))))) ;; -x foo
	   (setf (option-value option)
		 (funcall reduce-fn current-value optarg))))
	;; Option does not take a parameter
	(t
	 (setf (option-value option)
	       (funcall reduce-fn current-value))
	 ;; Options may be collapsed into a single argument, e.g. `-abc'
	 ;; For non-parameter option which length is greater than 2,
	 ;; we should push the rest of the options for processing.
	 (when (> (length arg) 2)
	   (let ((next-option (format nil "-~A" (subseq arg 2))))
	     (push next-option (context-initial-argv context)))))))))

(defmethod parse-option ((kind (eql :long)) (context context) &key)
  "Parses a long option from the arguments of the context"
  (let* ((arg (pop (context-initial-argv context)))
	 (equals-position (position #\= arg))
         (long-name (subseq arg 2 equals-position))
         (option (find-long-option context long-name)))
    (unless option
      (error 'unknown-option :kind :long :name (format nil "--~A" long-name)))
    (let* ((current-value (option-value option))
           (reduce-fn (option-reduce-fn option)))
      (cond
	;; Option takes a parameter
	((option-parameter option)
	 (let ((optarg (if equals-position
			   (subseq arg (1+ equals-position))       ;; --arg=foo
			   (pop (context-initial-argv context))))) ;; --arg foo
	   (setf (option-value option)
		 (funcall reduce-fn current-value optarg))))
	;; Option does not take a parameter
	(t
	 (setf (option-value option)
	       (funcall reduce-fn current-value)))))))

