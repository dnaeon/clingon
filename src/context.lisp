(in-package :cl-user)
(defpackage :clingon.context
  (:use :cl)
  (:import-from
   :clingon.options
   :option-key
   :option-value
   :initialize-option
   :finalize-option)
  (:export
   :context
   :context-parent
   :context-initial-argv
   :context-arguments
   :context-options
   :context-reduced-opts
   :context-is-finalized-p
   :make-context
   :initialize-context
   :finalize-context))
(in-package :clingon.context)

(defgeneric initialize-context (context &key)
  (:documentation "Initializes a context"))

(defgeneric finalize-context (context &key)
  (:documentation "Finalizes a context"))

(defclass context ()
  ((parent
    :initarg :parent
    :initform nil
    :reader context-parent
    :documentation "A parent context")
   (initial-argv
    :initarg :initial-argv
    :initform (error "Must specify initial argv")
    :reader context-initial-argv
    :documentation "Initial context arguments")
   (arguments
    :initarg :arguments
    :initform nil
    :accessor context-arguments
    :documentation "Discovered free arguments")
   (options
    :initarg :options
    :initform nil
    :accessor context-options
    :documentation "Command options for the context")
   (reduced-opts
    :initarg :reduced-opts
    :initform nil
    :accessor context-reduced-opts
    :documentation "Reduced options, which are set when finalizing a context")
   (is-finalized-p
    :initarg :is-finalized-p
    :initform nil
    :accessor context-is-finalized-p
    :documentation "Predicate which returns T if the context has been finalized"))
  (:documentation "A context class represents the environment in which a command runs"))

(defun make-context (&rest rest)
  "Creates a new CONTEXT instance"
  (apply #'make-instance 'context rest))

(defmethod initialize-context ((context context) &key)
  "Initializes the context. Also performs initialization of the context options"
  (dolist (option (context-options context))
    (initialize-option option)))

(defmethod finalize-context ((context context) &key)
  "Finalizes the context and derives the reduced set of options"
  (when (context-is-finalized-p context)
    (error "Context is already finalized"))
  (let ((result (context-reduced-opts context)))
    (dolist (option (context-options context))
      (finalize-option option)
      (setf (gethash (option-key option) result) (option-value option)))
    (setf (context-is-finalized-p context) t)))
