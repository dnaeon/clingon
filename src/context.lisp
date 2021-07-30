(in-package :cl-user)
(defpackage :clingon.context
  (:use :cl)
  (:import-from
   :clingon.options
   :option-key
   :option-value
   :option-short-name
   :option-long-name
   :initialize-option
   :finalize-option)
  (:import-from
   :clingon.generics
   :find-short-option
   :find-long-option)
  (:export
   :context
   :context-parent
   :context-initial-argv
   :context-arguments
   :context-options
   :context-reduced-opts
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
   (reduced-opts
    :initarg :reduced-opts
    :initform (make-hash-table :test #'equal)
    :accessor context-reduced-opts
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
  (let ((result (context-reduced-opts context)))
    (nreverse (context-arguments context))
    (dolist (option (context-options context))
      (finalize-option option)
      (setf (gethash (option-key option) result) (option-value option)))))

(defmethod find-short-option ((context context) name &key)
  (find name (context-options context) :key #'option-short-name :test #'char=))

(defmethod find-long-option ((context context) name &key)
  (find name (context-options context) :key #'option-long-name :test #'string=))
