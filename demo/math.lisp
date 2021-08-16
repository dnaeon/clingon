(in-package :clingon.demo)

(defun math/options ()
  "Returns the options for the `math' command"
  (list
   (clingon:make-option :enum
			:description "operation to perform"
			:short-name #\o
			:long-name "operation"
			:required t
			:items `(("add" . ,#'+)
				 ("sub" . ,#'-)
				 ("mul" . ,#'*)
				 ("div" . ,#'/))
			:key :math/operation)
   (clingon:make-option :list/integer
			:description "integers to work on"
			:short-name #\i
			:long-name "int"
			:required t
			:key :math/integers)))

(defun math/handler (cmd)
  "Handler for the `math' command"
  (let ((operation (clingon:getopt cmd :math/operation))
	(integers (clingon:getopt cmd :math/integers)))
    (format t "The result is ~A~%" (apply operation integers))))

(defun math/command ()
  "Creates a new command to do some basic math"
  (clingon:make-command
   :name "math"
   :usage "-o <OPERATION> -i <INT> ..."
   :description "perform basic math on integers"
   :options (math/options)
   :handler #'math/handler))
