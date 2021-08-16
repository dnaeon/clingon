(in-package :clingon.demo)

(defun logging/enable/command ()
  "Returns the `logging enable' command"
  (clingon:make-command :name "enable"
			:usage ""
			:description "enables logging"
			:handler (lambda (cmd)
				   (declare (ignore cmd))
				   (format t "Enabling logging~&"))))

(defun logging/disable/command ()
  "Returns the `logging disable' command"
  (clingon:make-command :name "disable"
			:usage ""
			:description "disables logging"
			:handler (lambda (cmd)
				   (declare (ignore cmd))
				   (format t "Disabling logging~&"))))

(defun logging/options ()
  "Returns the options for the `logging' command"
  (list
   (clingon:make-option :enum
			:description "level to configure"
			:short-name #\l
			:long-name "level"
			:parameter "LEVEL"
			:env-vars '("LOG_LEVEL")
			:items '(("info" . :info)
				 ("warn" . :warn)
				 ("error" . :error)
				 ("debug" . :debug))
			:initial-value "info"
			:key :log-level)))

(defun logging/handler (cmd)
  "Handler for the `logging' command"
  (let ((verbose (clingon:getopt cmd :verbose)) ;; <- global option
	(level (clingon:getopt cmd :log-level)))
    (format t "Global verbose option is set to ~A~&" verbose)
    (format t "Configuring log level to ~A~&" level)))

(defun logging/sub-commands ()
  "Returns the sub-commands for the `logging' command"
  (list
   (logging/enable/command)
   (logging/disable/command)))

(defun logging/command ()
  "Creates a new command to configure logging"
  (clingon:make-command
   :name "logging"
   :usage "[-l <LEVEL>]"
   :description "configure the logging system"
   :options (logging/options)
   :sub-commands (logging/sub-commands)
   :handler #'logging/handler))
