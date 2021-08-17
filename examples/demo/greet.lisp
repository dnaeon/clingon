(in-package :clingon.demo)

(defun greet/options ()
  "Returns the options for the `greet' command"
  (list
   (clingon:make-option :string
			:description "Person to greet"
			:short-name #\u
			:long-name "user"
			:env-vars '("USER")
			:key :user)))

(defun greet/handler (cmd)
  "Handler for the `greet' command"
  (let ((who (clingon:getopt cmd :user "Stranger")))
    (format t "Hello, ~A!~%" who)))

(defun greet/command ()
  "Creates a new command to perform basic math on integers"
  (clingon:make-command
   :name "greet"
   :aliases '("hi" "hey")
   :description "greets people"
   :version "0.1.0"
   :options (greet/options)
   :handler #'greet/handler))
