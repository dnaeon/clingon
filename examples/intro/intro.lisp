(in-package :cl-user)
(defpackage :clingon.intro
  (:use :cl)
  (:import-from :clingon)
  (:export :main))
(in-package :clingon.intro)

(defun shout/handler (cmd)
  "The handler for the `shout' command"
  (let ((args (mapcar #'string-upcase (clingon:command-arguments cmd)))
	(user (clingon:getopt cmd :user))) ;; <- a global option
    (format t "HEY, ~A!~%" user)
    (format t "~A!~%" (clingon:join-list args #\Space))))

(defun shout/command ()
  "Returns a command which SHOUTS back anything we write on the command-line"
  (clingon:make-command
   :name "shout"
   :description "shouts back anything you write"
   :usage "[options] [arguments ...]"
   :handler #'shout/handler))

(defun top-level/options ()
  "Creates and returns the options for the top-level command"
  (list
   (clingon:make-option
    :counter
    :description "verbosity level"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)
   (clingon:make-option
    :string
    :description "user to greet"
    :short-name #\u
    :long-name "user"
    :initial-value "stranger"
    :env-vars '("USER")
    :key :user)))

(defun top-level/handler (cmd)
  "The top-level handler"
  (let ((args (clingon:command-arguments cmd))
	(user (clingon:getopt cmd :user))
	(verbose (clingon:getopt cmd :verbose)))
    (format t "Hello, ~A!~%" user)
    (format t "The current verbosity level is set to ~A~%" verbose)
    (format t "You have provided ~A arguments~%" (length args))
    (format t "Bye.~%")))

(defun top-level/command ()
  "Creates and returns the top-level command"
  (clingon:make-command
   :name "clingon-intro"
   :description "my first clingon cli app"
   :version "0.1.0"
   :license "BSD 2-Clause"
   :authors '("John Doe <john.doe@example.com>")
   :usage "[-v] [-u <USER>]"
   :options (top-level/options)
   :handler #'top-level/handler
   :sub-commands (list (shout/command))))

(defun main ()
  (let ((app (top-level/command)))
    (clingon:run app)))
