(in-package :clingon.example.demo)

(defun top-level/options ()
  "Returns the options for the top-level command"
  (list
   (clingon:make-option :counter
			:description "how noisy we want to be"
			:short-name #\v
			:long-name "verbose"
			:key :verbose)))

(defun top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (greet/command)))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command :name "clingon-demo"
			:version "0.1.0"
			:description "The clingon demo app"
			:authors '("Marin Atanasov Nikolov <dnaeon@gmail.com>")
			:sub-commands (top-level/sub-commands)))

(defun main ()
  "The main entrypoint of our demo app"
  (let ((app (top-level/command)))
    (clingon:run app)))
