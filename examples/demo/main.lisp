(in-package :clingon.demo)

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
   (greet/command)
   (logging/command)
   (math/command)
   (echo/command)))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command :name "clingon-demo"
                        :version "0.1.0"
                        :description "The clingon demo app"
                        :long-description (format nil "A demo CLI application ~
                                                       showing some of the features of the ~
                                                       Common Lisp system for parsing ~
                                                       command-line arguments -- clingon.")
                        :authors '("Marin Atanasov Nikolov <dnaeon@gmail.com>")
                        :license "BSD 2-Clause"
                        :options (top-level/options)
                        :sub-commands (top-level/sub-commands)
                        :examples '(("Echo back each argument on a newline:" . "clingon-demo echo foo bar baz")
                                    ("Sum some numbers:" . "clingon-demo math -o add -i 1 -i 42 -i 84")
                                    ("Multiply some numbers:" . "clingon-demo math -o mul -i 2 -i 3 -i 4")
                                    ("Configure logging level:" . "clingon-demo -vvv logging --level=debug")
                                    ("Enable logging:" . "clingon-demo logging enable")
                                    ("Disable logging:" . "clingon-demo logging disable")
                                    ("Greet someone:" . "clingon-demo greet --user Lisper"))))

(defun main ()
  "The main entrypoint of our demo app"
  (let ((app (top-level/command)))
    (clingon:run app)))
