(defpackage :clingon-system
  (:use :cl :asdf))
(in-package :clingon-system)

(defsystem "clingon"
  :name "clingon"
  :long-name "clingon"
  :description "Command-line options parser system for Common Lisp"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.org"))
  :homepage "https://github.com/dnaeon/clingon"
  :bug-tracker "https://github.com/dnaeon/clingon"
  :source-control "https://github.com/dnaeon/clingon"
  :depends-on ()
  :components ((:module "core"
		:pathname #P"src/"
		:components ((:file "generics")
			     (:file "options")
			     (:file "context" :depends-on ("generics" "options"))
			     (:file "command" :depends-on ("generics" "context"))))
	       (:module "client-package"
		:pathname #P"src/"
		:components ((:file "package"))
		:depends-on ("core")))
  :in-order-to ((test-op (test-op "clingon.test"))))
