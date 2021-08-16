(defpackage :clingon-demo-system
  (:use :cl :asdf))
(in-package :clingon-demo-system)

(defsystem "clingon.demo"
  :name "clingon.demo"
  :long-name "clingon.demo"
  :description "Example demo of the Common Lisp clingon system"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/clingon"
  :bug-tracker "https://github.com/dnaeon/clingon"
  :source-control "https://github.com/dnaeon/clingon"
  :depends-on (:clingon)
  :components ((:module "demo"
		:serial t
		:pathname #P"demo/"
		:components ((:file "package")
			     (:file "greet")
			     (:file "logging")
			     (:file "math")
			     (:file "echo")
			     (:file "main"))))

  :build-operation "program-op"
  :build-pathname "clingon-demo"
  :entry-point "clingon.demo:main")
