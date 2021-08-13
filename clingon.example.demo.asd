(defpackage :clingon-example-demo-system
  (:use :cl :asdf))
(in-package :clingon-example-demo-system)

(defsystem "clingon.example.demo"
  :name "clingon.example.demo"
  :long-name "clingon.example.demo"
  :description "Example demo of the Common Lisp clingon system"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/clingon"
  :bug-tracker "https://github.com/dnaeon/clingon"
  :source-control "https://github.com/dnaeon/clingon"
  :depends-on (:clingon)
  :components ((:module "core"
		:pathname #P"examples/demo/"
		:components ((:file "package")
			     (:file "greet")
			     (:file "main"))))

  :build-operation "program-op"
  :build-pathname "clingon-demo"
  :entry-point "clingon.example.demo:main")
