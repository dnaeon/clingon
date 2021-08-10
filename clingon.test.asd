(defpackage :clingon-test-system
  (:use :cl :asdf))
(in-package :clingon-test-system)

(defsystem "clingon.test"
  :name "clingon.test"
  :long-name "clingon.test"
  :description "Test suite for the :clingon system"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/clingon"
  :bug-tracker "https://github.com/dnaeon/clingon"
  :source-control "https://github.com/dnaeon/clingon"
  :depends-on (:clingon
	       :rove)
  :components ((:module "tests"
		:pathname #P"tests/"
		:serial t
		:components ((:file "test-package")
			     (:file "test-utils")
			     (:file "test-options")
			     (:file "test-command"))))
  :perform (test-op (op c) (uiop:symbol-call :rove :run-suite :clingon.test)))
