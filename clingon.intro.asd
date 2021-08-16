(defpackage :clingon-intro-system
  (:use :cl :asdf))
(in-package :clingon-intro-system)

(defsystem "clingon.intro"
  :name "clingon.intro"
  :long-name "clingon.intro"
  :description "An introduction to the clingon system"
  :version "0.1.0"
  :author "John Doe <john.doe@example.org>"
  :license "BSD 2-Clause"
  :depends-on (:clingon)
  :components ((:module "intro"
		:pathname #P"examples/intro/"
		:components ((:file "intro"))))
  :build-operation "program-op"
  :build-pathname "clingon-intro"
  :entry-point "clingon.intro:main")
