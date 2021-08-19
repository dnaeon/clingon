LISP ?= sbcl

intro:
	${LISP} --eval '(ql:quickload :clingon.intro)' \
		--eval '(asdf:make :clingon.intro)' \
                --eval '(quit)'

demo:
	${LISP} --eval '(ql:quickload :clingon.demo)' \
		--eval '(asdf:make :clingon.demo)' \
                --eval '(quit)'

demo-doc: demo
	./clingon-demo print-doc > docs/clingon-demo.md

test:
	${LISP} --eval '(ql:quickload :clingon.test)' \
		--eval '(setf rove:*enable-colors* nil)' \
		--eval '(asdf:test-system :clingon.test)' \
                --eval '(quit)'

.PHONY: intro demo test
