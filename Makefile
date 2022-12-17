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
	./bin/clingon-demo print-doc > docs/clingon-demo.md

test:
	./scripts/run-tests.sh

.PHONY: intro demo demo-doc test
