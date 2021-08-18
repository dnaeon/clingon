#!/usr/bin/env sh

set -e

sbcl --eval '(ql:quickload :clingon.test)' \
     --eval '(setf rove:*enable-colors* nil)' \
     --eval '(asdf:test-system :clingon.test)' \
     --eval '(quit)'
