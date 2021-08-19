;; Copyright (c) 2021 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :clingon.demo)

(defun math/options ()
  "Returns the options for the `math' command"
  (list
   (clingon:make-option :enum
			:description "operation to perform"
			:short-name #\o
			:long-name "operation"
			:required t
			:items `(("add" . ,#'+)
				 ("sub" . ,#'-)
				 ("mul" . ,#'*)
				 ("div" . ,#'/))
			:key :math/operation)
   (clingon:make-option :list/integer
			:description "integers to work on"
			:short-name #\i
			:long-name "int"
			:required t
			:key :math/integers)))

(defun math/handler (cmd)
  "Handler for the `math' command"
  (let ((operation (clingon:getopt cmd :math/operation))
	(integers (clingon:getopt cmd :math/integers)))
    (format t "The result is ~A~%" (apply operation integers))))

(defun math/command ()
  "Creates a new command to do some basic math"
  (clingon:make-command
   :name "math"
   :usage "-o <OPERATION> -i <INT> ..."
   :description "perform basic math on integers"
   :options (math/options)
   :handler #'math/handler
   :examples '(("Sum some numbers:" . "clingon-demo math -o add -i 1 -i 42 -i 84")
	       ("Multiply some numbers:" . "clingon-demo math -o mul -i 2 -i 3 -i 4"))))
