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

(defun engine/options ()
  "Returns the options for the `engine' command"
  (list
   (clingon:make-option :switch
			:description "state of our engine"
			:short-name #\s
			:long-name "state"
			:required t
			:key :engine-state)))

(defun engine/handler (cmd)
  "Handler for the `engine' command"
  (let ((state (clingon:getopt cmd :engine-state)))
    (if state
	(format t "Starting engine.~%")
	(format t "Stopping engine.~%"))))

(defun engine/command ()
  "Creates a new command to switch the state of our engine"
  (clingon:make-command
   :name "engine"
   :usage "-s <STATE>"
   :description "start or stop an imaginary engine"
   :options (engine/options)
   :handler #'engine/handler
   :examples '(("Start engine:" . "clingon-demo engine --state=on")
	       ("Stop engine:" . "clingon-demo engine --state=off"))))
