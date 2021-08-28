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

(defun sleep/options ()
  "Returns the options for the `sleep' command"
  (list
   (clingon:make-option
    :integer
    :short-name #\s
    :long-name "seconds"
    :description "number of seconds to sleep"
    :initial-value 60
    :key :seconds)))

(defun sleep/handler (cmd)
  "Handler for the `sleep' command"
  (let ((seconds (clingon:getopt cmd :seconds)))
    (format t "Sleeping for ~d seconds. Press CTRL-C to interrupt.~%" seconds)
    (sleep (clingon:getopt cmd :seconds))))

(defun sleep/command ()
  "Creates a new command which sleeps for a given period of time"
  (clingon:make-command
   :name "sleep"
   :description "sleeps for the given period of time"
   :options (sleep/options)
   :handler #'sleep/handler
   :examples '(("Sleep for 60 seconds. Send SIGINT via CTRL-C to catch the signal" . "clingon-demo sleep --seconds 60"))))
