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

(defun logging/enable/command ()
  "Returns the `logging enable' command"
  (clingon:make-command :name "enable"
                        :description "enables logging"
                        :handler (lambda (cmd)
                                   (declare (ignore cmd))
                                   (format t "Enabling logging~&"))))

(defun logging/disable/command ()
  "Returns the `logging disable' command"
  (clingon:make-command :name "disable"
                        :description "disables logging"
                        :handler (lambda (cmd)
                                   (declare (ignore cmd))
                                   (format t "Disabling logging~&"))))

(defun logging/options ()
  "Returns the options for the `logging' command"
  (list
   (clingon:make-option :enum
                        :description "level to configure"
                        :short-name #\l
                        :long-name "level"
                        :parameter "LEVEL"
                        :env-vars '("LOG_LEVEL")
                        :items '(("info" . :info)
                                 ("warn" . :warn)
                                 ("error" . :error)
                                 ("debug" . :debug))
                        :initial-value "info"
                        :key :log-level)))

(defun logging/handler (cmd)
  "Handler for the `logging' command"
  (let ((verbose (clingon:getopt cmd :verbose)) ;; <- global option
        (level (clingon:getopt cmd :log-level)))
    (format t "Global verbose option is set to ~A~&" verbose)
    (format t "Configuring log level to ~A~&" level)))

(defun logging/sub-commands ()
  "Returns the sub-commands for the `logging' command"
  (list
   (logging/enable/command)
   (logging/disable/command)))

(defun logging/command ()
  "Creates a new command to configure logging"
  (clingon:make-command
   :name "logging"
   :aliases '("log")
   :description "configure the logging system"
   :options (logging/options)
   :sub-commands (logging/sub-commands)
   :handler #'logging/handler
   :examples '(("Configure logging level:" . "clingon-demo -vvv logging --level=debug")
               ("Enable logging:" . "clingon-demo logging enable")
               ("Disable logging:" . "clingon-demo logging disable"))))
