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

(defun dot/command ()
  "Returns the command for the `dot' command"
  (clingon:make-command
   :name "dot"
   :description "generate tree representation in Dot format"
   :usage ""
   :handler (lambda (cmd)
              (let ((parent (clingon:command-parent cmd)))
                (clingon:print-documentation :dot parent t)))))

(defun top-level/options ()
  "Returns the options for the top-level command"
  (list
   (clingon:make-option :string
                        :long-name "persistent-opt"
                        :description "example persistent option"
                        :persistent t
                        :key :persistent-opt)
   (clingon:make-option :counter
                        :description "how noisy we want to be"
                        :short-name #\v
                        :long-name "verbose"
                        :key :verbose)))

(defun top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (greet/command)
   (logging/command)
   (math/command)
   (echo/command)
   (engine/command)
   (print-doc/command)
   (sleep/command)
   (zsh-completion/command)
   (dot/command)))

(defun top-level/handler (cmd)
  "The handler for the top-level command. Will print the usage of the app"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command :name "clingon-demo"
                        :version "0.1.0"
                        :description "The clingon demo app"
                        :long-description (format nil "A demo CLI application ~
                                                       showing some of the features of the ~
                                                       Common Lisp system for parsing ~
                                                       command-line arguments -- clingon.")
                        :authors '("Marin Atanasov Nikolov <dnaeon@gmail.com>")
                        :license "BSD 2-Clause"
                        :handler #'top-level/handler
                        :options (top-level/options)
                        :sub-commands (top-level/sub-commands)))

(defun main ()
  "The main entrypoint of our demo app"
  (let ((app (top-level/command)))
    (clingon:run app)))

(defun buildapp-main (argv)
  "The main entrypoint for buildapp"
  (let ((app (top-level/command)))
    (clingon:run app (rest argv))))
