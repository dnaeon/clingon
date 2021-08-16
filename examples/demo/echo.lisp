(in-package :clingon.demo)

(defun echo/handler (cmd)
  "Handler for the `echo' command"
  (dolist (arg (clingon:command-arguments cmd))
    (format t "~A~&" arg)))

(defun echo/command ()
  "Creates a new command which echoes every argument we are given"
  (clingon:make-command
   :name "echo"
   :usage "[ARGUMENT ...]"
   :description "echoes back each argument on a newline"
   :handler #'echo/handler))
