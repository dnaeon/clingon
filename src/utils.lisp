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

(in-package :cl-user)
(defpackage :clingon.utils
  (:use :cl)
  (:export
   :walk
   :join-list
   :exit
   :git-rev-parse
   :group-by
   :hashtable-keys
   :hashtable-values))
(in-package :clingon.utils)

(defun walk (root neighbors-func &key (order :dfs))
  "Walks a tree structure starting from ROOT. Neighbors of each node are
  discovered by invoking the NEIGHBORS-FUNC function, which should accept a
  single argument -- the node we are currently visiting, and should
  return a list of adjacent nodes.

  The ORDER should be either :dfs or :bfs for Depth-First Search or
  Breadth-First Search respectively."
  (let ((to-visit (list root))
        (visited nil))
    (loop :while to-visit
          :for node = (pop to-visit)
          :for neighbors = (funcall neighbors-func node)
          :for not-seen = (remove-if (lambda (x)
                                       (member x visited :test #'equal))
                                     neighbors)
          :do
             (ecase order
               (:bfs (setf to-visit (nconc to-visit not-seen)))
               (:dfs (setf to-visit (nconc not-seen to-visit))))
             (unless (member node visited :test #'equal)
               (push node visited)))
    (nreverse visited)))

(defun argv ()
  "Returns the list of command-line arguments"
  #+ecl
  (rest (uiop:raw-command-line-arguments))
  #-ecl
  (uiop:command-line-arguments))

(defun join-list (list separator)
  "Returns a string representing the items in the given LIST with SEPARATOR between each item"
  (with-output-to-string (s)
    (loop :for (item . remaining) :on list :while item :do
      (if remaining
          (format s "~A~A" item separator)
          (format s "~A" item)))))

(defun exit (&optional (code 0))
  "Exit the program returning the given exit code to the operating system"
  ;; Do not exit if we are running from SLY or SLIME
  (unless (some (lambda (feat)
                  (member feat *features*))
                (list :slynk :swank))
    (uiop:quit code)))

(defun git-rev-parse (&key short (rev "HEAD") (path "."))
  "Returns the git revision with the given REV"
  (let ((args (if short
                  (list "git" "-C" path "rev-parse" "--short" rev)
                  (list "git" "-C" path "rev-parse" rev))))
    (uiop:run-program args :output '(:string :stripped t))))

(defun group-by (sequence predicate)
  "Groups the items from SEQUENCE based on the result from PREDICATE"
  (reduce (lambda (acc item)
            (let* ((key (funcall predicate item))
                   (group (gethash key acc nil)))
              (setf (gethash key acc) (cons item group))
              acc))
          sequence
          :initial-value (make-hash-table :test #'equal)))

(defun hashtable-keys (htable)
  "Returns the keys from the given hashtable"
  (let ((result nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k result))
             htable)
    result))

(defun hashtable-values (htable)
  "Returns the values from the given hashtable"
  (let ((result nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v result))
             htable)
    result))
