(in-package :cl-user)
(defpackage :clingon.utils
  (:use :cl)
  (:export
   :walk))
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
  (uiop:command-line-arguments))
