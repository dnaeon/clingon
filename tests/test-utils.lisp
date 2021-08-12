(in-package :clingon.test)

(defun get-neighbors (graph)
  "Returns a function which discovers the adjacent nodes in the given graph.
   The function is meant to be used with the sample graphs from the
   this test package."
  (lambda (name)
    (let ((node (find name graph :key (lambda (x) (getf x :node)))))
      (getf node :neighbors))))

(deftest graph-walk
  (testing "Simple graph"
    (let* ((g '((:node :A :neighbors (:B :C))
		(:node :B :neighbors (:D :A))
		(:node :C :neighbors (:D :A))
		(:node :D :neighbors (:B :C))))
	   (dfs-from-a (clingon.utils:walk :A (get-neighbors g) :order :dfs))
	   (want-dfs-from-a '(:A :B :D :C))
	   (dfs-from-c (clingon.utils:walk :C (get-neighbors g) :order :dfs))
	   (want-dfs-from-c '(:C :D :B :A))
	   (dfs-from-d (clingon.utils:walk :D (get-neighbors g) :order :dfs))
	   (want-dfs-from-d '(:D :B :A :C))
	   (bfs-from-a (clingon.utils:walk :A (get-neighbors g) :order :bfs))
	   (want-bfs-from-a '(:A :B :C :D)))
      (ok (equal want-dfs-from-a dfs-from-a) "DFS walk from :A root")
      (ok (equal want-dfs-from-c dfs-from-c) "DFS walk from :C root")
      (ok (equal want-dfs-from-d dfs-from-d) "DFS walk from :D root")
      (ok (equal want-bfs-from-a bfs-from-a) "BFS walk from :A root")))

  (testing "Tremaux tree"
    (let* ((g '((:node :A :neighbors (:B :C :E))
		(:node :B :neighbors (:D :F :A))
		(:node :C :neighbors (:G :A))
		(:node :D :neighbors (:B))
		(:node :E :neighbors (:F :A))
		(:node :F :neighbors (:B :E))
		(:node :G :neighbors (:C))))
	   (dfs-from-a (clingon.utils:walk :A (get-neighbors g) :order :dfs))
	   (want-dfs-from-a '(:A :B :D :F :E :C :G))
	   (bfs-from-a (clingon.utils:walk :A (get-neighbors g) :order :bfs))
	   (want-bfs-from-a '(:A :B :C :E :D :F :G)))
      (ok (equal want-dfs-from-a dfs-from-a) "DFS walk from :A root")
      (ok (equal want-bfs-from-a bfs-from-a) "BFS walk from :A root"))))

(deftest join-list
  (testing "non-empty list"
    (ok (string= "foo,bar,baz" (clingon:join-list '("foo" "bar" "baz") ","))
	"joined string matches")
    (ok (string= "127.0.0.1" (clingon:join-list '(127 0 0 1) "."))
	"joined string matches"))
  (testing "empty list"
    (ok (string= "" (clingon:join-list nil "."))
	"joined string matches")))
