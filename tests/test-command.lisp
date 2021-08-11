(in-package :clingon.test)

(defun foo/options ()
  "Creates some sample options"
  (list
   (clingon:make-option :generic
			:short-name #\a
			:long-name "a-option"
			:description "option a"
			:key :a)
   (clingon:make-option :generic
			:short-name #\b
			:long-name "b-option"
			:description "option b"
			:key :b)))

(defun foo/command ()
  "A sample command with options"
  (clingon:make-command :name "foo"
			:description "foo command"
			:options (foo/options)))

(defun make-duplicate-options ()
  "Returns a list of options which contain duplicates"
  (list
   (clingon:make-option :generic
			:short-name #\a
			:description "option a"
			:key :a)
   (clingon:make-option :generic
			:short-name #\a ;; <- duplicate short option
			:description "option b"
			:key :b)))

(deftest initialize-command-instance
  (testing "ensure parent is set"
    (let* ((child (clingon:make-command :name "child"
					:description "child command"))
	   (parent (clingon:make-command :name "parent"
					 :description "parent command"
					 :sub-commands (list child))))
      (ok (equal (clingon:command-parent child) parent) "parent command matches"))))

(deftest initialize-command
  (testing "ensure command arguments are nil"
    (let ((c (clingon:make-command :name "foo"
				   :description "foo command"
				   :arguments '(1 2 3))))
      (clingon:initialize-command c)
      (ok (equal (clingon:command-arguments c) nil) "free arguments are nil upon initialization"))))

(deftest finalize-command
  (testing "args to parse are nil"
    (let ((c (clingon:make-command :name "foo"
				   :description "foo command"
				   :arguments '(1 2 3))))
      (clingon:initialize-command c)
      (clingon:finalize-command c)
      (ok (equal (clingon.command:command-args-to-parse c) nil) "no more args to parse"))))

(deftest find-option
  (testing "short option"
    (let ((c (foo/command)))
      (ok (clingon:find-option :short c #\a) "short option found")))
  (testing "find long option"
    (let ((c (foo/command)))
      (ok (clingon:find-option :long c "b-option") "long option found")))
  (testing "missing option"
    (let ((c (foo/command)))
      (ng (clingon:find-option :short c #\x) "missing short option")
      (ng (clingon:find-option :long c "missing-option") "missing long option"))))

(deftest ensure-unique-options
  (testing "ensure no duplicates"
    (let ((c (clingon:make-command :name "foo"
				   :description "command with duplicate options"
				   :options (make-duplicate-options))))
      (ok (signals (clingon:ensure-unique-options c) 'clingon:duplicate-options)
	  "signals on duplicate options"))))

(deftest ensure-unique-sub-commands
  (testing "ensure no duplicate sub commands"
    (let* ((foo (clingon:make-command :name "foo" :description "foo command"))
	   (bar (clingon:make-command :name "foo" :description "bar command")) ;; <- duplicate name
	   (top-level (clingon:make-command :name "top-level"
					    :description "top-level command"
					    :sub-commands (list foo bar))))
      (ok (signals (clingon:ensure-unique-sub-commands top-level) 'clingon:duplicate-commands)
	  "signals on duplicate sub-commands"))))

(deftest command-lineage
  (testing "verify command lineage"
    (let* ((c3 (clingon:make-command :name "c3"
				     :description "c3 command"))
	   (c2 (clingon:make-command :name "c2"
				     :description "c2 command"
				     :sub-commands (list c3)))
	   (c1 (clingon:make-command :name "c1"
				     :description "c1 command"
				     :sub-commands (list c2)))
	   (top-level (clingon:make-command :name "top-level"
					    :description "top-level command"
					    :sub-commands (list c1))))
      (let ((lineage (clingon:command-lineage c3)))
	(ok (equal (list c3 c2 c1 top-level) lineage) "nodes match"))))

  (testing "circular dependencies"
    (let* ((c3 (clingon:make-command :name "c3"
				     :description "c3 command"))
	   (c2 (clingon:make-command :name "c2"
				     :description "c2 command"
				     :sub-commands (list c3)))
	   (c1 (clingon:make-command :name "c1"
				     :description "c1 command"
				     :sub-commands (list c2)))
	   (top-level (clingon:make-command :name "top-level"
					    :description "top-level command"
					    :sub-commands (list c1))))
      ;; Create a circular dependency between top-level and c3
      (setf (clingon:command-parent top-level) c3)
      (ok (signals (clingon:command-lineage c3) 'clingon:circular-dependency)
	  "signals on circular dependencies"))))
