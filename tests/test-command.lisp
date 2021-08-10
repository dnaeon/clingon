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

