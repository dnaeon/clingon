(in-package :clingon.test)

(defun foo/options ()
  "Creates some sample options"
  (list
   (clingon:make-option :boolean/true
                        :short-name #\a
                        :long-name "a-option"
                        :description "option a"
                        :key :a)
   (clingon:make-option :boolean/true
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

(deftest command-relationships
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
          "signals on circular dependencies")))

  (testing "verify full path to command"
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
                                            :sub-commands (list c1)))
           (full-path (clingon:command-full-path c3)))
      (declare (ignore top-level))
      (ok (equal '("top-level" "c1" "c2" "c3") full-path)
          "full path matches")))

  (testing "find sub-commands"
    (let* ((foo (clingon:make-command :name "foo"
                                      :description "foo command"))
           (bar (clingon:make-command :name "bar"
                                      :description "bar command"))
           (top-level (clingon:make-command :name "top-level"
                                            :description "top-level command"
                                            :sub-commands (list foo bar))))
      (ok (equal foo (clingon:find-sub-command top-level "foo"))
          "find existing command \"foo\"")
      (ok (equal bar (clingon:find-sub-command top-level "bar"))
          "find existing command \"bar\"")
      (ok (equal nil (clingon:find-sub-command top-level "INVALID"))
          "returns nil on missing command")))

  (testing "walk commands tree"
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
                                            :sub-commands (list c1)))
           (result nil))
      (clingon:with-commands-walk (c top-level)
        (push c result))
      (setf result (nreverse result))
      (ok (equal '("top-level" "c1" "c2" "c3") (mapcar #'clingon:command-name result))
          "walked nodes match"))))

(deftest parse-options
  (testing "consume all arguments"
    (let ((c (clingon:make-command :name "top-level"
                                   :description "sample top-level command"
                                   :args-to-parse (list "--" "foo" "bar" "baz" "qux"))))
      (clingon:initialize-command c)
      (clingon:parse-option :consume-all-arguments c)
      (clingon:finalize-command c)
      (ok (equal nil (clingon:command-args-to-parse c)) "args to parse is nil")
      (ok (equal '("foo" "bar" "baz" "qux") (clingon:command-arguments c))
          "free arguments match")))

  (testing "parse free arguments"
    (let ((c (clingon:make-command :name "top-level"
                                   :description "sample top-level command"
                                   :args-to-parse (list "foo" "bar" "baz"))))
      (clingon:initialize-command c)
      ;; Parse just two arguments
      (clingon:parse-option :free-argument c)
      (clingon:parse-option :free-argument c)

      (ok (equal '("baz") (clingon:command-args-to-parse c)) "remaining args to parse match")

      ;; After finalizing the command we don't have any remaining args to parse
      (clingon:finalize-command c)
      (ok (equal nil (clingon:command-args-to-parse c)) "no more args to parse")
      (ok (equal '("foo" "bar") (clingon:command-arguments c)) "free arguments match")))

  (testing "parse short and long options"
    (let ((c (foo/command)))
      ;; Set some args to parse
      (setf (clingon:command-args-to-parse c) '("-a" "-b" "--a-option" "--b-option" "-X" "--invalid"))
      (clingon:initialize-command c)
      (ok (equal :true (clingon:parse-option :short c)) "parse -a flag")
      (ok (equal :true (clingon:parse-option :short c)) "parse -b flag")
      (ok (equal :true (clingon:parse-option :long c)) "parse --a-option flag")
      (ok (equal :true (clingon:parse-option :long c)) "parse --b-option flag")
      (ok (signals (clingon:parse-option :short c) 'clingon:unknown-option)
          "signals on first unknown -X option")
      (ok (signals (clingon:parse-option :short c) 'clingon:unknown-option)
          "signals on second unknown --invalid option")
      (clingon:finalize-command c)))

  (testing "parse options with restarts"
    (let ((c (clingon:make-command :name "foo"  ;; <- no options defined for the command
                                   :description "foo with restarts"
                                   :args-to-parse '("-a" "--long" "-b"))))
      (clingon:initialize-command c)
      (handler-bind ((clingon:unknown-option #'clingon:treat-as-argument))
        (clingon:parse-option :short c)
        (clingon:parse-option :long c)
        (clingon:parse-option :short c))
      (clingon:finalize-command c)
      (ok (equal '("-a" "--long" "-b") (clingon:command-arguments c))
          "treat unknowns as free arguments")

      ;; Re-initialize the command with different input
      (setf (clingon:command-args-to-parse c) '("-a" "--long" "-b"))
      (clingon:initialize-command c)
      (handler-bind ((clingon:unknown-option #'clingon:discard-option))
        (clingon:parse-option :short c)
        (clingon:parse-option :long c)
        (clingon:parse-option :short c))
      (clingon:finalize-command c)
      (ok (equal nil (clingon:command-arguments c)) "discard unknown options")))

  (testing "parse required options"
    (let ((c (clingon:make-command :name "foo"
                                   :description "foo command"
                                   :options
                                   (list
                                    (clingon:make-option :string
                                                         :short-name #\s
                                                         :long-name "string"
                                                         :description "required options"
                                                         :required t
                                                         :key :string)))))
      ;; Test short option parsing
      (setf (clingon:command-args-to-parse c) '("-s" "foo"))
      (clingon:initialize-command c)
      (clingon:parse-option :short c)
      (clingon:finalize-command c)
      (ok (string= "foo" (clingon:getopt c :string)) "short option value matches")

      ;; Test long option parsing
      (setf (clingon:command-args-to-parse c) '("--string" "bar"))
      (clingon:initialize-command c)
      (clingon:parse-option :long c)
      (clingon:finalize-command c)
      (ok (string= "bar" (clingon:getopt c :string)) "long option value matches")

      ;; Test with missing optarg for short option
      (setf (clingon:command-args-to-parse c) '("-s"))
      (clingon:initialize-command c)
      (ok (signals (clingon:parse-option :short c) 'clingon:missing-option-argument)
          "signals on missing option argument (short option)")
      (clingon:finalize-command c)

      ;; Test with missing optarg for long option
      (setf (clingon:command-args-to-parse c) '("--string"))
      (clingon:initialize-command c)
      (ok (signals (clingon:parse-option :long c) 'clingon:missing-option-argument)
          "signals on missing option argument (long option) #1")
      (clingon:finalize-command c)

      ;; Test with missing optarg for long option
      (setf (clingon:command-args-to-parse c) '("--string="))
      (clingon:initialize-command c)
      (ok (signals (clingon:parse-option :long c) 'clingon:missing-option-argument)
          "signals on missing option argument (long option) #2")
      (clingon:finalize-command c))))

(defun status/options ()
  "Creates the options for the sample `status' command"
  (list
   (clingon:make-option :boolean
			:description "real-time updates"
			:short-name #\r
			:long-name "real-time"
			:required t
			:key :real-time)))

(defun status/command ()
  "Creates the sample `status' command"
  (clingon:make-command :name "status"
			:description "status information"
			:options (status/options)))

(defun display/options ()
  "Returns the options for the sample `display' sub-command"
  ;; An option that acts as a switch
  (list
   (clingon:make-option :boolean/true
			:description "enable progress reporting"
			:short-name #\p
			:long-name "progress"
			:key :progress)
   (clingon:make-option :boolean/false
			:description "disable progress reporting"
			:short-name #\P
			:long-name "no-progress"
			:key :progress)))

(defun display/command ()
  "Creates a sample `display' command"
  (clingon:make-command :name "display"
			:description "displays something funny"
			:options (display/options)
			:sub-commands (list (status/command))))

(defun top-level/options ()
  "Returns the top-level command options"
  (list
   (clingon:make-option :counter
			:description "how noisy we want to be"
			:short-name #\v
			:long-name "verbose"
			:key :verbose)))

(defun top-level/command ()
  "Our sample top-level command.
   The command we build here has the following usage spec:

  $ top-level [-v] [display [--no|progress] [status --real-time=[false|true]]]"
  (clingon:make-command :name "top-level"
			:description "top-level command"
			:options (top-level/options)
			:sub-commands (list (display/command))))

(deftest parse-command-line
  (testing "test with no arguments"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level nil)))
      (ok (string= "top-level" (clingon:command-name c)) "matches the top-level command")
      (ok (= 0 (clingon:getopt c :verbose)) "verbose is 0")))

  (testing "top-level command with global flag"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level '("-vvv" "--verbose"))))
      (ok (string= "top-level" (clingon:command-name c)) "matches the top-level command")
      (ok (= 4 (clingon:getopt c :verbose)) "verbose is 4")))

  (testing "top-level command with free arguments"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level '("-v" "foo" "bar" "baz"))))
      (ok (string= "top-level" (clingon:command-name c)) "matches the top-level command")
      (ok (= 1 (clingon:getopt c :verbose)) "verbose is 4")
      (ok (equal '("foo" "bar" "baz") (clingon:command-arguments c)) "free arguments match")))

  (testing "top-level command with free args matching sub-command name"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level '("-v" "--" "display" "status" "-a" "-b" "-c"))))
      (ok (string= "top-level" (clingon:command-name c)) "matches the top-level command")
      (ok (= 1 (clingon:getopt c :verbose)) "verbose is 4")
      (ok (equal '("display" "status" "-a" "-b" "-c") (clingon:command-arguments c)) "free arguments match")))

  (testing "first level sub-command"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level '("-vvv" "display" "--progress" "a" "b" "c"))))
      (ok (string= "display" (clingon:command-name c)) "matches first sub-command name")
      (ok (equal '("top-level" "display") (clingon:command-full-path c)) "command full path matches")
      (ok (= 3 (clingon:getopt c :verbose)) "global verbose option is 3")
      (ok (equal t (clingon:getopt c :progress)) "flag --progress is set")
      (ok (equal '("a" "b" "c") (clingon:command-arguments c)) "free arguments match")))

  (testing "first level sub-command option being switched on/off"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level '("display" "--progress" "--no-progress"))))
      (ok (string= "display" (clingon:command-name c)) "matches first sub-command name")
      (ok (equal '("top-level" "display") (clingon:command-full-path c)) "command full path matches")
      (ok (equal t (clingon:opt-is-set-p c :progress)) "the --progress|--no-progress flag was set")
      (ok (equal nil (clingon:getopt c :progress)) "flag --progress is set to nil")))

  (testing "second level sub-command with missing required option"
    (let ((top-level (top-level/command)))
      (ok (signals (clingon:parse-command-line top-level '("display" "status")) 'clingon:missing-required-option-value)
	  "signals on missing required option value")))

  (testing "second level sub-command with option set"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level
					  '("-vvv" "display" "--progress" "status" "--real-time=false"))))
      (ok (string= "status" (clingon:command-name c)) "matches second sub-command name")
      (ok (equal '("top-level" "display" "status") (clingon:command-full-path c)) "command full path matches")
      (ok (= 3 (clingon:getopt c :verbose)) "global verbose flag is 3")
      (ok (equal t (clingon:opt-is-set-p c :progress)) "flag --progress is set")
      (ok (equal t (clingon:getopt c :progress)) "flag --progress is set to t")
      (ok (equal t (clingon:opt-is-set-p c :real-time)) "flag --real-time is set")
      (ok (equal nil (clingon:getopt c :real-time)) "flag --real-time value is nil")))

  (testing "second level sub-command with free arguments"
    (let* ((top-level (top-level/command))
	   (c (clingon:parse-command-line top-level
					  '("display" "status" "--real-time=true" "a" "b" "c"))))
      (ok (string= "status" (clingon:command-name c)) "matches second sub-command name")
      (ok (equal '("top-level" "display" "status") (clingon:command-full-path c)) "command full path matches")
      (ok (= 0 (clingon:getopt c :verbose)) "global verbose flag is 0")
      (ok (equal nil (clingon:opt-is-set-p c :progress)) "flag --progress is not set")
      (ok (equal t (clingon:opt-is-set-p c :real-time)) "flag --real-time is set")
      (ok (equal t (clingon:getopt c :real-time)) "flag --real-time value is t")
      (ok (equal '("a" "b" "c") (clingon:command-arguments c)) "free arguments match"))))
