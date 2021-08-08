(in-package :clingon.test)

(deftest generic-options
  (testing "invalid options"
    (ok (signals (clingon:make-option :generic
                                      :help "foo"
                                      :short-name #\f
                                      :initial-value "bar"))
        "Signals on missing option key")
    (ok (signals (clingon:make-option :generic
                                      :help "foo"
                                      :key :foo)
            'clingon:invalid-option)
        "Signals on missing short and long options")
    (ok (signals (clingon:make-option :generic
                                      :short-name #\f
                                      :required t
                                      :help "foo"
                                      :key :foo)
            'clingon:invalid-option)
        "Signals on required option with missing parameter")
    (ok (signals (clingon:make-option :generic
                                      :parameter "FOO"
                                      :initial-value "some-value"
                                      :short-name #\f
                                      :required t
                                      :help "foo"
                                      :key :foo)
            'clingon:invalid-option)
        "Signals on required option with default value")
    (ok (signals (clingon:make-option :generic
                                      :help "foo"
                                      :short-name #\f
                                      :key "invalid-key")
            'clingon:invalid-option)
        "Signals when option key is not a keyword"))

  (testing "initialize, derive and finalize"
    (let ((foo (clingon:make-option :generic
                                    :help "foo"
                                    :short-name #\f
                                    :key :foo
                                    :initial-value "bar")))
      (ok (equal (clingon:option-value foo) nil) "value of foo is nil")

      ;; Initialize the option and derive a few sample values.  The
      ;; default implementation of DERIVE-OPTION-VALUE returns the
      ;; value itself.
      (clingon:initialize-option foo)
      (ok (string= (clingon:option-value foo) "bar") "option foo is properly initialized")
      (ok (string= (clingon:derive-option-value foo "baz") "baz") "derive baz value")
      (ok (string= (clingon:derive-option-value foo "qux") "qux") "derive qux value")

      ;; The finalized value of the option will still be "bar", since
      ;; we haven't set that place yet. It is usually set by the
      ;; parser.
      (ok (string= (clingon:finalize-option foo) "bar") "finalized value matches"))))

(deftest option-predicates
  (testing "short-options"
    (ok (clingon:short-option-p "-x") "-x matches a short option")
    (ok (clingon:short-option-p "-abc") "-abc matches a short option")
    (ng (clingon:short-option-p "--long") "--long does not match a short option")
    (ng (clingon:short-option-p "-") "`-' does not match a short option"))

  (testing "long-options"
    (ok (clingon:long-option-p "--foo") "--foo matches a long option")
    (ok (clingon:long-option-p "--foo=bar") "--foo=bar matches a long option")
    (ng (clingon:long-option-p "-x") "-x does not match a long option")
    (ng (clingon:long-option-p "--") "`--' does not match a long option")
    (ng (clingon:long-option-p "-abc") "-abc does not match a long option"))

  (testing "end-of-options-p"
    (ng (clingon:end-of-options-p "-") "`-' does not match end of options")
    (ok (clingon:end-of-options-p "--") "`--' matches end of options")
    (ng (clingon:end-of-options-p "---") "`---' does not match end of options")))

(deftest option-booleans
  (testing "generic boolean"
    (let ((foo (clingon:make-option :boolean
                                    :help "foo boolean"
                                    :short-name #\b
                                    :key :boolean)))
      (clingon:initialize-option foo)
      (ok (equal :true (clingon:option-value foo)) "option is properly initialized")
      (ok (equal :true (clingon:derive-option-value foo "true")) "derive value from \"true\"")
      (ok (equal :true (clingon:derive-option-value foo "1")) "derive value from \"1\"")
      (ok (equal :false (clingon:derive-option-value foo "false")) "derive value from \"false\"")
      (ok (equal :false (clingon:derive-option-value foo "0")) "derive value from \"0\"")
      (ok (equal :false (clingon:derive-option-value foo nil)) "derive value from nil")
      (ok (equal :false (clingon:derive-option-value foo "random-string")) "derive value from \"random-string\"")

      ;; value place has not been set, so it defaults to :true
      (ok (equal t (clingon:finalize-option foo)) "finalized value matches")))

  (testing "boolean-always-true"
    (let ((foo (clingon:make-option :boolean/true
                                    :help "always true option"
                                    :short-name #\b
                                    :key :boolean)))
      (clingon:initialize-option foo)
      (ok (equal :true (clingon:option-value foo)) "option is properly initialized")
      (ok (equal :true (clingon:derive-option-value foo "true")) "derive value from \"true\"")
      (ok (equal :true (clingon:derive-option-value foo "1")) "derive value from \"1\"")
      (ok (equal :true (clingon:derive-option-value foo "false")) "derive value from \"false\"")
      (ok (equal :true (clingon:derive-option-value foo "0")) "derive value from \"0\"")
      (ok (equal :true (clingon:derive-option-value foo nil)) "derive value from nil")
      (ok (equal :true (clingon:derive-option-value foo "random-string")) "derive value from \"random-string\"")
      (ok (equal t (clingon:finalize-option foo)) "finalized value matches")))

  (testing "boolean-always-false"
    (let ((foo (clingon:make-option :boolean/false
                                    :help "always false option"
                                    :short-name #\b
                                    :key :boolean)))
      (clingon:initialize-option foo)
      (ok (equal :false (clingon:option-value foo)) "option is properly initialized")
      (ok (equal :false (clingon:derive-option-value foo "true")) "derive value from \"true\"")
      (ok (equal :false (clingon:derive-option-value foo "1")) "derive value from \"1\"")
      (ok (equal :false (clingon:derive-option-value foo "false")) "derive value from \"false\"")
      (ok (equal :false (clingon:derive-option-value foo "0")) "derive value from \"0\"")
      (ok (equal :false (clingon:derive-option-value foo nil)) "derive value from nil")
      (ok (equal :false (clingon:derive-option-value foo "random-string")) "derive value from \"random-string\"")
      (ok (equal nil (clingon:finalize-option foo)) "finalized value matches"))))

(deftest option-counter
  (testing "counter with defaults"
    (let ((opt (clingon:make-option :counter
				    :help "counter with defaults"
				    :short-name #\c
				    :key :counter)))
      (clingon:initialize-option opt)
      (loop :repeat 42 :do
	(setf (clingon:option-value opt)
	      (clingon:derive-option-value opt nil)))
      (ok (= 42 (clingon:finalize-option opt)) "finalized value matches")))

  (testing "counter with a step"
    (let ((opt (clingon:make-option :counter
				    :help "counter with a step"
				    :short-name #\c
				    :key :counter
				    :initial-value 42
				    :step 3)))
      (clingon:initialize-option opt)
      (loop :repeat 3 :do
	(setf (clingon:option-value opt)
	      (clingon:derive-option-value opt nil)))
      (ok (= 51 (clingon:finalize-option opt)) "finalized value matches"))))

(deftest option-list
  (testing "list with defaults"
    (let ((opt (clingon:make-option :list
				    :help "list with defaults"
				    :short-name #\l
				    :key :list))
	  (items (list "foo" "bar" "baz")))
      (clingon:initialize-option opt)
      (loop :for item :in items :do
	(setf (clingon:option-value opt)
	      (clingon:derive-option-value opt item)))
      (ok (equal items (clingon:finalize-option opt)) "finalized value matches")))

  (testing "list with initial string value"
    ;; The string value for a list would usually be provided from
    ;; environment variables.
    (let ((opt (clingon:make-option :list
				    :help "list with defaults"
				    :short-name #\l
				    :key :list
				    :initial-value "foo, bar, baz")))
      (clingon:initialize-option opt)
      (ok (equal (list "foo" "bar" "baz") (clingon:finalize-option opt)) "finalized value matches"))))
