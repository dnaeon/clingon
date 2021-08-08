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
      (ok (string= (clingon:derive-option-value foo "qux") "qux") "derive qux value"))))

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
    (ng (clingon:long-option-p "-abc") "-abc does not match a long option"))

  (testing "end-of-options-p"
    (ng (clingon:end-of-options-p "-") "`-' does not match end of options")
    (ok (clingon:end-of-options-p "--") "`--' matches end of options")
    (ng (clingon:end-of-options-p "---") "`---' does not match end of options")))
