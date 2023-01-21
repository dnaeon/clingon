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

(in-package :clingon.test)

(deftest generic-options
  (testing "invalid options"
    (ok (signals (clingon:make-option :generic
                                      :description "foo"
                                      :short-name #\f
                                      :initial-value "bar"))
        "Signals on missing option key")
    (ok (signals (clingon:make-option :generic
                                      :description "foo"
                                      :key :foo)
            'clingon:invalid-option)
        "Signals on missing short and long options")
    (ok (signals (clingon:make-option :generic
                                      :short-name #\f
                                      :required t
                                      :description "foo"
                                      :key :foo)
            'clingon:invalid-option)
        "Signals on required option with missing parameter")
    (ok (signals (clingon:make-option :generic
                                      :parameter "FOO"
                                      :initial-value "some-value"
                                      :short-name #\f
                                      :required t
                                      :description "foo"
                                      :key :foo)
            'clingon:invalid-option)
        "Signals on required option with default value")
    (ok (signals (clingon:make-option :generic
                                      :description "foo"
                                      :short-name #\f
                                      :key "invalid-key")
            'clingon:invalid-option)
        "Signals when option key is not a keyword"))

  (testing "initialize, derive and finalize"
    (let ((foo (clingon:make-option :generic
                                    :description "foo"
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
                                    :description "foo boolean"
                                    :short-name #\b
                                    :key :boolean)))
      (clingon:initialize-option foo)
      (ok (equal nil (clingon:option-value foo)) "option is properly initialized")
      (ok (equal :true (clingon:derive-option-value foo "true")) "derive value from \"true\"")
      (ok (equal :true (clingon:derive-option-value foo "1")) "derive value from \"1\"")
      (ok (equal :false (clingon:derive-option-value foo "false")) "derive value from \"false\"")
      (ok (equal :false (clingon:derive-option-value foo "0")) "derive value from \"0\"")
      (ok (equal :false (clingon:derive-option-value foo nil)) "derive value from nil")
      (ok (equal :false (clingon:derive-option-value foo "random-string")) "derive value from \"random-string\"")

      ;; Set option and test finalized value
      (setf (clingon:option-value foo)
            (clingon:derive-option-value foo "false"))
      (ok (equal nil (clingon:finalize-option foo)) "finalized value matches")))

  (testing "boolean-always-true"
    (let ((foo (clingon:make-option :boolean/true
                                    :description "always true option"
                                    :short-name #\b
                                    :key :boolean)))
      (clingon:initialize-option foo)
      (ok (equal nil (clingon:option-value foo)) "option is properly initialized")
      (ok (equal :true (clingon:derive-option-value foo "true")) "derive value from \"true\"")
      (ok (equal :true (clingon:derive-option-value foo "1")) "derive value from \"1\"")
      (ok (equal :true (clingon:derive-option-value foo "false")) "derive value from \"false\"")
      (ok (equal :true (clingon:derive-option-value foo "0")) "derive value from \"0\"")
      (ok (equal :true (clingon:derive-option-value foo nil)) "derive value from nil")
      (ok (equal :true (clingon:derive-option-value foo "random-string")) "derive value from \"random-string\"")

      ;; Set option and test finalized value
      (setf (clingon:option-value foo)
            (clingon:derive-option-value foo nil))
      (ok (equal t (clingon:finalize-option foo)) "finalized value matches")))

  (testing "boolean-always-false"
    (let ((foo (clingon:make-option :boolean/false
                                    :description "always false option"
                                    :short-name #\b
                                    :key :boolean)))
      (clingon:initialize-option foo)
      (ok (equal nil (clingon:option-value foo)) "option is properly initialized")
      (ok (equal :false (clingon:derive-option-value foo "true")) "derive value from \"true\"")
      (ok (equal :false (clingon:derive-option-value foo "1")) "derive value from \"1\"")
      (ok (equal :false (clingon:derive-option-value foo "false")) "derive value from \"false\"")
      (ok (equal :false (clingon:derive-option-value foo "0")) "derive value from \"0\"")
      (ok (equal :false (clingon:derive-option-value foo nil)) "derive value from nil")
      (ok (equal :false (clingon:derive-option-value foo "random-string")) "derive value from \"random-string\"")

      ;; Set option and test finalized value
      (setf (clingon:option-value foo)
            (clingon:derive-option-value foo "true"))
      (ok (equal nil (clingon:finalize-option foo)) "finalized value matches"))))

(deftest option-counter
  (testing "counter with defaults"
    (let ((opt (clingon:make-option :counter
                                    :description "counter with defaults"
                                    :short-name #\c
                                    :key :counter)))
      (clingon:initialize-option opt)
      (loop :repeat 42 :do
        (setf (clingon:option-value opt)
              (clingon:derive-option-value opt nil)))
      (ok (= 42 (clingon:finalize-option opt)) "finalized value matches")))

  (testing "counter with a step"
    (let ((opt (clingon:make-option :counter
                                    :description "counter with a step"
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
                                    :description "list with defaults"
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
                                    :description "list with defaults"
                                    :short-name #\l
                                    :key :list
                                    :initial-value "foo, bar, baz")))
      (clingon:initialize-option opt)
      (ok (equal (list "foo" "bar" "baz") (clingon:finalize-option opt)) "finalized value matches")))

  (testing "list with initial list value"
    (let ((opt (clingon:make-option :list
                                    :description "list initialized from a list"
                                    :short-name #\l
                                    :key :list
                                    :initial-value '("foo" "bar" "baz"))))
      (clingon:initialize-option opt)
      (setf (clingon:option-value opt) (clingon:derive-option-value opt "qux"))
      (ok (equal (list "foo" "bar" "baz" "qux") (clingon:finalize-option opt)) "finalized value matches"))))

(deftest option-integer
  (testing "integer with defaults"
    (let ((opt (clingon:make-option :integer
                                    :description "int with defaults"
                                    :short-name #\i
                                    :key :int
                                    :initial-value 0)))
      (clingon:initialize-option opt)
      (ok (= 0 (clingon:option-value opt)) "initial value matches")
      (ok (= 42 (clingon:derive-option-value opt "42")) "derive 42 as int")
      (ok (= 42 (clingon:derive-option-value opt "42.0")) "derive 42.0 as int")
      (ok (= 42 (clingon:derive-option-value opt "42.42")) "derive 42.42 as int")
      (ok (signals (clingon:derive-option-value opt "NaN")) "signals on NaN")

      ;; The value's place has not been set at all
      (ok (= 0 (clingon:finalize-option opt)) "finalized value matches")))

  (testing "integer initialized from string with good input"
    ;; The string initial-value would usually come from an env var
    (let ((opt (clingon:make-option :integer
                                    :description "int with default string value"
                                    :short-name #\i
                                    :key :int
                                    :initial-value "42")))
      (clingon:initialize-option opt)
      (ok (= 42 (clingon:finalize-option opt)) "finalized value matches")))

  (testing "integer initialized with bad input"
    (let ((opt (clingon:make-option :integer
                                    :description "int with bad default string value"
                                    :short-name #\i
                                    :key :int
                                    :initial-value "NaN")))
      (ok (signals (clingon:initialize-option opt)) "signals on invalid initialization"))))

(deftest option-list-integer
  (testing "derive values from integers"
    (let ((opt (clingon:make-option :list/integer
                                    :description "list of integers"
                                    :short-name #\l
                                    :key :list-of-integers)))
      (clingon:initialize-option opt)
      (loop :for i :from 0 :to 5 :do
        (setf (clingon:option-value opt)
              (clingon:derive-option-value opt i)))
      (ok (equal '(0 1 2 3 4 5) (clingon:finalize-option opt)) "finalized value matches")))

  (testing "derive values from strings"
    (let ((opt (clingon:make-option :list/integer
                                    :description "list of integers"
                                    :short-name #\l
                                    :key :list-of-integers)))
      (clingon:initialize-option opt)
      (loop :for i :from 0 :to 5 :do
        (setf (clingon:option-value opt)
              (clingon:derive-option-value opt (format nil "~d" i))))
      (ok (equal '(0 1 2 3 4 5) (clingon:finalize-option opt)) "finalized value matches")))

  (testing "derive values with a default value as a list"
    (let ((opt (clingon:make-option :list/integer
                                    :description "list of integers"
                                    :short-name #\l
                                    :key :list-of-integers
                                    :initial-value '(-3 -2 -1))))
      (clingon:initialize-option opt)
      (loop :for i :from 0 :to 5 :do
        (setf (clingon:option-value opt)
              (clingon:derive-option-value opt (format nil "~d" i))))
      (ok (equal '(-3 -2 -1 0 1 2 3 4 5) (clingon:finalize-option opt)) "finalized value matches")))

  (testing "derive values with a default value as a string"
    (let ((opt (clingon:make-option :list/integer
                                    :description "list of integers"
                                    :short-name #\l
                                    :key :list-of-integers
                                    :initial-value "-3, -2, -1")))
      (clingon:initialize-option opt)
      (loop :for i :from 0 :to 5 :do
        (setf (clingon:option-value opt)
              (clingon:derive-option-value opt (format nil "~d" i))))
      (ok (equal '(-3 -2 -1 0 1 2 3 4 5) (clingon:finalize-option opt)) "finalized value matches"))))

(deftest option-choice
  (testing "test with no default choice set"
    (let ((opt (clingon:make-option :choice
                                    :description "choice option"
                                    :short-name #\c
                                    :key :choice
                                    :items '("foo" "bar" "baz"))))
      (clingon:initialize-option opt)
      (ok (string= "foo" (clingon:derive-option-value opt "foo")) "derive foo choice")
      (ok (string= "bar" (clingon:derive-option-value opt "bar")) "derive bar choice")
      (ok (string= "baz" (clingon:derive-option-value opt "baz")) "derive baz choice")
      (ok (signals (clingon:derive-option-value opt "INVALID")
              'clingon:option-derive-error)
          "signals on invalid choice")))

  (testing "test with a default choice"
    (let ((opt (clingon:make-option :choice
                                    :description "choice option with default value"
                                    :short-name #\c
                                    :key :choice
                                    :items '("foo" "bar" "baz")
                                    :initial-value "foo")))
      (clingon:initialize-option opt)
      (ok (string= "foo" (clingon:finalize-option opt)) "finalized value matches")))

  (testing "test with invalid default choice"
    (let ((opt (clingon:make-option :choice
                                    :description "choice option with invalid default value"
                                    :short-name #\c
                                    :key :choice
                                    :items '("foo" "bar" "baz")
                                    :initial-value "INVALID")))
      (ok (signals (clingon:initialize-option opt) 'clingon:option-derive-error) "signals on invalid default choice"))))

(deftest option-enum
  (testing "test with no default value"
    (let ((opt (clingon:make-option :enum
                                    :description "enum option"
                                    :short-name #\e
                                    :key :enum
                                    :items '(("one" . 1)
                                             ("two" . 2)
                                             ("three" . 3)))))
      (clingon:initialize-option opt)
      (ok (= 1 (clingon:derive-option-value opt "one")) "derive value from \"one\"")
      (ok (= 2 (clingon:derive-option-value opt "two")) "derive value from \"two\"")
      (ok (= 3 (clingon:derive-option-value opt "three")) "derive value from \"three\"")
      (ok (signals (clingon:derive-option-value opt "INVALID") 'clingon:option-derive-error)
          "signals on invalid enum variant")))

  (testing "test with default value"
    (let ((opt (clingon:make-option :enum
                                    :description "enum option"
                                    :short-name #\e
                                    :key :enum
                                    :items '(("one" . 1)
                                             ("two" . 2)
                                             ("three" . 3))
                                    :initial-value "one")))
      (clingon:initialize-option opt)
      (ok (= 1 (clingon:finalize-option opt)) "finalized value matches")))

  (testing "test with invalid default value"
    (let ((opt (clingon:make-option :enum
                                    :description "enum option"
                                    :short-name #\e
                                    :key :enum
                                    :items '(("one" . 1)
                                             ("two" . 2)
                                             ("three" . 3))
                                    :initial-value "INVALID")))
      (ok (signals (clingon:initialize-option opt) 'clingon:option-derive-error)
          "signals on invalid default value"))))

(deftest option-string
  (testing "test with no default"
    (let ((opt (clingon:make-option :string
                                    :description "string option"
                                    :short-name #\c
                                    :key :string)))
      (clingon:initialize-option opt)
      (ok (string= "foo" (clingon:derive-option-value opt "foo")) "derive foo")
      (ok (string= "bar" (clingon:derive-option-value opt "bar")) "derive bar")
      (ok (string= "baz" (clingon:derive-option-value opt "baz")) "derive baz")))

  (testing "test with a default value"
    (let ((opt (clingon:make-option :string
                                    :description "string with default value"
                                    :short-name #\c
                                    :key :string
                                    :initial-value "foo")))
      (clingon:initialize-option opt)
      (ok (string= "foo" (clingon:finalize-option opt)) "finalized value matches"))))

(deftest option-switch
  (testing "test with no default value"
    (let ((opt (clingon:make-option :switch
                                    :description "switch option"
                                    :short-name #\s
                                    :key :switch)))
      (clingon:initialize-option opt)
      (ok (equal :true (clingon:derive-option-value opt "on")) "derive value from \"on\"")
      (ok (equal :true (clingon:derive-option-value opt "yes")) "derive value from \"yes\"")
      (ok (equal :true (clingon:derive-option-value opt "enable")) "derive value from \"enable\"")
      (ok (equal :true (clingon:derive-option-value opt "1")) "derive value from \"1\"")
      (ok (equal :true (clingon:derive-option-value opt "true")) "derive value from \"true\"")
      (ok (equal :false (clingon:derive-option-value opt "off")) "derive value from \"off\"")
      (ok (equal :false (clingon:derive-option-value opt "no")) "derive value from \"no\"")
      (ok (equal :false (clingon:derive-option-value opt "false")) "derive value from \"false\"")
      (ok (equal :false (clingon:derive-option-value opt "disable")) "derive value from \"disable\"")
      (ok (equal :false (clingon:derive-option-value opt "0")) "derive value from \"0\"")
      (ok (signals (clingon:derive-option-value opt "INVALID") 'clingon:option-derive-error)
          "signals on invalid switch state")))

  (testing "test with a default value"
    (let ((opt (clingon:make-option :switch
                                    :description "switch with a default state"
                                    :short-name #\s
                                    :key :switch
                                    :initial-value "on")))
      (clingon:initialize-option opt)
      (ok (equal t (clingon:finalize-option opt)) "finalized value matches")))

  (testing "test with invalid default value"
    (let ((opt (clingon:make-option :switch
                                    :description "switch with invalid default"
                                    :short-name #\s
                                    :key :switch
                                    :initial-value "INVALID")))
      (ok (signals (clingon:initialize-option opt) 'clingon:option-derive-error)
          "signals on invalid default value"))))
