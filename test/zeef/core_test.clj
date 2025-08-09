(ns zeef.core-test
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :refer [difference]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [zeef.core :as zf]
            [zeef.specs]))

(alias 'zs 'zeef.specs)

;;;; generative test runner

(def ^:private num-tests 10)

(defn- test-fn
  "Performs generative testing on function `f` using clojure.spec.

   The function generates test data according to `specs` and applies it
   to `f`. Then it verifies that `pred` returns true when applied to `f`'s
   result. Runs `num-tests` test cases and fails if any test case fails."
  [pred f specs]
  (let [prop (prop/for-all* (map s/gen specs)
                            (fn [& args]
                              (pred (apply f args))))
        result (tc/quick-check num-tests prop)]
    (is (:pass? result)
        (str "Test failed with arguments: "
             (pr-str (-> result :shrunk :smallest))))))

(defn- run-test-cases
  "Runs `test-fn` across multiple test cases.
   
   Ensures each test case is properly formatted as a sequence before passing it
   to `test-fn`."
  [pred f specs]
  (doseq [args specs]
    (let [args' (cond-> args
                  (not (sequential? args)) list)]
      (test-fn pred f args'))))

(defn- test-exception
  "Tests that function `f` throws an exception of type `ex-type` when called
   with arguments generated from `gens`.
   
   Runs `num-tests` test cases and fails if any call to `f` either doesn't
   throw or throws a different exception type."
  [ex-type f gens]
  (let [prop (prop/for-all* gens
                            (fn [& args]
                              (try
                                (apply f args)
                                false
                                (catch Throwable e
                                  (instance? ex-type e)))))
        result (tc/quick-check num-tests prop)]
    (str "Test failed with arguments: "
         (pr-str (-> result :shrunk :smallest)))))

;;;; specs & generators

(s/def ::non-argument
  (s/with-gen
    (fn [v]
      (not (s/valid? (s/or :v ::zs/value
                           :f ::zs/field
                           :coll ::zs/collection
                           :expr ::zs/expression)
                     v)))
    (constantly gen/any)))

(def ^:private value-specs #{string?
                             integer?
                             boolean?
                             float?
                             inst?
                             uuid?
                             keyword?})

(def ^:private all-specs (concat value-specs
                                 [::zs/field
                                  ::zs/collection
                                  ::zs/expression
                                  ::non-argument]))

(defn- all-spec-combinations
  [n]
  (into #{} (combo/selections all-specs n)))

;;;; test creator functions

(deftest test-field
  (testing "a -> Field
            a = Keyword"
    (test-fn zf/field? zf/field [keyword?]))

  (testing "a -> Exception
            a ∈ (Values ∖ {Field}) ∪ {Collection, Expression, NonArgument}"
    (test-exception AssertionError
                    zf/field
                    [(gen/such-that (complement keyword?) gen/any)])))

;;;; test predicates

;;; field?

(def ^:private valid-field?-args [::zs/field])

(def ^:private invalid-field?-args [::zs/value
                                    ::zs/collection
                                    ::zs/expression
                                    ::non-argument])

(deftest test-field?
  (testing "a -> true
            a = Field"
    (run-test-cases true? zf/field? valid-field?-args))

  (testing "a -> false
            a ∈ Values ∪ {Collection, Expression, NonArgument}"
    (run-test-cases false? zf/field? invalid-field?-args)))

;;; condition?

(def ^:private valid-condition?-args [::zs/condition])

(def ^:private invalid-condition?-args [::zs/value
                                        ::zs/field
                                        ::zs/collection
                                        ::zs/logical-operator
                                        ::non-argument])

(deftest test-condition?
  (testing "a -> true
            a = Condition"
    (run-test-cases true? zf/condition? valid-condition?-args))

  (testing "a -> false
            a ∈ Values ∪ {Field, Collection, Expression, NonArgument}
            a = Expression ⇒ a ≠ Condition"
    (run-test-cases false? zf/condition? invalid-condition?-args)))

;;; expression?

(def ^:private valid-expression?-args [::zs/expression])

(def ^:private invalid-expression?-args [::zs/value
                                         ::zs/field
                                         ::zs/collection
                                         ::non-argument])

(deftest test-expression?
  (testing "a -> true
            a = Expression"
    (run-test-cases true? zf/expression? valid-expression?-args))

  (testing "a -> false
            a ∈ Values ∪ {Field, Collection, NonArgument}
            a = Expression ⇒ a ≠ Condition"
    (run-test-cases false? zf/expression? invalid-expression?-args)))

;;; test conditions

;;; nil?

(def ^:private valid-nil?-args [::zs/field])

(def ^:private invalid-nil?-args [::zs/value
                                  ::zs/collection
                                  ::zs/expression
                                  ::non-argument])

(deftest test-z-nil?
  (testing "a -> Condition
            a = Field"
    (run-test-cases zf/condition? zf/z-nil? valid-nil?-args))

  (testing "a -> nil
            a ∈ Values ∪ {Collection, Expression, NonArgument}"
    (run-test-cases nil? zf/z-nil? invalid-nil?-args)))

;;; compare functions

(def ^:private valid-compare-fn-args
  (into #{} (mapcat (fn [spec]
                      (combo/selections [spec ::zs/field] 2))
                    value-specs)))

(def ^:private invalid-compare-fn-args
  (difference (all-spec-combinations 2) valid-compare-fn-args))

(deftest test-compare-functions
  (letfn [(test
            [f]
            (testing "a -> b -> Condition
                      a, b ∈  Values ∪ {Field}
                      (a ∈ Values ∧ b ∈ Values) ⇒ a = b"
              (run-test-cases zf/condition? f valid-compare-fn-args))

            (testing "a -> b -> nil
                      a, b ∈ Values ∪ {Field, Collection, Expression, NonArgument}
                      (a ∈ Values ∧ b ∈ Values) ⇒ a ≠ b
                      a = Field ⇒ b ∉ Values ∪ {Field}
                      b = Field ⇒ a ∉ Values ∪ {Field}"
              (run-test-cases nil? f invalid-compare-fn-args)))]
    (testing "testing z-<"
      (test zf/z-<))

    (testing "testing z-<="
      (test zf/z-<=))

    (testing "testing z-="
      (test zf/z-=))

    (testing "testing z->"
      (test zf/z->))

    (testing "testing z->="
      (test zf/z->=))))

;;; test string functions

(def ^:private valid-string-fn-args
  (into #{} (combo/selections [string? ::zs/field] 2)))

(def ^:private invalid-string-fn-args
  (difference (all-spec-combinations 2) valid-string-fn-args))

(deftest test-string-functions
  (letfn [(test
            [f]
            (testing "a -> b -> Condition
                      a, b ∈ {String, Field}"
              (run-test-cases zf/condition? f valid-string-fn-args))

            (testing "a -> b -> nil
                      a, b ∈ Values ∪ {Field, Collection, Expression, NonArgument}
                      a ∈ {String, Field} ⇒ b ∉ {String, Field}
                      b ∈ {String, Field} ⇒ a ∉ {String, Field}"
              (run-test-cases nil? f invalid-string-fn-args)))]
    (testing "testing z-starts-with?"
      (test zf/z-starts-with?))

    (testing "testing z-ends-with?"
      (test zf/z-ends-with?))

    (testing "testing z-includes?"
      (test zf/z-includes?))))

;;; test collection functions

(def ^:private valid-collection-fn-args
  (into #{} (map (fn [spec]
                   [spec ::zs/collection])
                 (cons ::zs/field value-specs))))

(def ^:private invalid-collection-fn-args
  (difference (all-spec-combinations 2) valid-collection-fn-args))

(deftest test-collection-functions
  (testing "a -> b -> Condition
            (a ∈ Values ∪ {Field} ∧ b = Collection)"
    (run-test-cases zf/condition? zf/z-in? valid-collection-fn-args))

  (testing "a -> b -> nil
            a, b ∈ Values ∪ {Field, Collection, Expression, NonArgument}
            a ∈ Values ∪ {Field} ⇒ b ≠ Collection"
    (run-test-cases nil? zf/z-in? invalid-collection-fn-args)))

;;; test range functions

(def ^:private valid-range-fn-args
  (into #{} (mapcat (fn [spec]
                      (combo/selections [spec ::zs/field] 3))
                    value-specs)))

(def ^:private invalid-range-fn-args
  (difference (all-spec-combinations 3) valid-range-fn-args))

(deftest test-range-functions
  (testing "a -> b -> c -> Condition
            a, b, c ∈ Values ∪ {Field}
            (a ∈ Values ∧ b ∈ Values ∧ c ∈ Values) ⇒ (a = b ∧ b = c)
            (a ∈ Values ∧ b ∈ Values ∧ c = Field) ⇒ a = b
            (a ∈ Values ∧ b = Field ∧ c ∈ Values) ⇒ a = c
            (a = Field ∧ b ∈ Values ∧ c ∈ Values) ⇒ b = c"
    (run-test-cases zf/condition? zf/z-between? valid-range-fn-args))

  (testing "a -> b -> c -> nil
            a, b, c ∈ Values ∪ {Field, Collection, Expression, NonArgument}
            (a = b ∧ b = c) ⇒ (a, b, c ∉ {Values} ∧ a, b, c ∉ {Field})
            (a ∈ Values ∧ b ∈ Values ∧ c ∈ Values) ⇒ (a ≠ b ∧ b ≠ c)
            (a ∈ Values ∧ b ∈ Values ∧ c = Field) ⇒ a ≠ b
            (a ∈ Values ∧ b = Field ∧ c ∈ Values) ⇒ a ≠ c
            (a = Field ∧ b ∈ Values ∧ c ∈ Values) ⇒ b ≠ c
            (a = Field ∧ b = Field) ⇒ c ∉ Values ∪ {Field}
            (a = Field ∧ c = Field) ⇒ b ∉ Values ∪ {Field}
            (b = Field ∧ c = Field) ⇒ a ∉ Values ∪ {Field}"
    (run-test-cases nil? zf/z-between? invalid-range-fn-args)))

;;;; logical operators

;;; unary

(def ^:private valid-unary-logical-fn-args
  [::zs/expression ::zs/field])

(def ^:private invalid-unary-logical-fn-args
  (concat value-specs [::zs/collection ::non-argument]))

(deftest test-unary-logical-functions
  (testing "a -> Expression
            a ∈ {Expression, Field}"
    (run-test-cases zf/expression? zf/z-not valid-unary-logical-fn-args))

  (testing "a -> nil
            a ∈ Values ∪ {Collection, NonArgument}"
    (run-test-cases nil? zf/z-not invalid-unary-logical-fn-args)))

;;; nary

(def ^:private valid-nary-logical-fn-args
  [[::zs/expression]
   [::zs/expression ::zs/expression]
   [::zs/expression ::zs/expression ::zs/expression]])

(def ^:private invalid-nary-logical-fn-args
  (for [spec (concat value-specs [::zs/field ::zs/collection ::non-argument])
        non-expr-count [1 2]
        expr-count [0 1]]
    (concat (repeat non-expr-count spec)
            (repeat expr-count ::zs/expression))))

(deftest test-nary-logical-functions
  (letfn [(test
            [f]
            (testing "[a] -> Expression
                      a = Expression"
              (run-test-cases zf/expression? f valid-nary-logical-fn-args))

            (testing "[a, b] -> nil
                      a, b ∈ Values ∪ {Field, Collection, Expression, NonArgument}
                      a = Expression ⇒ b ≠ Expression
                      b = Expression ⇒ a ≠ Expression"
              (run-test-cases nil? f invalid-nary-logical-fn-args)))]
    (testing "testing z-and"
      (test zf/z-and))

    (testing "testing z-or"
      (test zf/z-or))))