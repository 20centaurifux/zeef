(ns zeef.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [zeef.core :as zf]
            [zeef.schema :as zs]
            [zeef.test-generators :refer [FieldSameTypeTupleGenerator]]))

;;;; generative test runner

(def ^:private num-tests 20)

(defn- test-fn
  "Performs generative testing on function `f`.
  
   The function generates test data according to `g` and applies it to `f`.
   Then it verifies that `pred` returns true when applied to `f`'s result.
   Runs `num-tests` test cases and fails if any test case fails."
  [pred f g]
  (let [prop (prop/for-all [args g]
               (pred (apply f args)))
        result (tc/quick-check num-tests prop)]
    (is (:pass? result)
        (str "Test failed with arguments: "
             (pr-str (-> result :shrunk :smallest))))))

;;;; test creator functions

(deftest test-field
  (testing "Valid input: Returns a field when the input is a keyword."
    (test-fn zs/field? zf/field (gen/tuple gen/keyword)))

  (testing "Invalid input: Returns nil when the input is not a keyword."
    (test-fn nil?
             zf/field
             (gen/tuple (gen/such-that (complement keyword?) gen/any)))))

;;;; test conditions

;;; nil?

(deftest test-z-nil?
  (testing "Valid input: Returns a unary condition when the input is a field."
    (test-fn zs/unary-condition? zf/z-nil? (gen/tuple zs/FieldGenerator)))

  (testing "Invalid input: Returns nil when the input is not a field."
    (test-fn nil? zf/z-nil? (gen/tuple gen/any))))

;;; compare functions

(deftest test-compare-functions
  (letfn [(test [f]
            (testing "Valid input: Returns a binary comparison condition when both
                      inputs are values of the same type or fields."
              (test-fn zs/binary-condition? f (FieldSameTypeTupleGenerator 2)))

            (testing "Invalid input: Returns nil when inputs have different
                      types."
              (test-fn nil?
                       f
                       (gen/tuple (gen/one-of [gen/string
                                               gen/small-integer
                                               gen/boolean])
                                  (gen/one-of [gen/double
                                               gen/uuid
                                               gen/keyword]))))

            (testing "Invalid input: Returns nil when at least one input is not
                      a value."
              (test-fn nil?
                       f
                       (gen/let [a zs/ValueGenerator
                                 b (gen/one-of [zs/CollectionGenerator
                                                zs/LogicalOperationGenerator])]
                         (shuffle [a b])))))]
    (testing "Testing z-<"
      (test zf/z-<))

    (testing "Testing z-<="
      (test zf/z-<=))

    (testing "Testing z-="
      (test zf/z-=))

    (testing "Testing z->"
      (test zf/z->))

    (testing "Testing z->="
      (test zf/z->=))))

;;; string functions

(deftest test-string-functions
  (letfn [(test [f]
            (testing "Valid input: Returns a binary string condition when both
                      inputs are strings or fields."
              (test-fn zs/binary-string-condition?
                       f
                       (FieldSameTypeTupleGenerator 2 [gen/string])))

            (testing "Invalid input: Returns nil when at least one input is
                      neither a field nor a string."
              (test-fn nil?
                       f
                       (gen/such-that (fn [[a b]]
                                        (or (not (or (string? a) (zs/field? a)))
                                            (not (or (string? b) (zs/field? b)))))
                                      (gen/vector (gen/one-of [zs/FieldGenerator
                                                               zs/ValueGenerator
                                                               zs/CollectionGenerator
                                                               zs/LogicalOperationGenerator])
                                                  2)))))]
    (testing "Testing z-starts-with?"
      (test zf/z-starts-with?))

    (testing "Testing z-ends-with?"
      (test zf/z-ends-with?))

    (testing "Testing z-includes?"
      (test zf/z-includes?))))

;;; collection functions

(deftest test-collection-functions
  (testing "Valid input: Returns a binary collection condition when the first
            input is a field and the second is a collection."
    (test-fn zs/binary-collection-condition?
             zf/z-in?
             (gen/tuple zs/FieldGenerator zs/CollectionGenerator)))

  (testing "Invalid input: Returns nil when the first input is not a field."
    (test-fn nil?
             zf/z-in?
             (gen/tuple gen/any zs/CollectionGenerator)))

  (testing "Invalid input: Returns nil when the second input is not a
            collection."
    (test-fn nil?
             zf/z-in?
             (gen/tuple zs/FieldGenerator
                        (gen/such-that (complement zs/collection?) gen/any)))))

;;; range functions

(deftest test-range-functions
  (testing "Valid input: Returns a ternary range condition when all three inputs
            are values of the same type or fields."
    (test-fn zs/ternary-range-condition?
             zf/z-between?
             (FieldSameTypeTupleGenerator 3)))

  (testing "Invalid input: Returns nil when at least one input is neither a
            field nor a value."
    (test-fn nil?
             zf/z-between?
             (gen/let [values (FieldSameTypeTupleGenerator 2)
                       other (gen/one-of [zs/CollectionGenerator
                                          zs/LogicalOperationGenerator])]
               (shuffle (cons other values))))))

;;;; test operations

;;; unary functions

(deftest test-unary-logical-functions
  (testing "Valid input: Returns a unary logical operation when the input is a
            field or a logical operation."
    (test-fn zs/unary-logical-operation?
             zf/z-not
             (gen/tuple (gen/one-of [zs/FieldGenerator
                                     zs/LogicalOperationGenerator]))))

  (testing "Invalid input: Returns nil when the input is neither a field nor
            a logical operation."
    (test-fn nil?
             zf/z-not
             (gen/tuple gen/any))))

;;; n-ary functions

(deftest test-nary-logical-operations
  (letfn [(test [f]
            (testing "Valid input: Returns an n-ary logical operation when all
                      inputs are logical operations."
              (test-fn zs/nary-logical-operation?
                       f
                       (gen/vector zs/LogicalOperationGenerator 1 5)))

            (testing "Invalid input: Returns nil when at least one argument is
                      not a logical operation."
              (test-fn nil?
                       f
                       (gen/let [operations (gen/vector zs/LogicalOperationGenerator 0 3)
                                 v gen/any]
                         (shuffle (cons v operations))))))]
    (testing "Testing z-and"
      (test zf/z-and))

    (testing "Testing z-or"
      (test zf/z-or))))

;;;; nested queries

(deftest test-nested-queries
  (letfn [(test [f]
            (testing "Valid input: Returns a nested query when the first input
                      is a field and the second is a logical operation."
              (test-fn zs/nested-query?
                       f
                       (gen/tuple zs/FieldGenerator zs/LogicalOperationGenerator)))

            (testing "Invalid input: Returns nil when the first input is not a
                      field."
              (test-fn nil?
                       f
                       (gen/tuple gen/any zs/LogicalOperationGenerator)))

            (testing "Invalid input: Returns nil when the second input is not a
                      logical operation."
              (test-fn nil?
                       f
                       (gen/tuple zs/FieldGenerator gen/any))))]
    (testing "Testing z-satisfies?"
      (test zf/z-satisfies?))

    (testing "Testing z-some?"
      (test zf/z-some?))))