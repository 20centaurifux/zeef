(ns zeef.schema-test
  (:require [camel-snake-kebab.core :as csk]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [malli.generator :as mg]
            [zeef.schema :as zs]))

(def ^:private num-tests 20)

(defn- test-property
  "Runs a generative test property and asserts it passes.
  
  Executes `num-tests` test cases and fails if any test case fails."
  [prop]
  (let [result (tc/quick-check num-tests prop)]
    (is (:pass? result)
        (str "Test failed with arguments: "
             (pr-str (-> result :shrunk :smallest))))))

(defmacro ^:private defschema-test
  "Defines tests for schema validation and predicate function consistency."
  [schema generator predicate]
  `(deftest ~(symbol (str "test-" (csk/->kebab-case-symbol schema)))
     (testing "Schema validation passes for generated values"
       (test-property (prop/for-all [value# ~generator]
                        (m/validate ~schema value#))))

     (testing "Predicate function matches schema validation"
       (test-property (prop/for-all [value# ~generator]
                        (~predicate value#))))))

;;;; argument type tests

(defschema-test zs/FieldSchema zs/FieldGenerator zs/field?)

(deftest test-field?
  (letfn [(test-non-field [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/field? expr)))))

          (test-field [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/field? expr))))]
    (testing "Values are not fields"
      (test-non-field zs/ValueSchema))

    (testing "Collections are not fields"
      (test-non-field zs/CollectionSchema))

    (testing "Conditions are not fields"
      (test-non-field zs/ConditionSchema))

    (testing "Logical operations are not fields"
      (test-non-field zs/LogicalOperationSchema))

    (testing "Nested queries are not fields"
      (test-non-field zs/NestedQuerySchema))

    (testing "Fields are fields"
      (test-field zs/FieldSchema))))

(defschema-test zs/ValueSchema zs/ValueGenerator zs/value?)

(deftest test-value?
  (letfn [(test-non-value [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/value? expr)))))

          (test-value [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/value? expr))))]
    (testing "Fields are not values"
      (test-non-value zs/FieldSchema))

    (testing "Collections are not values"
      (test-non-value zs/CollectionSchema))

    (testing "Conditions are not values"
      (test-non-value zs/ConditionSchema))

    (testing "Logical operations are not values"
      (test-non-value zs/LogicalOperationSchema))

    (testing "Nested queries are not values"
      (test-non-value zs/NestedQuerySchema))

    (testing "Values are values"
      (test-value zs/ValueSchema))))

(defschema-test zs/CollectionSchema zs/CollectionGenerator zs/collection?)

(deftest test-collection?
  (letfn [(test-non-collection [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/collection? expr)))))

          (test-collection [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/collection? expr))))]
    (testing "Fields are not collections"
      (test-non-collection zs/FieldSchema))

    (testing "Values are not collections"
      (test-non-collection zs/ValueSchema))

    (testing "Conditions are not collections"
      (test-non-collection zs/ConditionSchema))

    (testing "Logical operations are not collections"
      (test-non-collection zs/LogicalOperationSchema))

    (testing "Nested queries are not collections"
      (test-non-collection zs/NestedQuerySchema))

    (testing "Collections are collections"
      (test-collection zs/CollectionSchema))))

;;;; condition tests

(defschema-test zs/UnaryConditionSchema zs/UnaryConditionGenerator zs/unary-condition?)

(deftest test-unary-condition?
  (letfn [(test-non-unary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/unary-condition? expr)))))

          (test-unary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/unary-condition? expr))))]
    (testing "Fields are not unary conditions"
      (test-non-unary-condition zs/FieldSchema))

    (testing "Values are not unary conditions"
      (test-non-unary-condition zs/ValueSchema))

    (testing "Collections are not unary conditions"
      (test-non-unary-condition zs/CollectionSchema))

    (testing "Binary conditions are not unary conditions"
      (test-non-unary-condition zs/BinaryConditionSchema))

    (testing "Ternary conditions are not unary conditions"
      (test-non-unary-condition zs/TernaryConditionSchema))

    (testing "Logical operations are not unary conditions"
      (test-non-unary-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not unary conditions"
      (test-non-unary-condition zs/NestedQuerySchema))

    (testing "Unary conditions are unary conditions"
      (test-unary-condition zs/UnaryConditionSchema))))

(defschema-test zs/BinaryCompareConditionSchema zs/BinaryCompareConditionGenerator zs/binary-compare-condition?)

(deftest test-binary-compare-condition?
  (letfn [(test-non-binary-compare-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-compare-condition? expr)))))

          (test-binary-compare-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-compare-condition? expr))))]
    (testing "Fields are not binary comparison conditions"
      (test-non-binary-compare-condition zs/FieldSchema))

    (testing "Values are not binary comparison conditions"
      (test-non-binary-compare-condition zs/ValueSchema))

    (testing "Collections are not binary comparison conditions"
      (test-non-binary-compare-condition zs/CollectionSchema))

    (testing "Unary conditions are not binary comparison conditions"
      (test-non-binary-compare-condition zs/UnaryConditionSchema))

    (testing "Binary string conditions are not binary comparison conditions"
      (test-non-binary-compare-condition zs/BinaryStringConditionSchema))

    (testing "Binary collection conditions are not binary comparison conditions"
      (test-non-binary-compare-condition zs/BinaryCollectionConditionSchema))

    (testing "Ternary conditions are not binary comparison conditions"
      (test-non-binary-compare-condition zs/TernaryConditionSchema))

    (testing "Logical operations are not binary comparison conditions"
      (test-non-binary-compare-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not binary comparison conditions"
      (test-non-binary-compare-condition zs/NestedQuerySchema))

    (testing "Binary comparison conditions are binary comparison conditions"
      (test-binary-compare-condition zs/BinaryCompareConditionSchema))))

(defschema-test zs/BinaryStringConditionSchema zs/BinaryStringConditionGenerator zs/binary-string-condition?)

(deftest test-binary-string-condition?
  (letfn [(test-non-binary-string-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-string-condition? expr)))))

          (test-binary-string-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-string-condition? expr))))]
    (testing "Fields are not binary string conditions"
      (test-non-binary-string-condition zs/FieldSchema))

    (testing "Values are not binary string conditions"
      (test-non-binary-string-condition zs/ValueSchema))

    (testing "Collections are not binary string conditions"
      (test-non-binary-string-condition zs/CollectionSchema))

    (testing "Unary conditions are not binary string conditions"
      (test-non-binary-string-condition zs/UnaryConditionSchema))

    (testing "Binary comparison conditions are not binary string conditions"
      (test-non-binary-string-condition zs/BinaryCompareConditionSchema))

    (testing "Binary collection conditions are not binary string conditions"
      (test-non-binary-string-condition zs/BinaryCollectionConditionSchema))

    (testing "Ternary conditions are not binary string conditions"
      (test-non-binary-string-condition zs/TernaryConditionSchema))

    (testing "Logical operations are not binary string conditions"
      (test-non-binary-string-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not binary string conditions"
      (test-non-binary-string-condition zs/NestedQuerySchema))

    (testing "Binary string conditions are binary string conditions"
      (test-binary-string-condition zs/BinaryStringConditionSchema))))

(defschema-test zs/BinaryCollectionConditionSchema zs/BinaryCollectionConditionGenerator zs/binary-collection-condition?)

(deftest test-binary-collection-condition?
  (letfn [(test-non-binary-collection-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-collection-condition? expr)))))

          (test-binary-collection-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-collection-condition? expr))))]
    (testing "Fields are not binary collection conditions"
      (test-non-binary-collection-condition zs/FieldSchema))

    (testing "Values are not binary collection conditions"
      (test-non-binary-collection-condition zs/ValueSchema))

    (testing "Collections are not binary collection conditions"
      (test-non-binary-collection-condition zs/CollectionSchema))

    (testing "Unary conditions are not binary collection conditions"
      (test-non-binary-collection-condition zs/UnaryConditionSchema))

    (testing "Binary comparison conditions are not binary collection conditions"
      (test-non-binary-collection-condition zs/BinaryCompareConditionSchema))

    (testing "Binary string conditions are not binary collection conditions"
      (test-non-binary-collection-condition zs/BinaryStringConditionSchema))

    (testing "Ternary conditions are not binary collection conditions"
      (test-non-binary-collection-condition zs/TernaryConditionSchema))

    (testing "Logical operations are not binary collection conditions"
      (test-non-binary-collection-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not binary collection conditions"
      (test-non-binary-collection-condition zs/NestedQuerySchema))

    (testing "Binary collection conditions are binary collection conditions"
      (test-binary-collection-condition zs/BinaryCollectionConditionSchema))))

(defschema-test zs/BinaryConditionSchema zs/BinaryConditionGenerator zs/binary-condition?)

(deftest test-binary-condition?
  (letfn [(test-non-binary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-condition? expr)))))

          (test-binary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-condition? expr))))]
    (testing "Fields are not binary conditions"
      (test-non-binary-condition zs/FieldSchema))

    (testing "Values are not binary conditions"
      (test-non-binary-condition zs/ValueSchema))

    (testing "Collections are not binary conditions"
      (test-non-binary-condition zs/CollectionSchema))

    (testing "Unary conditions are not binary conditions"
      (test-non-binary-condition zs/UnaryConditionSchema))

    (testing "Ternary conditions are not binary conditions"
      (test-non-binary-condition zs/TernaryConditionSchema))

    (testing "Logical operations are not binary conditions"
      (test-non-binary-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not binary conditions"
      (test-non-binary-condition zs/NestedQuerySchema))

    (testing "Binary comparison conditions are binary conditions"
      (test-binary-condition zs/BinaryCompareConditionSchema))

    (testing "Binary string conditions are binary conditions"
      (test-binary-condition zs/BinaryStringConditionSchema))

    (testing "Binary collection conditions are binary conditions"
      (test-binary-condition zs/BinaryCollectionConditionSchema))))

(defschema-test zs/TernaryRangeConditionSchema zs/TernaryRangeConditionGenerator zs/ternary-range-condition?)

(deftest test-ternary-range-condition?
  (letfn [(test-non-ternary-range-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/ternary-range-condition? expr)))))

          (test-ternary-range-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/ternary-range-condition? expr))))]
    (testing "Fields are not ternary range conditions"
      (test-non-ternary-range-condition zs/FieldSchema))

    (testing "Values are not ternary range conditions"
      (test-non-ternary-range-condition zs/ValueSchema))

    (testing "Collections are not ternary range conditions"
      (test-non-ternary-range-condition zs/CollectionSchema))

    (testing "Unary conditions are not ternary range conditions"
      (test-non-ternary-range-condition zs/UnaryConditionSchema))

    (testing "Binary comparison conditions are not ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryCompareConditionSchema))

    (testing "Binary string conditions are not ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryStringConditionSchema))

    (testing "Binary collection conditions are not ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryCollectionConditionSchema))

    (testing "Logical operations are not ternary range conditions"
      (test-non-ternary-range-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not ternary range conditions"
      (test-non-ternary-range-condition zs/NestedQuerySchema))

    (testing "Ternary range conditions are ternary range conditions"
      (test-ternary-range-condition zs/TernaryRangeConditionSchema))))

(defschema-test zs/TernaryConditionSchema zs/TernaryConditionGenerator zs/ternary-condition?)

(deftest test-ternary-condition?
  (letfn [(test-non-ternary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/ternary-condition? expr)))))

          (test-ternary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/ternary-condition? expr))))]
    (testing "Fields are not ternary conditions"
      (test-non-ternary-condition zs/FieldSchema))

    (testing "Values are not ternary conditions"
      (test-non-ternary-condition zs/ValueSchema))

    (testing "Collections are not ternary conditions"
      (test-non-ternary-condition zs/CollectionSchema))

    (testing "Unary conditions are not ternary conditions"
      (test-non-ternary-condition zs/UnaryConditionSchema))

    (testing "Binary comparison conditions are not ternary conditions"
      (test-non-ternary-condition zs/BinaryCompareConditionSchema))

    (testing "Binary string conditions are not ternary conditions"
      (test-non-ternary-condition zs/BinaryStringConditionSchema))

    (testing "Binary collection conditions are not ternary conditions"
      (test-non-ternary-condition zs/BinaryCollectionConditionSchema))

    (testing "Logical operations are not ternary conditions"
      (test-non-ternary-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not ternary conditions"
      (test-non-ternary-condition zs/NestedQuerySchema))

    (testing "Ternary range conditions are ternary conditions"
      (test-ternary-condition zs/TernaryRangeConditionSchema))))

(defschema-test zs/ConditionSchema zs/ConditionGenerator zs/condition?)

(deftest test-condition?
  (letfn [(test-non-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/condition? expr)))))

          (test-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/condition? expr))))]
    (testing "Fields are not conditions"
      (test-non-condition zs/FieldSchema))

    (testing "Values are not conditions"
      (test-non-condition zs/ValueSchema))

    (testing "Collections are not conditions"
      (test-non-condition zs/CollectionSchema))

    (testing "Logical operations are not conditions"
      (test-non-condition zs/LogicalOperationSchema))

    (testing "Nested queries are not conditions"
      (test-non-condition zs/NestedQuerySchema))

    (testing "Unary conditions are conditions"
      (test-condition zs/UnaryConditionSchema))

    (testing "Binary conditions are conditions"
      (test-condition zs/BinaryConditionSchema))

    (testing "Ternary conditions are conditions"
      (test-condition zs/TernaryConditionSchema))))

;;;; logical operation tests

(defschema-test zs/UnaryLogicalOperationSchema zs/UnaryLogicalOperationGenerator zs/unary-logical-operation?)

(deftest test-unary-logical-operation?
  (letfn [(test-non-unary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/unary-logical-operation? expr)))))

          (test-unary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/unary-logical-operation? expr))))]
    (testing "Fields are not unary logical operations"
      (test-non-unary-logical-operation zs/FieldSchema))

    (testing "Values are not unary logical operations"
      (test-non-unary-logical-operation zs/ValueSchema))

    (testing "Collections are not unary logical operations"
      (test-non-unary-logical-operation zs/CollectionSchema))

    (testing "Conditions are not unary logical operations"
      (test-non-unary-logical-operation zs/ConditionSchema))

    (testing "N-ary logical operations are not unary logical operations"
      (test-non-unary-logical-operation zs/NaryLogicalOperationSchema))

    (testing "Nested queries are not unary logical operations"
      (test-non-unary-logical-operation zs/NestedQuerySchema))

    (testing "Unary logical operations are unary logical operations"
      (test-unary-logical-operation zs/UnaryLogicalOperationSchema))))

(defschema-test zs/NaryLogicalOperationSchema zs/NaryLogicalOperationGenerator zs/nary-logical-operation?)

(deftest test-nary-logical-operation?
  (letfn [(test-non-nary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/nary-logical-operation? expr)))))

          (test-nary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/nary-logical-operation? expr))))]
    (testing "Fields are not n-ary logical operations"
      (test-non-nary-logical-operation zs/FieldSchema))

    (testing "Values are not n-ary logical operations"
      (test-non-nary-logical-operation zs/ValueSchema))

    (testing "Collections are not n-ary logical operations"
      (test-non-nary-logical-operation zs/CollectionSchema))

    (testing "Conditions are not n-ary logical operations"
      (test-non-nary-logical-operation zs/ConditionSchema))

    (testing "Unary logical operations are not n-ary logical operations"
      (test-non-nary-logical-operation zs/UnaryLogicalOperationSchema))

    (testing "Nested queries are not n-ary logical operations"
      (test-non-nary-logical-operation zs/NestedQuerySchema))

    (testing "N-ary logical operations are n-ary logical operations"
      (test-nary-logical-operation zs/NaryLogicalOperationSchema))))

(defschema-test zs/LogicalOperationSchema zs/LogicalOperationGenerator zs/logical-operation?)

(deftest test-logical-operation?
  (letfn [(test-non-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/logical-operation? expr)))))

          (test-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/logical-operation? expr))))]
    (testing "Fields are not logical operations"
      (test-non-logical-operation zs/FieldSchema))

    (testing "Values are not logical operations"
      (test-non-logical-operation zs/ValueSchema))

    (testing "Collections are not logical operations"
      (test-non-logical-operation zs/CollectionSchema))

    (testing "Conditions are not logical operations"
      (test-non-logical-operation zs/ConditionSchema))

    (testing "Nested queries are not logical operations"
      (test-non-logical-operation zs/NestedQuerySchema))

    (testing "Unary logical operations are logical operations"
      (test-logical-operation zs/UnaryLogicalOperationSchema))

    (testing "N-ary logical operations are logical operations"
      (test-logical-operation zs/NaryLogicalOperationSchema))))

;;;; nested query tests

(defschema-test zs/NestedQuerySchema zs/NestedQueryGenerator zs/nested-query?)

(deftest test-nested-query?
  (letfn [(test-non-nested-query [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/nested-query? expr)))))

          (test-nested-query [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/nested-query? expr))))]
    (testing "Fields are not nested queries"
      (test-non-nested-query zs/FieldSchema))

    (testing "Values are not nested queries"
      (test-non-nested-query zs/ValueSchema))

    (testing "Collections are not nested queries"
      (test-non-nested-query zs/CollectionSchema))

    (testing "Conditions are not nested queries"
      (test-non-nested-query zs/ConditionSchema))

    (testing "Logical operations are not nested queries"
      (test-non-nested-query zs/LogicalOperationSchema))

    (testing "Nested queries are nested queries"
      (test-nested-query zs/NestedQuerySchema))))

;;;; expression tests

(defschema-test zs/ExpressionSchema zs/ExpressionGenerator zs/expression?)

(deftest test-expression?
  (letfn [(test-non-expression [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/expression? expr)))))

          (test-expression [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/expression? expr))))]
    (testing "Fields are not expressions"
      (test-non-expression zs/FieldSchema))

    (testing "Values are not expressions"
      (test-non-expression zs/ValueSchema))

    (testing "Collections are not expressions"
      (test-non-expression zs/CollectionSchema))

    (testing "Conditions are expressions"
      (test-expression zs/ConditionSchema))

    (testing "Logical operations are expressions"
      (test-expression zs/LogicalOperationSchema))

    (testing "Nested queries are expressions"
      (test-expression zs/NestedQuerySchema))))