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
  [prop]
  (let [result (tc/quick-check num-tests prop)]
    (is (:pass? result)
        (str "Test failed with arguments: "
             (pr-str (-> result :shrunk :smallest))))))

(defmacro ^:private defschema-test
  [schema generator predicate]
  `(deftest ~(symbol (str "test-" (csk/->kebab-case-symbol schema)))
     (testing "schema validation"
       (test-property (prop/for-all [value# ~generator]
                        (m/validate ~schema value#))))

     (testing "predicate function")
     (test-property (prop/for-all [value# ~generator]
                      (~predicate value#)))))

;;;; arguments

(defschema-test zs/FieldSchema zs/FieldGenerator zs/field?)

(deftest test-field?
  (letfn [(test-non-field [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/field? expr)))))

          (test-field [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/field? expr))))]
    (testing "values are no fields"
      (test-non-field zs/ValueSchema))

    (testing "collections are no fields"
      (test-non-field zs/CollectionSchema))

    (testing "conditions are no fields"
      (test-non-field zs/ConditionSchema))

    (testing "logical operations are no fields"
      (test-non-field zs/LogicalOperationSchema))

    (testing "nested queries are no fields"
      (test-non-field zs/NestedQuerySchema))

    (testing "fields are fields"
      (test-field zs/FieldSchema))))

(defschema-test zs/ValueSchema zs/ValueGenerator zs/value?)

(deftest test-value?
  (letfn [(test-non-value [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/value? expr)))))

          (test-value [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/value? expr))))]
    (testing "fields are no values"
      (test-non-value zs/FieldSchema))

    (testing "collections are no values"
      (test-non-value zs/CollectionSchema))

    (testing "conditions are no values"
      (test-non-value zs/ConditionSchema))

    (testing "logical operations are no values"
      (test-non-value zs/LogicalOperationSchema))

    (testing "nested queries are no values"
      (test-non-value zs/NestedQuerySchema))

    (testing "values are values"
      (test-value zs/ValueSchema))))

(defschema-test zs/CollectionSchema zs/CollectionGenerator zs/collection?)

(deftest test-collection?
  (letfn [(test-non-collection [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/collection? expr)))))

          (test-collection [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/collection? expr))))]
    (testing "fields are no collections"
      (test-non-collection zs/FieldSchema))

    (testing "values are no collections"
      (test-non-collection zs/ValueSchema))

    (testing "conditions are no collections"
      (test-non-collection zs/ConditionSchema))

    (testing "logical operations are no collections"
      (test-non-collection zs/LogicalOperationSchema))

    (testing "nested queries are no collections"
      (test-non-collection zs/NestedQuerySchema))

    (testing "collections are collections"
      (test-collection zs/CollectionSchema))))

;;;; conditions

(defschema-test zs/UnaryConditionSchema zs/UnaryConditionGenerator zs/unary-condition?)

(deftest test-unary-condition?
  (letfn [(test-non-unary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/unary-condition? expr)))))

          (test-unary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/unary-condition? expr))))]
    (testing "fields are no unary conditions"
      (test-non-unary-condition zs/FieldSchema))

    (testing "values are no unary conditions"
      (test-non-unary-condition zs/ValueSchema))

    (testing "collections are no unary conditions"
      (test-non-unary-condition zs/CollectionSchema))

    (testing "binary conditions are no unary conditions"
      (test-non-unary-condition zs/BinaryConditionSchema))

    (testing "ternary conditions are no unary conditions"
      (test-non-unary-condition zs/TernaryConditionSchema))

    (testing "logical operations are no unary conditions"
      (test-non-unary-condition zs/LogicalOperationSchema))

    (testing "nested queries are no unary conditions"
      (test-non-unary-condition zs/NestedQuerySchema))

    (testing "unary conditions are unary conditions"
      (test-unary-condition zs/UnaryConditionSchema))))

(defschema-test zs/BinaryCompareConditionSchema zs/BinaryCompareConditionGenerator zs/binary-compare-condition?)

(deftest test-binary-compare-condition?
  (letfn [(test-non-binary-compare-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-compare-condition? expr)))))

          (test-binary-compare-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-compare-condition? expr))))]
    (testing "fields are no binary compare conditions"
      (test-non-binary-compare-condition zs/FieldSchema))

    (testing "values are no binary compare conditions"
      (test-non-binary-compare-condition zs/ValueSchema))

    (testing "collections are no binary compare conditions"
      (test-non-binary-compare-condition zs/CollectionSchema))

    (testing "unary conditions are no binary compare conditions"
      (test-non-binary-compare-condition zs/UnaryConditionSchema))

    (testing "binary string conditions are no binary compare conditions"
      (test-non-binary-compare-condition zs/BinaryStringConditionSchema))

    (testing "binary collection conditions are no binary compare conditions"
      (test-non-binary-compare-condition zs/BinaryCollectionConditionSchema))

    (testing "ternary conditions are no binary compare conditions"
      (test-non-binary-compare-condition zs/TernaryConditionSchema))

    (testing "logical operations are no binary compare conditions"
      (test-non-binary-compare-condition zs/LogicalOperationSchema))

    (testing "nested queries are no binary compare conditions"
      (test-non-binary-compare-condition zs/NestedQuerySchema))

    (testing "binary compare conditions are binary compare conditions"
      (test-binary-compare-condition zs/BinaryCompareConditionSchema))))

(defschema-test zs/BinaryStringConditionSchema zs/BinaryStringConditionGenerator zs/binary-string-condition?)

(deftest test-binary-string-condition?
  (letfn [(test-non-binary-string-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-string-condition? expr)))))

          (test-binary-string-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-string-condition? expr))))]
    (testing "fields are no binary string conditions"
      (test-non-binary-string-condition zs/FieldSchema))

    (testing "values are no binary string conditions"
      (test-non-binary-string-condition zs/ValueSchema))

    (testing "collections are no binary string conditions"
      (test-non-binary-string-condition zs/CollectionSchema))

    (testing "unary conditions are no binary string conditions"
      (test-non-binary-string-condition zs/UnaryConditionSchema))

    (testing "binary compare conditions are no binary string conditions"
      (test-non-binary-string-condition zs/BinaryCompareConditionSchema))

    (testing "binary collection conditions are no binary string conditions"
      (test-non-binary-string-condition zs/BinaryCollectionConditionSchema))

    (testing "ternary conditions are no binary string conditions"
      (test-non-binary-string-condition zs/TernaryConditionSchema))

    (testing "logical operations are no binary string conditions"
      (test-non-binary-string-condition zs/LogicalOperationSchema))

    (testing "nested queries are no binary string conditions"
      (test-non-binary-string-condition zs/NestedQuerySchema))

    (testing "binary string conditions are binary string conditions"
      (test-binary-string-condition zs/BinaryStringConditionSchema))))

(defschema-test zs/BinaryCollectionConditionSchema zs/BinaryCollectionConditionGenerator zs/binary-collection-condition?)

(deftest test-binary-string-condition?
  (letfn [(test-non-binary-string-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-string-condition? expr)))))

          (test-binary-string-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-string-condition? expr))))]
    (testing "fields are no binary string conditions"
      (test-non-binary-string-condition zs/FieldSchema))

    (testing "values are no binary string conditions"
      (test-non-binary-string-condition zs/ValueSchema))

    (testing "collections are no binary string conditions"
      (test-non-binary-string-condition zs/CollectionSchema))

    (testing "unary conditions are no binary string conditions"
      (test-non-binary-string-condition zs/UnaryConditionSchema))

    (testing "binary compare conditions are no binary string conditions"
      (test-non-binary-string-condition zs/BinaryCompareConditionSchema))

    (testing "binary collection conditions are no binary string conditions"
      (test-non-binary-string-condition zs/BinaryCollectionConditionSchema))

    (testing "ternary conditions are no binary string conditions"
      (test-non-binary-string-condition zs/TernaryConditionSchema))

    (testing "logical operations are no binary string conditions"
      (test-non-binary-string-condition zs/LogicalOperationSchema))

    (testing "nested queries are no binary string conditions"
      (test-non-binary-string-condition zs/NestedQuerySchema))

    (testing "binary string conditions are binary string conditions"
      (test-binary-string-condition zs/BinaryStringConditionSchema))))

(defschema-test zs/BinaryConditionSchema zs/BinaryConditionGenerator zs/binary-condition?)

(deftest test-binary-condition?
  (letfn [(test-non-binary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/binary-condition? expr)))))

          (test-binary-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/binary-condition? expr))))]
    (testing "fields are no binary conditions"
      (test-non-binary-condition zs/FieldSchema))

    (testing "values are no binary conditions"
      (test-non-binary-condition zs/ValueSchema))

    (testing "collections are no binary conditions"
      (test-non-binary-condition zs/CollectionSchema))

    (testing "unary conditions are no binary conditions"
      (test-non-binary-condition zs/UnaryConditionSchema))

    (testing "ternary conditions are no binary conditions"
      (test-non-binary-condition zs/TernaryConditionSchema))

    (testing "logical operations are no binary conditions"
      (test-non-binary-condition zs/LogicalOperationSchema))

    (testing "nested queries are no binary conditions"
      (test-non-binary-condition zs/NestedQuerySchema))

    (testing "binary compare conditions are binary conditions"
      (test-binary-condition zs/BinaryCompareConditionSchema))

    (testing "binary string conditions are binary conditions"
      (test-binary-condition zs/BinaryStringConditionSchema))

    (testing "binary collection conditions are binary conditions"
      (test-binary-condition zs/BinaryCollectionConditionSchema))))

(defschema-test zs/TernaryRangeConditionSchema zs/TernaryRangeConditionGenerator zs/ternary-range-condition?)

(deftest test-ternary-range-condition?
  (letfn [(test-non-ternary-range-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/ternary-range-condition? expr)))))

          (test-ternary-range-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/ternary-range-condition? expr))))]
    (testing "fields are no ternary range conditions"
      (test-non-ternary-range-condition zs/FieldSchema))

    (testing "values are no ternary range conditions"
      (test-non-ternary-range-condition zs/ValueSchema))

    (testing "collections are no ternary range conditions"
      (test-non-ternary-range-condition zs/CollectionSchema))

    (testing "unary conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/UnaryConditionSchema))

    (testing "binary compare conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryCompareConditionSchema))

    (testing "binary string conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryStringConditionSchema))

    (testing "binary collection conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryCollectionConditionSchema))

    (testing "logical operations are no ternary range conditions"
      (test-non-ternary-range-condition zs/LogicalOperationSchema))

    (testing "nested queries are no ternary range conditions"
      (test-non-ternary-range-condition zs/NestedQuerySchema))

    (testing "ternary range conditions are ternary range conditions"
      (test-ternary-range-condition zs/TernaryRangeConditionSchema))))

(defschema-test zs/TernaryConditionSchema zs/TernaryConditionGenerator zs/ternary-condition?)

(deftest test-ternary-range-condition?
  (letfn [(test-non-ternary-range-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/ternary-range-condition? expr)))))

          (test-ternary-range-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/ternary-range-condition? expr))))]
    (testing "fields are no ternary range conditions"
      (test-non-ternary-range-condition zs/FieldSchema))

    (testing "values are no ternary range conditions"
      (test-non-ternary-range-condition zs/ValueSchema))

    (testing "collections are no ternary range conditions"
      (test-non-ternary-range-condition zs/CollectionSchema))

    (testing "unary conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/UnaryConditionSchema))

    (testing "binary compare conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryCompareConditionSchema))

    (testing "binary string conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryStringConditionSchema))

    (testing "binary collection conditions are no ternary range conditions"
      (test-non-ternary-range-condition zs/BinaryCollectionConditionSchema))

    (testing "logical operations are no ternary range conditions"
      (test-non-ternary-range-condition zs/LogicalOperationSchema))

    (testing "nested queries are no ternary range conditions"
      (test-non-ternary-range-condition zs/NestedQuerySchema))

    (testing "ternary range conditions are ternary range conditions"
      (test-ternary-range-condition zs/TernaryRangeConditionSchema))))

(defschema-test zs/ConditionSchema zs/ConditionGenerator zs/condition?)

(deftest test-condition?
  (letfn [(test-non-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/condition? expr)))))

          (test-condition [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/condition? expr))))]
    (testing "fields are no conditions"
      (test-non-condition zs/FieldSchema))

    (testing "values are no conditions"
      (test-non-condition zs/ValueSchema))

    (testing "collections are no conditions"
      (test-non-condition zs/CollectionSchema))

    (testing "logical operations are no conditions"
      (test-non-condition zs/LogicalOperationSchema))

    (testing "nested queries are no conditions"
      (test-non-condition zs/NestedQuerySchema))

    (testing "unary conditions are conditions"
      (test-condition zs/UnaryConditionSchema))

    (testing "binary conditions are conditions"
      (test-condition zs/BinaryConditionSchema))

    (testing "ternary conditions are conditions"
      (test-condition zs/TernaryConditionSchema))))

;;;; logical operations

(defschema-test zs/UnaryLogicalOperationSchema zs/UnaryLogicalOperationGenerator zs/unary-logical-operation?)

(deftest test-unary-logical-operation?
  (letfn [(test-non-unary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/unary-logical-operation? expr)))))

          (test-unary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/unary-logical-operation? expr))))]
    (testing "fields are no unary logical operations"
      (test-non-unary-logical-operation zs/FieldSchema))

    (testing "values are no unary logical operations"
      (test-non-unary-logical-operation zs/ValueSchema))

    (testing "collections are no unary logical operations"
      (test-non-unary-logical-operation zs/CollectionSchema))

    (testing "conditions are no unary logical operations"
      (test-non-unary-logical-operation zs/ConditionSchema))

    (testing "nary logical operations are no unary logical operations"
      (test-non-unary-logical-operation zs/NaryLogicalOperationSchema))

    (testing "nested queries are no unary logical operations"
      (test-non-unary-logical-operation zs/NestedQuerySchema))

    (testing "unary logical operations are unary logical operations"
      (test-unary-logical-operation zs/UnaryLogicalOperationSchema))))

(defschema-test zs/NaryLogicalOperationSchema zs/NaryLogicalOperationGenerator zs/nary-logical-operation?)

(deftest test-nary-logical-operation?
  (letfn [(test-non-nary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/nary-logical-operation? expr)))))

          (test-nary-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/nary-logical-operation? expr))))]
    (testing "fields are no nary logical operations"
      (test-non-nary-logical-operation zs/FieldSchema))

    (testing "values are no nary logical operations"
      (test-non-nary-logical-operation zs/ValueSchema))

    (testing "collections are no nary logical operations"
      (test-non-nary-logical-operation zs/CollectionSchema))

    (testing "conditions are no nary logical operations"
      (test-non-nary-logical-operation zs/ConditionSchema))

    (testing "unary logical operations are no nary logical operations"
      (test-non-nary-logical-operation zs/UnaryLogicalOperationSchema))

    (testing "nested queries are no nary logical operations"
      (test-non-nary-logical-operation zs/NestedQuerySchema))

    (testing "nary logical operations are nary logical operations"
      (test-nary-logical-operation zs/NaryLogicalOperationSchema))))

(defschema-test zs/LogicalOperationSchema zs/LogicalOperationGenerator zs/logical-operation?)

(deftest test-logical-operation?
  (letfn [(test-non-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/logical-operation? expr)))))

          (test-logical-operation [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/logical-operation? expr))))]
    (testing "fields are no logical operations"
      (test-non-logical-operation zs/FieldSchema))

    (testing "values are no logical operations"
      (test-non-logical-operation zs/ValueSchema))

    (testing "collections are no logical operations"
      (test-non-logical-operation zs/CollectionSchema))

    (testing "conditions are no logical operations"
      (test-non-logical-operation zs/ConditionSchema))

    (testing "nested queries are no logical operations"
      (test-non-logical-operation zs/NestedQuerySchema))

    (testing "unary logical operations are logical operations"
      (test-logical-operation zs/UnaryLogicalOperationSchema))

    (testing "nary logical operations are logical operations"
      (test-logical-operation zs/NaryLogicalOperationSchema))))

;;;; nested queries

(defschema-test zs/NestedQuerySchema zs/NestedQueryGenerator zs/nested-query?)

(deftest test-nested-query?
  (letfn [(test-non-nested-query [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/nested-query? expr)))))

          (test-nested-query [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/nested-query? expr))))]
    (testing "fields are no nested queries"
      (test-non-nested-query zs/FieldSchema))

    (testing "values are no nested queries"
      (test-non-nested-query zs/ValueSchema))

    (testing "collections are no nested queries"
      (test-non-nested-query zs/CollectionSchema))

    (testing "conditions are no nested queries"
      (test-non-nested-query zs/ConditionSchema))

    (testing "logical operations are no nested queries"
      (test-non-nested-query zs/LogicalOperationSchema))

    (testing "nested queries are nested queries"
      (test-nested-query zs/NestedQuerySchema))))

;;;; expressions

(defschema-test zs/ExpressionSchema zs/ExpressionGenerator zs/expression?)

(deftest test-expression?
  (letfn [(test-non-expression [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (not (zs/expression? expr)))))

          (test-expression [schema]
            (test-property (prop/for-all [expr (mg/generator schema)]
                             (zs/expression? expr))))]
    (testing "fields are no expressions"
      (test-non-expression zs/FieldSchema))

    (testing "values are no expressions"
      (test-non-expression zs/ValueSchema))

    (testing "collections are no expressions"
      (test-non-expression zs/CollectionSchema))

    (testing "conditions are expressions"
      (test-expression zs/ConditionSchema))

    (testing "logical operations are expressions"
      (test-expression zs/LogicalOperationSchema))

    (testing "nested queries are expressions"
      (test-expression zs/NestedQuerySchema))))