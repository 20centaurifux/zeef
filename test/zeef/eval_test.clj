(ns zeef.eval-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [zeef.core :as zf]
            [zeef.eval :refer [compile-expression]]
            [zeef.schema :as zs]))

;;;; generative test runner

(def ^:private num-tests 20)

(defn- test-property
  "Runs a generative test property and asserts it passes.
  
   Executes `num-tests` test cases and fails if any test case fails."
  [prop]
  (let [result (tc/quick-check num-tests prop)]
    (is (:pass? result)
        (str "Test failed with arguments: "
             (pr-str (-> result :shrunk :smallest))))))

;;;; resolver

(deftest test-resolver
  (testing "default resolver"
    (test-property
     (prop/for-all [k gen/keyword
                    v zs/ValueGenerator]
       (let [expr [:= (zf/field k) v]
             compiled-expr (compile-expression expr)]
         (compiled-expr {k v})))))

  ;; Tests custom resolver that increments field values before comparison.
  ;; Expression expects v+1, resolver provides v+1, so test passes.
  (testing "custom resolver"
    (test-property
     (prop/for-all [k gen/keyword
                    v gen/small-integer]
       (let [expr [:= (zf/field k) (inc v)]
             compiled-expr (compile-expression expr
                                               (fn [m k]
                                                 (-> m k inc)))]
         (compiled-expr {k v}))))))

;;;; test conditions

;;; test nil?

(def ^:private diverging-keyword-pair-gen
  (gen/fmap vec
            (gen/set gen/keyword {:num-elements 2})))

(deftest test-nil?
  (test-property
   (prop/for-all [[k other] diverging-keyword-pair-gen
                  v gen/any]
     (let [expr [:nil? (zf/field k)]
           compiled-expr (compile-expression expr)]
       (and (not (compiled-expr {k v}))
            (compiled-expr {other v}))))))

;;; test compare functions

(defn- test-compare-function
  "Tests a comparison operator with all combinations of field/value operands."
  [op]
  (let [pred (ns-resolve 'clojure.core (symbol op))]
    (testing "field field"
      (test-property
       (prop/for-all [k gen/keyword
                      v gen/small-integer]
         (let [expr [op (zf/field k) (zf/field k)]
               compiled-expr (compile-expression expr)]
           (= (pred v v) (compiled-expr {k v}))))))

    (testing "field value"
      (test-property
       (prop/for-all [k gen/keyword
                      v gen/small-integer]
         (let [expr [op (zf/field k) v]
               compiled-expr (compile-expression expr)]
           (= (pred v v) (compiled-expr {k v}))))))

    (testing "value field"
      (test-property
       (prop/for-all [k gen/keyword
                      v gen/small-integer]
         (let [expr [op v (zf/field k)]
               compiled-expr (compile-expression expr)]
           (= (pred v v) (compiled-expr {k v}))))))

    (testing "value value"
      (test-property
       (prop/for-all [k gen/keyword
                      v gen/small-integer]
         (let [expr [op v v]
               compiled-expr (compile-expression expr)]
           (= (pred v v) (compiled-expr {k v}))))))))

(deftest test-compare-functions
  (testing ":<"
    (test-compare-function :<))

  (testing ":<="
    (test-compare-function :<=))

  (testing ":="
    (test-compare-function :=))

  (testing ":>"
    (test-compare-function :>))

  (testing ":>="
    (test-compare-function :>=)))

;;; test string functions

;; Generates [base-string, substring-or-random] pairs.
(def ^:private string-pair-gen
  (gen/bind
   (gen/fmap str/join (gen/vector gen/char-alpha 2 5))
   (fn [s]
     (let [start (rand-int (count s))
           end (+ start (rand-int (inc (- (count s) start))))]
       (gen/tuple (gen/return s)
                  (gen/one-of [(gen/return (subs s start end))
                               gen/string-alphanumeric]))))))

(defn- test-string-function
  "Tests a string operator with field/string and string/field combinations."
  [op]
  (let [pred (ns-resolve 'clojure.string (symbol op))]
    (test-property
     (prop/for-all [k gen/keyword
                    [s1 s2] string-pair-gen]
       (testing "field string"
         (let [m {k s1}
               expr [op (zf/field k) s2]
               compiled-expr (compile-expression expr)]
           (= (pred (k m) s2) (compiled-expr m))))

       (testing "string field"
         (let [m {k s2}
               expr [op s1 (zf/field k)]
               compiled-expr (compile-expression expr)]
           (= (pred s1 (k m)) (compiled-expr m))))))))

(deftest test-string-functions
  (testing ":starts-with?"
    (test-string-function :starts-with?))

  (testing ":ends-with?"
    (test-string-function :ends-with?))

  (testing ":includes?"
    (test-string-function :includes?)))

;;; test collection functions

(deftest test-collection-functions
  (testing ":in?"
    (test-property
     (prop/for-all [k gen/keyword
                    v gen/small-integer
                    coll (gen/vector (gen/choose 1 10))]
       (let [m {k v}
             expr [:in? (zf/field k) coll]
             compiled-expr (compile-expression expr)]
         (= (boolean (some (fn [v']
                             (= (k m) v'))
                           coll))
            (compiled-expr m)))))))

;;; test range functions

(deftest test-range-functions
  (testing ":between?"
    (let [tuple-gen (gen/tuple (gen/choose 1 100)
                               (gen/choose 1 50)
                               (gen/choose 50 100))]
      (testing "field value value"
        (test-property
         (prop/for-all [k gen/keyword
                        [v start end] tuple-gen]
           (let [expr [:between? (zf/field k) start end]
                 compiled-expr (compile-expression expr)]
             (= (<= start v end) (compiled-expr {k v}))))))

      (testing "value field value"
        (test-property
         (prop/for-all [k gen/keyword
                        [v start end] tuple-gen]
           (let [expr [:between? v (zf/field k) end]
                 compiled-expr (compile-expression expr)]
             (= (<= start v end) (compiled-expr {k start}))))))

      (testing "value value field"
        (test-property
         (prop/for-all [k gen/keyword
                        [v start end] tuple-gen]
           (let [expr [:between? v start (zf/field k)]
                 compiled-expr (compile-expression expr)]
             (= (<= start v end) (compiled-expr {k end})))))))))

;;;; logical operators

;;; test unary logical functions

(deftest test-unary-logical-functions
  (testing ":not"
    (test-property
     (prop/for-all [k gen/keyword
                    v gen/small-integer]
       (let [expr [:not [:= (zf/field k) v]]
             compiled-expr (compile-expression expr)]
         (not (compiled-expr {k v})))))))

;;; test n-ary logical functions

(defn- test-nary-logical-function
  "Tests a logical operator with two boolean expressions."
  [op pred]
  (test-property
   (prop/for-all [a (gen/choose 1 5)
                  b (gen/choose 3 8)
                  c (gen/choose 8 10)]
     (let [expr [op
                 [:> (zf/field :b) (zf/field :a)]
                 [:> (zf/field :c) (zf/field :b)]]
           compiled-expr (compile-expression expr)]
       (= (pred (> b a) (> c b))
          (compiled-expr {:a a :b b :c c}))))))

(deftest test-nary-logical-functions
  ;; Functions are passed instead of using `clojure.core/and` and
  ;; `clojure.core/or` directly because macros cannot be passed as first-class
  ;; values.
  (testing ":and"
    (test-nary-logical-function :and (fn [left right]
                                       (and left right))))

  (testing ":or"
    (test-nary-logical-function :or (fn [left right]
                                      (or left right)))))

;;;; nested query functions

(deftest test-nested-query-functions
  (testing ":satisfies?"
    (test-property
     (prop/for-all [k1 gen/keyword
                    k2 gen/keyword
                    v zs/ValueGenerator]
       (let [expr [:satisfies? (zf/field k1) [:= (zf/field k2) v]]
             compiled-expr (compile-expression expr)]
         (compiled-expr {k1 {k2 v}})))))

  (testing ":some?"
    (test-property
     (prop/for-all [k1 gen/keyword
                    k2 gen/keyword
                    v1 zs/ValueGenerator
                    v2 zs/ValueGenerator]
       (let [expr [:some? (zf/field k1) [:= (zf/field k2) v2]]
             compiled-expr (compile-expression expr)]
         (compiled-expr {k1 [{k2 v1} {k2 v2}]}))))))