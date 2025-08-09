(ns zeef.eval-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [zeef.core :as zf]
            [zeef.eval :refer [compile-expression]]))

;;;; generative test runner

(def ^:private num-tests 10)

(defn- test-property
  [prop]
  (let [result (tc/quick-check num-tests prop)]
    (is (:pass? result)
        (str "Test failed with arguments: "
             (pr-str (-> result :shrunk :smallest))))))

;;;; generators

(defn- simple-map-gen
  []
  (gen/map (gen/return :a)
           (gen/choose 1 10)
           {:num-elements 1}))

;;;; test conditions

;;; test nil?

(deftest test-nil?
  (let [tuple-gen (gen/such-that (fn [[k1 k2]]
                                   (not= k1 k2))
                                 (gen/tuple gen/keyword gen/keyword gen/any))]
    (test-property (prop/for-all [[k1 k2 v] tuple-gen]
                     (let [expr [:nil? (zf/field k1)]
                           pred (compile-expression expr get)]
                       (and (not (pred {k1 v}))
                            (pred {k2 v})))))))

;;; test compare functions

(deftest test-compare-functions
  (let [tuple-gen (gen/tuple (simple-map-gen)
                             gen/small-integer)]
    (letfn [(test [op]
              (test-property (prop/for-all [[m v] tuple-gen]
                               (let [expr [op (zf/field :a) v]
                                     pred (compile-expression expr get)
                                     f (ns-resolve 'clojure.core (symbol op))]
                                 (= (f (:a m) v) (pred m))))))]
      (testing ":<"
        (test :<))

      (testing ":<="
        (test :<=))

      (testing ":="
        (test :=))

      (testing ":>"
        (test :>))

      (testing ":>="
        (test :>=)))))

;;; test string functions

(deftest test-string-functions
  (let [tuple-gen (gen/bind
                   (gen/fmap str/join (gen/vector gen/char-alpha 2 5))
                   (fn [s]
                     (let [start (rand-int (count s))
                           end (+ start (rand-int (inc (- (count s) start))))]
                       (gen/tuple (gen/return {:a s})
                                  (gen/frequency [[5 (gen/return (subs s start end))]
                                                  [1 gen/string-alphanumeric]])))))]
    (letfn [(test [k pred]
              (test-property (prop/for-all [[m s] tuple-gen]
                               (let [expr [k (zf/field :a) s]
                                     compiled-expr (compile-expression expr get)]
                                 (= (pred (:a m) s) (compiled-expr m))))))]
      (testing ":starts-with?"
        (test :starts-with? str/starts-with?))

      (testing ":ends-with?"
        (test :ends-with? str/ends-with?))

      (testing ":includes?"
        (test :includes? str/includes?)))))

;;; test collection functions

(deftest test-collection-functions
  (testing ":in?"
    (test-property (prop/for-all [m (simple-map-gen)
                                  coll (gen/vector (gen/choose 1 10))]
                     (let [expr [:in? (zf/field :a) coll]
                           pred (compile-expression expr get)]
                       (= (boolean (some (fn [v]
                                           (= (:a m) v))
                                         coll))
                          (pred m)))))))

;;; test range functions

(deftest test-range-functions
  (testing ":between?"
    (test-property (prop/for-all [m (simple-map-gen)
                                  start gen/small-integer
                                  end gen/small-integer]
                     (let [expr [:between? (zf/field :a) start end]
                           pred (compile-expression expr get)]
                       (= (<= start (:a m) end) (pred m)))))))

;;;; logical operators

(deftest test-nary-logical-functions
  (testing "unary"
    (test-property (prop/for-all [a gen/small-integer]
                     (let [expr [:not [:= (zf/field :a) a]]
                           pred (compile-expression expr get)]
                       (not (pred {:a a}))))))

  (testing "nary"
    (test-property (prop/for-all [a gen/small-integer
                                  b gen/small-integer
                                  c gen/small-integer]
                     (let [expr [:and
                                 [:= (zf/field :a) a]
                                 [:= b (zf/field :b)]
                                 [:or
                                  [:= (zf/field :c) c]]]
                           pred (compile-expression expr get)]
                       (pred {:a a :b b :c c}))))))