(ns zeef.types-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [zeef.types :as zt])
  (:import [zeef.types Field]))

(def ^:private num-tests 50)

(defn- test-property
  [prop]
  (let [result (tc/quick-check num-tests prop)]
    (is (:pass? result)
        (str "Test failed with arguments: "
             (pr-str (-> result :shrunk :smallest))))))

;;;; Field type tests

(deftest test-field-creation
  (testing "Field creation with"
    (let [field (Field. :test)]
      (is (instance? Field field))
      (is (= :test (.name field))))))

(deftest test-field-string-representation
  (testing "toString method"
    (test-property
     (prop/for-all [kw gen/keyword]
       (let [field (Field. kw)
             expected (str "#zeef/field " kw)]
         (= expected (str field))))))

  (testing "print-method"
    (test-property
     (prop/for-all [kw gen/keyword]
       (let [field (Field. kw)
             expected (str "#zeef/field " kw)]
         (= expected (pr-str field)))))))

(deftest test-str-vs-pr-str-consistency
  (testing "str and pr-str return same value"
    (test-property
     (prop/for-all [kw gen/keyword]
       (let [field (Field. kw)]
         (= (str field) (pr-str field)))))))

;;;; Reader function tests

(deftest test-read-field
  (testing "read-field with keyword name"
    (test-property
     (prop/for-all [kw gen/keyword]
       (let [kw-str (name kw)
             field (zt/read-field kw-str)]
         (and (instance? Field field)
              (= kw (.name field)))))))

  (testing "read-field with namespace qualified keyword"
    (let [field (zt/read-field "ns/test")]
      (is (instance? Field field))
      (is (= :ns/test (.name field))))))

;;;; Serialization/Deserialization tests

(deftest test-serialization-roundtrip
  (testing "Field serialization roundtrip"
    (test-property
     (prop/for-all [kw gen/keyword]
       (let [original-field (Field. kw)
             serialized (pr-str original-field)
             deserialized (zt/read-string serialized)]
         (and (instance? Field deserialized)
              (= (.name original-field) (.name deserialized)))))))

  (testing "Complex data with Field serialization"
    (let [data [:and [:= (Field. :age) 25] [:> (Field. :score) 80]]
          serialized (pr-str data)
          deserialized (zt/read-string serialized)]
      (is (= (count data) (count deserialized)))
      (is (= (first data) (first deserialized)))
      ;; Check that Fields are properly deserialized
      (let [[_ [op1 field1 val1] [op2 field2 val2]] deserialized]
        (is (= op1 :=))
        (is (instance? Field field1))
        (is (= :age (.name field1)))
        (is (= val1 25))
        (is (= op2 :>))
        (is (instance? Field field2))
        (is (= :score (.name field2)))
        (is (= val2 80))))))

(deftest test-read-string-with-fields
  (testing "Simple field deserialization"
    (let [serialized "#zeef/field :name"
          field (zt/read-string serialized)]
      (is (instance? Field field))
      (is (= :name (.name field)))))

  (testing "Field in vector deserialization"
    (let [serialized "[:= #zeef/field :age 30]"
          result (zt/read-string serialized)]
      (is (vector? result))
      (is (= 3 (count result)))
      (is (= := (first result)))
      (is (instance? Field (second result)))
      (is (= :age (.name (second result))))
      (is (= 30 (nth result 2)))))

  (testing "Nested structure with multiple fields"
    (let [serialized "[:and [:= #zeef/field :name \"Alice\"] [:> #zeef/field :age 18]]"
          result (zt/read-string serialized)]
      (is (vector? result))
      (is (= :and (first result)))
      (is (every? vector? (rest result)))
      (is (every? #(instance? Field (second %)) (rest result))))))

;;;; Error handling tests

(deftest test-error-handling
  (testing "Invalid EDN without zeef readers"
    (is (thrown? Exception
                 (clojure.edn/read-string "#zeef/field :test"))))

  (testing "Empty string"
    (is (thrown? Exception
                 (zt/read-string ""))))

  (testing "Invalid EDN"
    (is (thrown? Exception
                 (zt/read-string "[invalid edn")))))

;;;; Reader map tests

(deftest test-readers-map
  (testing "readers contains zeef/field"
    (is (contains? zt/readers 'zeef/field))
    (is (= zt/read-field (get zt/readers 'zeef/field)))))