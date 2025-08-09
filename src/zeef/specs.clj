(ns zeef.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [zeef.types])
  (:import [zeef.types Field]))

;;;; field & value specs

(s/def ::fieldname keyword?)

(s/def ::field
  (s/with-gen
    (partial instance? Field)
    (fn []
      (gen/fmap (fn [k]
                  (Field. k))
                gen/keyword))))

(s/def ::value
  (s/or :string string?
        :int integer?
        :boolean boolean?
        :float float?
        :datetime inst?
        :uuid uuid?
        :keyword keyword?))

(s/def ::collection
  (s/with-gen
    (s/coll-of ::value :kind coll?)
    (fn []
      (gen/vector (s/gen ::value)))))

;;;; conditions

;;; unary

(s/def ::unary-condition-name #{:nil?})

(s/def ::unary-condition (s/cat
                          :fname ::unary-condition-name
                          :arg1 ::field))

;;; binary

(s/def ::binary-compare-condition-name #{:< :<= := :> :>=})

(s/def ::binary-compare-condition
  (s/or :value-value (s/and (s/cat :fname ::binary-compare-condition-name
                                   :arg1 ::value
                                   :arg2 ::value)
                            (fn [{[t] :arg1 [t2] :arg2}]
                              (= t t2)))
        :field-value (s/cat :fname ::binary-compare-condition-name
                            :arg1 ::field
                            :arg2 ::value)
        :value-field (s/cat :fname ::binary-compare-condition-name
                            :arg1 ::value
                            :arg2 ::field)
        :field-field (s/cat :fname ::binary-compare-condition-name
                            :arg1 ::field
                            :arg2 ::field)))

(s/def ::binary-string-condition-name #{:starts-with? :ends-with? :includes?})

(s/def ::binary-string-condition (s/cat :fname ::binary-string-condition-name
                                        :arg1 (s/or :field ::field
                                                    :string string?)
                                        :arg2 (s/or :field ::field
                                                    :string string?)))

(s/def ::binary-collection-condition-name #{:in?})

(s/def ::binary-collection-condition (s/cat :fname ::binary-collection-condition-name
                                            :arg1 (s/or :field ::field
                                                        :value ::value)
                                            :arg2 ::collection))

(s/def ::binary-condition (s/or :compare ::binary-compare-condition
                                :string ::binary-string-condition
                                :collection ::binary-collection-condition))

;;; ternary

(s/def ::ternary-range-condition-name #{:between?})

(s/def ::ternary-range-condition
  (s/with-gen
    (s/or :value-value-value (s/and (s/cat :fname ::ternary-range-condition-name
                                           :arg1 ::value
                                           :arg2 ::value
                                           :arg3 ::value)
                                    (fn [{[t1 _] :arg1
                                          [t2 _] :arg2
                                          [t3 _] :arg3}]
                                      (= t1 t2 t3)))
          :field-value-value (s/and (s/cat :fname ::ternary-range-condition-name
                                           :arg1 ::field
                                           :arg2 ::value
                                           :arg3 ::value)
                                    (fn [{[t1 _] :arg2
                                          [t2 _] :arg3}]
                                      (= t1 t2)))
          :value-field-value (s/and (s/cat :fname ::ternary-range-condition-name
                                           :arg1 ::value
                                           :arg2 ::field
                                           :arg3 ::value)
                                    (fn [{[t1 _] :arg1
                                          [t2 _] :arg3}]
                                      (= t1 t2)))
          :value-value-field (s/and (s/cat :fname ::ternary-range-condition-name
                                           :arg1 ::value
                                           :arg2 ::value
                                           :arg3 ::field)
                                    (fn [{[t1 _] :arg1
                                          [t2 _] :arg2}]
                                      (= t1 t2)))
          :value-field-field (s/cat :fname ::ternary-range-condition-name
                                    :arg1 ::value
                                    :arg2 ::field
                                    :arg3 ::field)
          :field-value-field (s/cat :fname ::ternary-range-condition-name
                                    :arg1
                                    ::field :arg2
                                    ::value :arg3 ::field)
          :field-field-value (s/cat :fname ::ternary-range-condition-name
                                    :arg1 ::field
                                    :arg2 ::field
                                    :arg3 ::value)
          :field-field-field (s/cat :fname ::ternary-range-condition-name
                                    :arg1 ::field
                                    :arg2 ::field
                                    :arg3 ::field))
    (fn []
      (gen/bind (gen/elements [string?
                               integer?
                               boolean?
                               float?
                               inst?
                               uuid?
                               keyword?])
                (fn [spec]
                  (apply gen/tuple
                         (s/gen ::ternary-range-condition-name)
                         (repeat 3 (gen/one-of [(s/gen spec) (s/gen ::field)]))))))))

(s/def ::ternary-condition ::ternary-range-condition)

(s/def ::condition (s/or :unary ::unary-condition
                         :binary ::binary-condition
                         :ternary ::ternary-condition))

;;;; logical operators

;;; unary

(s/def ::unary-logical-operator-name #{:not})

(s/def ::unary-logical-operator (s/cat
                                 :fname ::unary-logical-operator-name
                                 :arg1 (s/or :expression ::expression
                                             :field ::field)))

;;; nary

(s/def ::nary-logical-operator-name #{:and :or})

(s/def ::nary-logical-operator
  (s/cat
   :fname ::nary-logical-operator-name
   :arg1 ::expression
   :many (s/* ::expression)))

(s/def ::logical-operator (s/or :unary ::unary-logical-operator
                                :nary ::nary-logical-operator))

(s/def ::expression
  (s/with-gen
    (s/or :condition ::condition
          :logical ::logical-operator)
    (fn []
      (letfn [(expression-gen
                [depth]
                (if (zero? depth)
                  (s/gen ::condition)
                  (gen/let [args (gen/vector (expression-gen (dec depth)) 2)]
                    (if (= (count args) 1)
                      (gen/tuple (gen/elements [:not :and :or])
                                 (gen/return (first args)))
                      (gen/fmap (fn [op]
                                  (cons op args))
                                (gen/elements [:and :or]))))))]
        (gen/bind (gen/choose 1 3)
                  expression-gen)))))