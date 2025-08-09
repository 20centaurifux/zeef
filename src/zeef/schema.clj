(ns zeef.schema
  (:require [camel-snake-kebab.core :as csk]
            [clojure.test.check.generators :as gen]
            [malli.core :as m]
            [malli.generator :as mg]
            [zeef.types])
  (:import [zeef.types Field]))

(defmacro defschema
  "Defines a schema, its corresponding generator and predicate function."
  [name schema]
  `(do
     (def ~(symbol (str name "Schema"))
       ~schema)
     (def ~(symbol (str name "Generator"))
       (mg/generator ~(symbol (str name "Schema")) {:max-tries 1000}))
     (def ~(symbol (str (csk/->kebab-case name) "?"))
       (m/validator ~schema))))

(defn- same-value-types?
  [& args]
  (let [preds [string? integer? float? boolean? uuid? inst? keyword?]
        values (filter (complement #(instance? Field %)) args)]
    (-> (some (fn [pred]
                (every? pred values))
              preds)
        boolean)))

;;;; arguments

(defschema Field
  [:fn
   {:gen/gen (gen/fmap #(Field. %) gen/keyword)}
   (partial instance? Field)])

(defschema Value
  [:or :string integer? :float :boolean :uuid inst? :keyword])

(defschema Collection
  [:and
  [:or
   [:seqable :string]
   [:seqable integer?]
   [:seqable float?]
   [:seqable :boolean]
   [:seqable :uuid]
   [:seqable inst?]
   [:seqable :keyword]]
   [:not :string]])

;;;; conditions

;;; unary

(defschema UnaryCondition
  [:tuple
   [:enum :nil?]
   FieldSchema])

;;; binary

(defschema BinaryCompareCondition
  [:and
   [:tuple
    [:enum :< :<= := :> :>=]
    [:or FieldSchema ValueSchema]
    [:or FieldSchema ValueSchema]]
   [:fn (fn [[_ arg1 arg2]]
          (same-value-types? arg1 arg2))]])

(defschema BinaryStringCondition
  [:tuple
   [:enum :starts-with? :ends-with? :includes?]
   [:or FieldSchema string?]
   [:or FieldSchema string?]])

(defschema BinaryCollectionCondition
  [:tuple
   [:enum :in?]
   FieldSchema
   CollectionSchema])

(defschema BinaryCondition
  [:or
   BinaryCompareConditionSchema
   BinaryStringConditionSchema
   BinaryCollectionConditionSchema])

;;; ternary

(defschema TernaryRangeCondition
  [:and
   [:tuple
    [:enum :between?]
    [:or FieldSchema ValueSchema]
    [:or FieldSchema ValueSchema]
    [:or FieldSchema ValueSchema]]
   [:fn (fn [[_ arg1 arg2 arg3]]
          (same-value-types? arg1 arg2 arg3))]])

(defschema TernaryCondition
  TernaryRangeConditionSchema)

(defschema Condition
  [:or
   UnaryConditionSchema
   BinaryCompareConditionSchema
   BinaryStringConditionSchema
   BinaryCollectionConditionSchema
   TernaryConditionSchema])

;;;; logical operators & nested queries

(def ^:private LogicalOperationRegistry
  {:registry {::condition [:or
                           UnaryConditionSchema
                           BinaryCompareConditionSchema
                           BinaryStringConditionSchema
                           BinaryCollectionConditionSchema
                           TernaryConditionSchema]
              ::unary-logical-operator [:tuple
                                        [:enum :not]
                                        [:or
                                         FieldSchema
                                         [:or
                                          [:ref ::condition]
                                          [:ref ::unary-logical-operator]
                                          [:ref ::nary-logical-operator]
                                          [:ref ::nested-query]]]]
              ::nary-logical-operator [:cat
                                       [:enum :or :and]
                                       [:+ [:or
                                            [:ref ::condition]
                                            [:ref ::unary-logical-operator]
                                            [:ref ::nary-logical-operator]
                                            [:ref ::nested-query]]]]
              ::nested-query [:tuple
                              [:enum :satisfies? :some?]
                              FieldSchema
                              [:or
                               [:ref ::condition]
                               [:ref ::unary-logical-operator]
                               [:ref ::nary-logical-operator]
                               [:ref ::nested-query]]]}})

;;; unary

(defschema UnaryLogicalOperation
  [:schema LogicalOperationRegistry
   [:ref ::unary-logical-operator]])

;;; nary

(defschema NaryLogicalOperation
  [:schema LogicalOperationRegistry
   [:ref ::nary-logical-operator]])

(defschema LogicalOperation
  [:or
   UnaryLogicalOperationSchema
   NaryLogicalOperationSchema])

;;; nested queries

(defschema NestedQuery
  [:schema LogicalOperationRegistry
   [:ref ::nested-query]])

;;;; expressions

(defschema Expression
  [:or ConditionSchema LogicalOperationSchema NestedQuerySchema])