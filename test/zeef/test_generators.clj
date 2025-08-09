(ns zeef.test-generators
  (:require [clojure.test.check.generators :as gen]
            [zeef.schema :as zs]))

(def ^:private InstGenerator
  (gen/fmap #(java.util.Date. %)
            (gen/choose 0 (System/currentTimeMillis))))

(def ^:private value-gens [gen/string
                           gen/small-integer
                           gen/boolean
                           gen/double
                           InstGenerator
                           gen/uuid
                           gen/keyword])

(defn FieldSameTypeTupleGenerator
  ([num-elements]
   (FieldSameTypeTupleGenerator num-elements value-gens))
  ([num-elements gens]
   (gen/bind (gen/elements (cons zs/FieldGenerator gens))
             (fn [g]
               (gen/vector (gen/one-of [g zs/FieldGenerator]) num-elements)))))