(ns zeef.core
  (:require [clojure.spec.alpha :as s]
            [zeef.specs]
            [zeef.types])
  (:import [zeef.types Field]))

;;;; field

(defn field?
  "Returns true if `x` is a Field."
  [x]
  (instance? Field x))

(defn field
  "Returns a Field with the specified `name`."
  [name]
  {:pre [(s/valid? :zeef.specs/fieldname name)]}
  (Field. name))

;;;; expressions

(defn expression?
  "Returns true if `x` is a valid expression."
  [x]
  (s/valid? :zeef.specs/expression x))

(defn- expression
  [k arg1 & more]
  (let [expr (into [k] (cons arg1 more))]
    (when (expression? expr)
      expr)))

(defmacro ^:private defn-expression
  [name docstring binding]
  `(defn ~name
     ~docstring
     ~binding
     (expression ~(-> name str (subs 2) keyword) ~@binding)))

;;;; conditions

(defn condition?
  "Returns true if `x` is a valid condition"
  [x]
  (s/valid? :zeef.specs/condition x))

;;; nil?

(defn-expression z-nil?
  "Returns a condition that tests if `field` is nil."
  [x])

;;; compare functions

(defn-expression z-<
  "Returns a condition that tests if `a` is less than value `b`.
  Both `a` and `b` must be either a field or a value."
  [a b])

(defn-expression z-<=
  "Returns a condition that tests if `a` is less than or equal to `b`.
  Both `a` and `b` must be either a field or a value."
  [a b])

(defn-expression z-=
  "Returns a condition that tests if `a` equals `b`.
  Both `a` and `b` must be either a field or a value."
  [a b])

(defn-expression z->
  "Returns a condition that tests if `a` is greater than `b`.
  Both `a` and `b` must be either a field or a value."
  [a b])

(defn-expression z->=
  "Returns a condition that tests if `a` is greater than or equal to `b`.
  Both `a` and `b` must be either a field or a value."
  [a b])

;;; string functions

(defn-expression z-starts-with?
  "Returns a condition that tests if `s` starts with `prefix`.
  Both `x` and `prefix` must be either a field or a string."
  [x prefix])

(defn-expression z-ends-with?
  "Returns a condition that tests if `s` ends with `suffix`.
  Both `x` and `suffix` must be either a field or a string."
  [x suffix])

(defn-expression z-includes?
  "Returns a condition that tests if `s` contains substring `substr`.
  Both `x` and `substr` must be either a field or a string."
  [x substr])

;;; collection functions

(defn-expression z-in?
  "Returns a condition that tests if `coll` contains `x`. `x` must be either
   a field or value."
  [x coll])

;;; range functions

(defn-expression z-between?
  "Returns a a condition that tests if `x` is within range `lo` and `hi`.
  `x`, `lo`, and `hi` can each be a value or a field, but all values must be of
   the same type."
  [x lo hi])

;;;; logical operators

(defn-expression z-not
  "Returns an expression that returns true if `x` is logical false, false
   otherwise. `x` must be either a field or expression."
  [x])

(defn z-and
  "Combines one or more expressions into a logical AND expression."
  [expr1 & more]
  (apply expression :and expr1 more))

(defn z-or
  "Combines one or more expressions into a logical OR expression."
  [expr1 & more]
  (apply expression :or expr1 more))