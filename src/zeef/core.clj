(ns zeef.core
  (:require [zeef.core :as zf]
            [zeef.schema :as zs]
            [zeef.types])
  (:import [zeef.types Field]))

;;;; field

(defn field
  "Returns a Field with the specified `name`. Returns nil if `name` is not a
   keyword."
  [name]
  (when (keyword? name)
    (Field. name)))

;;;; expressions

(defn expression?
  "Tests if `x` is a condition, logical operation or nested query."
  [x]
  (or (zs/condition? x)
      (zs/logical-operation? x)
      (zs/nested-query? x)))

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

;;; nil?

(defn-expression z-nil?
  "Returns a condition that tests if `field` is nil.
   Returns nil if the expression is invalid."
  [field])

;;; compare functions

(defn-expression z-<
  "Returns a condition that tests if `a` is less than `b`.
   Both `a` and `b` must be either a field or a value.
   Returns nil if the expression is invalid."
  [a b])

(defn-expression z-<=
  "Returns a condition that tests if `a` is less than or equal to `b`.
   Both `a` and `b` must be either a field or a value.
   Returns nil if the expression is invalid."
  [a b])

(defn-expression z-=
  "Returns a condition that tests if `a` equals `b`.
   Both `a` and `b` must be either a field or a value.
   Returns nil if the expression is invalid."
  [a b])

(defn-expression z->
  "Returns a condition that tests if `a` is greater than `b`.
   Both `a` and `b` must be either a field or a value.
   Returns nil if the expression is invalid."
  [a b])

(defn-expression z->=
  "Returns a condition that tests if `a` is greater than or equal to `b`.
   Both `a` and `b` must be either a field or a value.
   Returns nil if the expression is invalid."
  [a b])

;;; string functions

(defn-expression z-starts-with?
  "Returns a condition that tests if `s` starts with `prefix`.
   Both `x` and `prefix` must be either a field or a string.
   Returns nil if the expression is invalid."
  [x prefix])

(defn-expression z-ends-with?
  "Returns a condition that tests if `s` ends with `suffix`.
   Both `x` and `suffix` must be either a field or a string.
   Returns nil if the expression is invalid."
  [x suffix])

(defn-expression z-includes?
  "Returns a condition that tests if `s` contains substring `substr`.
   Both `x` and `substr` must be either a field or a string.
   Returns nil if the expression is invalid."
  [x substr])

;;; range functions

(defn-expression z-between?
  "Returns a a condition that tests if `x` is within range `lo` and `hi`
   (inclusive). `x`, `lo`, and `hi` can each be a value or a field, but all
   values must be of the same type.
   Returns nil if the expression is invalid."
  [x lo hi])

;;; collection functions

(defn-expression z-in?
  "Returns a condition that tests if `x` is contained in `coll`. `x` must be 
   either a field or value, and `coll` must be a collection.
   Returns nil if the expression is invalid."
  [x coll])

;;; nested query functions

(defn-expression z-satisfies?
  "Returns a condition that tests if `expr` evaluates to logical true when 
   applied to the value of `field`.
   Returns nil if the expression is invalid."
  [field expr])

(defn-expression z-some?
  "Returns a condition that tests if any element in `field` satisfies `expr`.
   Returns nil if the expression is invalid."
  [field expr])

;;; logical operators

(defn-expression z-not
  "Returns an expression that returns true if `field` is logical false, false
   otherwise.
   Returns nil if the expression is invalid."
  [field])

(defn z-and
  "Combines one or more expressions into a logical AND expression.
   Returns nil if the expression is invalid."
  [expr1 & more]
  (apply expression :and expr1 more))

(defn z-or
  "Combines one or more expressions into a logical OR expression.
   Returns nil if the expression is invalid."
  [expr1 & more]
  (apply expression :or expr1 more))