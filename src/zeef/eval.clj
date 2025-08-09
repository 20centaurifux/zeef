(ns zeef.eval
  (:require [clojure.spec.alpha :as s]
            [clojure.string]
            [clojure.walk :as walk]
            [zeef.core :as zf]
            [zeef.specs]
            [zeef.types])
  (:import [zeef.types Field]))

(alias 'zs 'zeef.specs)

;;;; transformation

(declare transform-expression)

;;; conditions

(defmulti ^:private transform-condition first)

(defmethod transform-condition :starts-with?
  [[_ x substr]]
  `(clojure.string/starts-with? ~x ~substr))

(defmethod transform-condition :ends-with?
  [[_ x substr]]
  `(clojure.string/ends-with? ~x ~substr))

(defmethod transform-condition :includes?
  [[_ x substr]]
  `(clojure.string/includes? ~x ~substr))

(defmethod transform-condition :between?
  [[_ x lo hi]]
  `(clojure.core/<= ~lo ~x ~hi))

(defmethod transform-condition :in?
  [[_ needle haystack]]
  `(clojure.core/boolean
    (clojure.core/some (fn [v#]
                         (= v# ~needle))
                       ~haystack)))

(defn- resolve-op
  [ns op]
  (if-let [v (ns-resolve ns (symbol op))]
    v
    (throw (ex-info "Couldn't resolve symbol."
                    {:sym (symbol op)}))))

(defmethod transform-condition :default
  [[op & args]]
  (cons (resolve-op 'clojure.core op) args))

;;; operations

(defn- transform-operation
  [[op & args]]
  (cons (resolve-op 'clojure.core op)
        (map transform-expression args)))

;;; expressions

(defn- transform-expression
  [expr]
  (if (zf/condition? expr)
    (transform-condition expr)
    (transform-operation expr)))

(defn compile-expression
  "Compiles symbolic expression `expr` into a predicate function.
    
   `expr` has to conform to zeef's expression syntax.
   
   If the expression contains fields, these are resolved by calling `resolver`
   during evaluation. `resolver` is called with the argument passed to the
   predicate function and the name of the field as a keyword.
   
   Throws an `ExceptionInfo` with message \"Invalid expression.\" if `expr` is
   not a valid expression. The exception contains additional information:
   - `:expr` - The invalid expression
   - `:explanation` - A string explaining why the expression doesn't match the
     spec
   - `:spec` - The spec that was used for validation

   Example:

   ```
   (let [pred (compile-expression [:between? (zeef.core/field :id) 1 3] get)
         data [{:id 1 :text \"a\"}
               {:id 3 :text \"b\"}
               {:id 5 :text \"c\"}]]
     (eduction (comp (filter pred) (map :text)) data)) => (\"a\" \"b\")
   ```"
  [expr resolver]
  (when-not (zf/expression? expr)
    (throw (ex-info "Invalid expression."
                    {:expr expr
                     :explanation (s/explain-str ::zs/expression expr)
                     :spec ::zs/expression})))
  (let [m (gensym)
        form `(fn [~m]
                ~(walk/postwalk
                  (fn [node]
                    (if (instance? Field node)
                      `(~resolver ~m ~(.name node))
                      node))
                  (transform-expression expr)))]
    (eval form)))