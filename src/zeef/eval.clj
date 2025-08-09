(ns zeef.eval
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
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
  `(str/starts-with? ~x ~substr))

(defmethod transform-condition :ends-with?
  [[_ x substr]]
  `(str/ends-with? ~x ~substr))

(defmethod transform-condition :includes?
  [[_ x substr]]
  `(str/includes? ~x ~substr))

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

(defn translate-expression
  "Translates symbolic expression `expr` into a symbolic form
    
   `expr` has to conform to zeef's expression syntax.
   
   If the expression contains fields, these are resolved by calling `resolver`
   during evaluation. `resolver` is called with the argument passed to the
   predicate function and the name of the field as a keyword.
   
   There are two arities:
   - `(compile-expression expr)` uses `clojure.core/get` as the default resolver.
   - `(compile-expression expr resolver)` allows specifying a custom resolver.

   Throws an `ExceptionInfo` with message \"Invalid expression.\" if `expr` is
   not a valid expression. The exception contains additional information:
   - `:expr` - The invalid expression
   - `:explanation` - A string explaining why the expression doesn't match the
     spec
   - `:spec` - The spec that was used for validation"
  ([expr]
   (translate-expression expr get))
  ([expr resolver]
   (when-not (zf/expression? expr)
     (throw (ex-info "Invalid expression."
                     {:expr expr
                      :explanation (s/explain-str ::zs/expression expr)
                      :spec ::zs/expression})))
   (let [m (gensym)]
     `(fn [~m]
        ~(walk/postwalk
          (fn [node]
            (if (instance? Field node)
              `(~resolver ~m ~(.name node))
              node))
          (transform-expression expr))))))

(defn compile-expression
  "Compiles symbolic expression `expr` into a predicate function.
    
   `expr` has to conform to zeef's expression syntax.
   
   If the expression contains fields, these are resolved by calling `resolver`
   during evaluation. `resolver` is called with the argument passed to the
   predicate function and the name of the field as a keyword.
   
   Internally, `translate-expression` is used to generate the symbolic form of
   the expression, which is then evaluated using `eval` to produce the predicate
   function.

   There are two arities:
   - `(compile-expression expr)` uses `clojure.core/get` as the default resolver.
   - `(compile-expression expr resolver)` allows specifying a custom resolver.

   Throws an `ExceptionInfo` with message \"Invalid expression.\" if `expr` is
   not a valid expression. The exception contains additional information:
   - `:expr` - The invalid expression
   - `:explanation` - A string explaining why the expression doesn't match the
     spec
   - `:spec` - The spec that was used for validation

   Example:

   ```
   (let [pred (compile-expression [:between? (zeef.core/field :id) 1 3])
         data [{:id 1 :text \"a\"}
               {:id 3 :text \"b\"}
               {:id 5 :text \"c\"}]]
     (eduction (comp (filter pred) (map :text)) data)) => (\"a\" \"b\")
   ```"
  ([expr]
   (compile-expression expr get))
  ([expr resolver]
   (let [form (translate-expression expr resolver)]
     (eval form))))