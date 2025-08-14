(ns zeef.eval
  (:require [clojure.walk :as walk]
            [malli.core :as m]
            [malli.error :as me]
            [zeef.core :as zf]
            [zeef.schema :as zs]))

(defn- resolve-op
  [ns op]
  (if-let [v (ns-resolve ns (symbol op))]
    v
    (throw (ex-info "Couldn't resolve symbol."
                    {:sym (symbol op)}))))

(defn- transform-condition
  [[op & args]]
  (cond
    (#{:starts-with? :ends-with? :includes?} op)
    (cons (resolve-op 'clojure.string op) args)

    (= op :between?)
    (let [[x lo hi] args]
      `(clojure.core/<= ~lo ~x ~hi))

    (= op :in?)
    (let [[needle haystack] args]
      `(clojure.core/boolean
        (clojure.core/some (fn [v#]
                             (= v# ~needle))
                           ~haystack)))

    :else
    (cons (resolve-op 'clojure.core op) args)))

(defn- transform-logical-operator
  [[op & args]]
  (cons (resolve-op 'clojure.core op) args))

(defn- transform-satisfies?
  [sym resolver [_ field expr]]
  `(let [~sym (~resolver ~sym ~(.name field))]
     ~expr))

(defn- transform-some?
  [sym resolver [_ field expr]]
  `(let [~sym (~resolver ~sym ~(.name field))]
     (clojure.core/boolean
      (clojure.core/some (fn [~sym] ~expr)
                         ~sym))))

(defn- transform-field
  [sym resolver field]
  `(~resolver ~sym ~(.name field)))

(defn- transform-node
  [sym resolver node]
  (cond
    (coll? node)
    (let [op (first node)]
      (cond
        (#{:and :or :not} op)
        (transform-logical-operator node)

        (= :satisfies? op)
        (transform-satisfies? sym resolver node)

        (= :some? op)
        (transform-some? sym resolver node)

        (#{:nil? :< :<= := :> :>= :starts-with? :ends-with? :includes? :in? :between?} op)
        (transform-condition node)

        :else
        node))

    (zs/field? node)
    (transform-field sym resolver node)

    :else
    node))

(defn translate-expression
  "Translates symbolic expression `expr` into a symbolic form.
    
   `expr` must conform to zeef's expression syntax.
   
   If the expression contains fields, these are resolved by calling `resolver`
   during evaluation. `resolver` is called with the data being evaluated
   and the field name as a keyword.
   
   There are two arities:
   - `(translate-expression expr)` uses `clojure.core/get` as the default resolver.
   - `(translate-expression expr resolver)` allows specifying a custom resolver.

   Throws an `ExceptionInfo` with message \"Invalid expression.\" if `expr` is
   not a valid expression. The exception contains additional information:
   - `:expr` - The invalid expression
   - `:explanation` - A string explaining why the expression doesn't match
     zeef's expression syntax"
  ([expr]
   (translate-expression expr get))
  ([expr resolver]
   (when-not (zf/expression? expr)
     (throw (ex-info "Invalid expression."
                     {:expr expr
                      :explanation (-> (m/explain zs/ExpressionSchema expr)
                                       me/humanize)})))
   (let [sym (gensym "m")]
     `(fn [~sym]
        ~(walk/prewalk
          (partial transform-node sym resolver)
          expr)))))

(defn compile-expression
  "Compiles symbolic expression `expr` into a predicate function.
    
   `expr` must conform to zeef's expression syntax.
   
   If the expression contains fields, these are resolved by calling `resolver`
   during evaluation. `resolver` is called with the data being evaluated
   and the field name as a keyword.
   
   Internally, `translate-expression` is used to generate the symbolic form of
   the expression, which is then evaluated using `eval` to produce the predicate
   function.

   There are two arities:
   - `(compile-expression expr)` uses `clojure.core/get` as the default resolver.
   - `(compile-expression expr resolver)` allows specifying a custom resolver.

   Since the compiled expression is evaluated using `clojure.core/eval`, the
   resolver must be a symbol that can be resolved at evaluation time. This means:
   - Anonymous functions cannot be used as resolvers
   - Local variables or closures cannot be used as resolvers
   - Only globally accessible functions, vars, or fully qualified symbols work
   - The resolver symbol is embedded in the generated code and resolved during eval

   For dynamic resolvers, consider using a global var that you can rebind, or
   use `translate-expression` directly and handle evaluation in your own context.

   Throws an `ExceptionInfo` with message \"Invalid expression.\" if `expr` is
   not a valid expression. The exception contains additional information:
   - `:expr` - The invalid expression
   - `:explanation` - A string explaining why the expression doesn't match
      zeef's expression syntax

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