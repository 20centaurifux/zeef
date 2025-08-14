# zeef

zeef is a DSL for filter expressions in Clojure. It provides a unified and declarative way to define conditions, logical expressions, and nested queries on structured data. 

Filter expressions in zeef are structured Clojure data that use special field references and operators, making them easy to serialize, store, and manipulate programmatically.

Use zeef when you want to define filter logic once and apply it across multiple data sources – from in-memory collections to databases, search engines, and APIs – without rewriting your filtering logic for each backend.

zeef comes with a built-in evaluation module that compiles filter expressions into predicate functions, enabling seamless integration with standard Clojure collection functions like `filter`, `remove`, and `some`.

## Installation

zeef is available via [Clojars](https://clojars.org/). To use zeef in your project, add the following dependency to your `project.clj`:

```clojure
:dependencies [[de.dixieflatline/zeef "0.1.0-SNAPSHOT"]]
```

## Quick Start

Here's a simple example to get you started:

```clojure
(require '[zeef.core :as zf]
         '[zeef.eval :as ze])

;; Define a filter expression
(def adult-filter (zf/z->= (zf/field :age) 18))

;; Compile to a predicate function
(def adult? (ze/compile-expression adult-filter))

;; Use with data
(adult? {:name "Alice" :age 25}) ; => true
(adult? {:name "Bob" :age 16})   ; => false

;; Use with collections
(def people [{:name "Alice" :age 25} {:name "Bob" :age 16}])
(filter adult? people) ; => ({:name "Alice", :age 25})
```

## Building Filter Expressions

Filter expressions in zeef can be:
- **Conditions** – Simple tests on fields or values
- **Logical Expressions** – Combine conditions with AND, OR, NOT
- **Nested Queries** – Test properties of nested objects or collections

Once defined, filter expressions can be compiled into executable predicate functions using the `zeef.eval` module. You can also create custom evaluation modules to translate filter expressions for different data sources (databases, search engines, APIs, etc.), enabling the same filter expressions to work across multiple backends.

### Fields

Use `field` to reference data fields:

```clojure
(require '[zeef.core :as zf])

(zf/field :name) ; references the :name field
(zf/field :age)  ; references the :age field
```

### Conditions

#### Nil Test
- `z-nil?` – Tests if a field is `nil`

```clojure
(zf/z-nil? (zf/field :name))
```

#### Comparison Operations
- `z-<` – Less than
- `z-<=` – Less than or equal to  
- `z-=` – Equal to
- `z->` – Greater than
- `z->=` – Greater than or equal to

```clojure
(zf/z-< (zf/field :age) 30)          ; age < 30
(zf/z->= (zf/field :score) 80)       ; score >= 80
(zf/z-= (zf/field :status) "active") ; status = "active"
```

#### String Operations
- `z-starts-with?` – Tests if a string starts with a prefix
- `z-ends-with?` – Tests if a string ends with a suffix
- `z-includes?` – Tests if a string contains a substring

```clojure
(zf/z-starts-with? (zf/field :name) "A")        ; name starts with "A"
(zf/z-ends-with? (zf/field :email) ".com")      ; email ends with ".com"
(zf/z-includes? (zf/field :description) "test") ; description contains "test"
```

#### Collection Operations
- `z-in?` – Tests if a value is in a collection

```clojure
(zf/z-in? (zf/field :category) ["tech" "science" "news"]) ; category in collection
```

#### Range Operations
- `z-between?` – Tests if a value is within a range (inclusive)

```clojure
(zf/z-between? (zf/field :age) 18 65) ; 18 <= age <= 65
```

### Logical Expressions

Combine conditions with logical operators:

- `z-not` – Logical negation
- `z-and` – Logical AND (accepts multiple arguments)
- `z-or` – Logical OR (accepts multiple arguments)

```clojure
(zf/z-not (zf/z-nil? (zf/field :name))) ; name is not nil

(zf/z-and                               ; age < 30 AND status = "active"
  (zf/z-< (zf/field :age) 30)
  (zf/z-= (zf/field :status) "active"))

(zf/z-or                                ; age < 18 OR age > 65
  (zf/z-< (zf/field :age) 18)
  (zf/z-> (zf/field :age) 65))
```

### Nested Queries

Test properties of nested objects or collections:

- `z-satisfies?` – Tests if a nested object satisfies an expression
- `z-some?` – Tests if any element in a collection satisfies an expression

```clojure
;; Test if nested address has a specific city
(zf/z-satisfies? (zf/field :address) 
                 (zf/z-= (zf/field :city) "Berlin"))

;; Test if any order has a value > 100
(zf/z-some? (zf/field :orders)
            (zf/z-> (zf/field :value) 100))
```

## Compilation and Evaluation

The `zeef.eval` module provides the `compile-expression` function that transforms declarative filter expressions into predicate functions:

```clojure
(require '[zeef.core :as zf]
         '[zeef.eval :as ze])

;; Define a filter expression
(def age-filter (zf/z->= (zf/field :age) 18))

;; Compile it to a predicate function
(def age-predicate (ze/compile-expression age-filter))

;; Use the predicate function
(age-predicate {:name "Alice" :age 25}) ; => true
(age-predicate {:name "Bob" :age 16})   ; => false
```

### Custom Resolvers

By default, `compile-expression` uses Clojure's `get` function to resolve field values. You can provide a custom resolver function:

```clojure
;; Custom resolver for nested field access
(defn nested-resolver [data field-name]
  (get-in data [:user field-name]))

(def custom-predicate 
  (ze/compile-expression 
    (zf/z-= (zf/field :status) "active")
    nested-resolver))

;; Works with nested data structure
(custom-predicate {:user {:status "active" :name "Alice"}}) ; => true
```

## Serialization and Deserialization

zeef filter expressions can be serialized to and from EDN format, making them easy to store in databases, transmit over networks, or persist to files. This is particularly useful when you need to save user-defined filters or share filter logic between systems.

Use `pr-str` to serialize any zeef filter expression to an EDN string:

```clojure
(require '[zeef.core :as zf]
         '[zeef.types :as zt])

;; Create a complex filter expression
(def user-filter
  (zf/z-and
    (zf/z->= (zf/field :age) 18)
    (zf/z-= (zf/field :status) "active")
    (zf/z-some? (zf/field :orders)
                (zf/z-> (zf/field :amount) 100))))

;; Serialize to EDN string
(def serialized-filter (pr-str user-filter))
;; => "[:and [:>= #zeef/field :age 18] [:= #zeef/field :status \"active\"] [:some? #zeef/field :orders [:> #zeef/field :amount 100]]]"

;; Deserialize back to filter expression
(def deserialized-filter (zt/read-string serialized-filter))

;; The deserialized expression works exactly like the original
(def pred (ze/compile-expression deserialized-filter))
(pred {:age 25 :status "active" :orders [{:amount 150}]}) ; => true
```

> **Important:** Always use `zeef.types/read-string` instead of `clojure.edn/read-string` when deserializing, as it includes the necessary reader for the `#zeef/field` tagged literal.

## Examples

All examples below demonstrate how filter expressions are compiled into predicate functions using the `zeef.eval` module.

### Basic Filtering

```clojure
(require '[zeef.core :as zf]
         '[zeef.eval :as ze])

;; Define a filter expression for young adults in tech
(def young-tech-filter
  (zf/z-and
    (zf/z-between? (zf/field :age) 18 35)
    (zf/z-= (zf/field :industry) "tech")))

;; Compile the expression to a predicate function
(def predicate (ze/compile-expression young-tech-filter))

;; Sample data
(def people
  [{:name "Alice" :age 25 :industry "tech"}
   {:name "Bob" :age 45 :industry "finance"}
   {:name "Charlie" :age 30 :industry "tech"}
   {:name "Diana" :age 22 :industry "healthcare"}])

;; Apply the compiled predicate function to filter data
(filter predicate people)
;; => ({:name "Alice", :age 25, :industry "tech"}
;;     {:name "Charlie", :age 30, :industry "tech"})
```

### Complex Filtering with Nested Data

```clojure
;; Define a complex filter expression with nested queries
(def premium-user-filter
  (zf/z-or
    ;; Has active premium subscription
    (zf/z-satisfies? (zf/field :subscription)
                     (zf/z-and
                       (zf/z-= (zf/field :type) "premium")
                       (zf/z-= (zf/field :status) "active")))
    ;; Has any order > $500
    (zf/z-some? (zf/field :orders)
                (zf/z-> (zf/field :amount) 500))))

(def users
  [{:name "Alice"
    :subscription {:type "premium" :status "active"}
    :orders [{:amount 50} {:amount 200}]}
   {:name "Bob" 
    :subscription {:type "basic" :status "active"}
    :orders [{:amount 600} {:amount 100}]}
   {:name "Charlie"
    :subscription {:type "premium" :status "expired"}
    :orders [{:amount 25}]}])

;; Compile the expression to a predicate function
(def premium-predicate (ze/compile-expression premium-user-filter))

;; Apply the predicate to filter users
(filter premium-predicate users)
;; => Both Alice (premium subscription) and Bob (high-value order)
```

## Function Reference

The following table shows which argument types are supported for each zeef expression builder function:

| Function | Argument 1 | Argument 2 | Argument 3 | Description |
|----------|------------|------------|------------|-------------|
| **Nil Test** |
| `z-nil?` | Field | - | - | Tests if field is nil |
| **Comparison Operations** |
| `z-<` | Field/Value | Field/Value | - | Less than |
| `z-<=` | Field/Value | Field/Value | - | Less than or equal |
| `z-=` | Field/Value | Field/Value | - | Equal to |
| `z->` | Field/Value | Field/Value | - | Greater than |
| `z->=` | Field/Value | Field/Value | - | Greater than or equal |
| **String Operations** |
| `z-starts-with?` | Field/String | Field/String | - | String starts with prefix |
| `z-ends-with?` | Field/String | Field/String | - | String ends with suffix |
| `z-includes?` | Field/String | Field/String | - | String contains substring |
| **Collection Operations** |
| `z-in?` | Field/Value | Collection | - | Value is in collection |
| **Range Operations** |
| `z-between?` | Field/Value | Field/Value | Field/Value | Value is within range (inclusive) |
| **Logical Operations** |
| `z-not` | Expression | - | - | Logical negation |
| `z-and` | Expression | Expression+ | - | Logical AND (variadic) |
| `z-or` | Expression | Expression+ | - | Logical OR (variadic) |
| **Nested Queries** |
| `z-satisfies?` | Field | Expression | - | Nested object satisfies expression |
| `z-some?` | Field | Expression | - | Any collection element satisfies expression |

### Argument Types

- **Field**: Created with `(zf/field :keyword)` - references a data field
- **Value**: String, Integer, Float, Boolean, UUID, Inst, Keyword
- **String**: String values specifically
- **Collection**: Vector, list, or set of values
- **Expression**: Any valid zeef filter expression
- **Expression+**: One or more expressions (variadic)