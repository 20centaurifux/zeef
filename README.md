# Zeef

zeef is a **lingua franca** for filter expressions in Clojure. It provides a unified and declarative way to define and evaluate conditions and logical expressions.

## Installation

Zeef is available via [Clojars](https://clojars.org/). To use zeef in your project, add the following dependency to your `project.clj`:

```clojure
:dependencies [[de.dixieflatline/zeef "0.1.0-SNAPSHOT"]]
```

## Filter Expressions

Filter expressions in zeef can either be **conditions** or **logical expressions**.

### Conditions

Conditions are simple expressions based on fields or values. Examples:

- `z-nil?` – Tests if a field is `nil`.
- `z-<` – Tests if a value or field is less than another.
- `z-starts-with?` – Tests if a string starts with a specific prefix.

### Logical Expressions

Logical expressions combine conditions or other logical expressions. Examples:

- `z-not` – Negates an expression.
- `z-and` – Combines multiple expressions with a logical AND.
- `z-or` – Combines multiple expressions with a logical OR.

## Example

Here is an example of how to use zeef:

```clojure
(require '[zeef.core :as zf])

(def my-filter
  (zf/z-and
    (zf/z-< (z/field :age) 30)
    (zf/z-starts-with? (zf/field :name) "A")))

(println "Filter expression is valid:" (zf/expression? my-filter))
```

This example defines a filter that checks if the age is less than 30 and the name starts with "A".