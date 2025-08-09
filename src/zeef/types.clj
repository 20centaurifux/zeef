(ns zeef.types
  (:require [clojure.edn :as edn]))

(deftype Field [name]
  Object
  (toString [_]
    (str "#zeef/field " name)))

(defmethod print-method Field [field ^java.io.Writer writer]
  (.write writer (str "#zeef/field " (.name field))))

(defn read-field
  "Reader function for the zeef/field EDN tag.
  
  Takes a string representation of a field name and creates a Field instance
  with the name converted to a keyword."
  [name]
  (Field. (keyword name)))

(def readers
  "A map of reader functions for custom EDN tags.

  Contains reader functions that can be used with clojure.edn/read-string
  to parse custom tagged literals. Currently supports:
  - zeef/field: Creates a Field instance from a string representation"
  {'zeef/field read-field})

(defn read-string
  "Reads an EDN string with support for custom zeef types.
  
  This is a wrapper around clojure.edn/read-string that includes
  the custom readers defined in this namespace. This allows parsing
  of EDN strings containing zeef-specific tagged literals like #zeef/field."
  [s]
  (edn/read-string {:readers readers} s))