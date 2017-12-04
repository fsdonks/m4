;;This is an initial hack at writing generative
;;testing and validation specs for various
;;MARATHON constructs.  note: much of the
;;primitives, particularly stuff that wraps
;;spork tables and schemas, will likely
;;be moved to spork proper.  Only the
;;MARATHON-specific stuff will live
;;here long term.
(ns marathon.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            ;;brings in extras for clojure.core, shim between 1.8/1.9
            ;;we can drop this after moving to 1.9, everything will
            ;;keep working.
            [clojure.future :refer :all]
            [marathon.schemas :as schemas])
  )

;;let's start by validating demand records.
;;We'll defin a function that can map a
;;spork schema to a spec.

;;This has the benefit of validating and
;;providing a basis for generating
;;random records via s/conform.

(defn maybe-type [tag pred]
  (so/or tag pred
         :nil nil?))

(s/def ::clojure-number
  (s/or
   :long      long?
   :double    double?
   :int       int?
   :float     float?
   :ratio     ratio?
   :integer   integer?
   )

(s/def ::clojure-primitive
  (s/or
   :keyword   keyword?
   :symbol    symbol?
   :string    string?
   :character char?
   :numeric   ::clojure-number))

(s/def ::clojure-collection
  (s/or
   :list       list?
   :vector     vector?
   :hash-map   map?
   :hash-set   set?
   :double     double?
   :sequence   seq?
   :collection coll?))

#_(s/def ::clojure-expression
  )


;;parsing defaults defined by spork.util.parsing
(def primitives
  {:float? (maybe-type :float? float?)
   :date   inst?
   :long   long?
   :double double?
   :double? (maybe-type :double? double?)
   :int    int?
   :number number?
   :int?   (maybe-type :int? int?)
   :clojure 
   :float
   :symbol
   :string
   :long?
   :keyword
   :literal
   :code
   :boolean
   :text
   })

;;Coerces a spork.util.parsing schema
;;into a clojure.spec.
(defn schema->spec [schema]
  )
