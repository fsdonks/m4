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
  (eval `(s/or ~tag ~pred
               :nil nil?)))

(s/def ::clojure-number
  number?)

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

(s/def ::clojure-expression
  (s/or :primitive ::clojure-primitive
        :collection ::clojure-collection))

;; (s/def ::boolean #{true false})
;; (s/def ::text string?)
;; (s/def ::double double?)
;; (s/def ::double? (maybe-type :double? float?))
;; (s/def ::date inst?)
;; (s/def ::long integer?)
;; (s/def ::int integer?)
;; (s/def ::int? (maybe-type :int? integer?))
;; (s/def ::number number?)

;;parsing defaults defined by spork.util.parsing
(def specs
  '{:float? (maybe-type :float? float?)
    :date   inst?
    :long   integer?
    :double double?
    :double? (maybe-type :double? float?)
    :int    integer?
    :number number?
    :int?   (maybe-type :int? integer?)
    :clojure ::clojure-expression
    :float  float?
    :symbol symbol?
    :string string?
    :long?  (maybe-type :long? integer?)
    :keyword keyword?
    :literal ::clojure-expression
    :code    ::clojure-expression
    :boolean ::boolean
    :text   string?
    })

;;Coerces a spork.util.parsing schema
;;into a clojure.spec. To do this, we want
;;to ensure our map conforms to the defined
;;specification.  Order of keys is unimportant.

#_(defn schema->spec [schema]
  (eval `(s/map-of keyword?
                   (s/or   ~@(reduce (fn [acc x] (into acc x)) []
                                     (for [[fld parser] (seq schema)]
                                       [fld (get specs parser)]))
                           ))))

;;looks like what we "want" to do
;;is create a template for defining ns-qualified
;;specs, which live in their own namespace relative
;;to the name of the schema we're using.

;; (ns marathon.schema.thing)
;; (s/def ::field1 some-pred-or-spec)
;; (s/def ::field2 some-pred-or-spec)
;; (s/def ::thing
;;   (s/keys :req-un [::field1
;;                    ::field2]))


;;other option is to define a spec that
;;cats together the fields we care about.
;;process the kv pairs to see if they
;;match.
;;so, based on kv,
;;we could dispatch using ::or

#_(s/def ::kvp
  (s/or ::field1 some-spec
        ::field2 some-other-spec))

;;another way...
;;spec of kv tuples.
;;k corresponds to v.

#_(defn rows [schema]
  (eval `(s/coll-of (schema->spec ~schema))))

#_(let [rws (rows  (marathon.schemas/get-schema :DemandRecords))]
  (eval `(s/def ::demand-records ~rws)))
