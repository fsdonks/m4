;A generic namespace for drawing samples from populations.
(ns util.sampling
  (:require [util [stats :as stats]]))

;This rose from the "stochastic demand generation" problem that was presented
;in a somewhat convoluted solution in VBA...
;The basic process is this: 
;  We have a population of records P.
;  We want to define functions that sample from the population, according to 
;    ad-hoc constraints, to generate samples of records.  These functions are
;    record-generators, and they typically involve identifying a set of records,
;    the "template" population, and then modifying the template according to 
;    some function.  Modification means mapping a function to the template 
;    population that returns a sampled population (i.e. transforming template
;    or original records into sampled records).  
;  We then want to define a sample as a function of a population and a sequence of
;  or [record-generator, frequency] pairs, that reduces the sequence of one 
;  generator pairs by applying the record-generator to the population, frequency 
;  times, accumulating the resulting sample-records.

;  There is an additional, post-processing phase, where we validate the sample.
;  A valid sample is a set of records in which:
;    A set of dependencies between records from the original population P are 
;    enforced, in that if record A from population P exists in sample S, and
;    record B from population P is dependent upon A, then record B must also 
;    exist in S, where 
;    dependent:: record -> record -> boolean

;    For two records x1, x2, which are members of an equivalence class, denoted
;    by equivalent:: record -> record -> boolean
;    x1 and x2 cannot "intersect", where 
;      intersection:: record -> record -> boolean
;    The "distance" between records identified as members of an equivalance 
;      class  is either 0 or (distance x1 x2) >= (minimum-distance x1 x2) > 0
;      where
;      distance :: record -> record -> float
;      minimum-distance:: record -> record -> float


;Special operators for our temporal records...
(defn record->segment
  "Convert a map into a vector of [start duration]"
  [r] [(:start r) (+ (:start r) (:duration r))])
(defn segment->record
  "Convers a vector of [start end] into a record of {start duration}."
  [seg]
  {:start (first seg) :duration (- (second seg) (first seg))})

;functions for dealing with abstract 1D line segments, as represented by vector
;pairs, where each entry in the vector is a point on a shared axis.
(defn overlap?
  "Determine if two segments, or 2 coordinate pairs, overlap."
  ([s1 e1 s2 e2] (and (<= s1 e2) (>= e1 s2)))
  ([seg1 seg2] (overlap? (first seg1) (second seg1) 
                         (first seg2) (second seg2))))
(defn segment-distance
  "Compute the distance between two segments, where overlapping segments have
   0 distance, and non-overlapping segments have distance equal to the minimum
   space between exterior points."
  [[s1 e1] [s2 e2]] 
  (if (overlap? s1 e1 s2 e2)   0
    (if (< s1 s2)
      (- s2 e1)
      (- s1 e2))))

(defn segment-encloses?
  "Returns true if segment1 encloses segment2, which means that segment2 lies 
   entirely inside of segment1, end-points inclusive."
  [[s1 e1] [s2 e2]]
  (and (<= s1 s2) (>= e1 e2)))

(defn clip-segment
  "Attempts to project target-segment onto base-segment, were regions of 
   target-segment outside of base-segment are truncated, or clipped, to the 
   bounds of the base-segment."
  [base-segment target-segment]
  (cond (segment-encloses? base-segment target-segment) target-segment
        (overlap? base-segment target-segment) 
          (let [[s1 e1] base-segment 
                [s2 e2] target-segment]
            [(min e1 (max s2 s1))
             (max s1 (min e1 e2))])
        :else nil))

;for records that contain a :start and :duration field, we can view them as 
;line segments on the temporal axis.
(defn record-distance [r1 r2] 
  (apply segment-distance (map record->segment) [r1 r2]))

(defn stretch-to [rec1 rec2]
  (assoc rec1 :duration (- (:start rec2) (:start rec1))))

(defn lengthen [length rec]
  (assoc rec :duration (+ (:duration rec) length)))

(defn scale [alpha rec]
  (assoc rec :duration (* (:duration rec) alpha)))

(defn group [group-key rs] 
  (map #(assoc % :group group-key) rs)) 

(defn end-time [r]
  (+ (:start r) (:duration r)))

(defn unbounded-segment? [s] 
  (= s [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]))

(defn translate
  "Shifts record along its temporal axis by delta."
  [delta rec]
  (assoc rec :start (- (:start rec) delta)))

(defn move-before
  "Translates record1 to occur directly before record2.  Record1 will not 
   overlap record2."
  ([rec1 rec2 separation]
    (assoc rec1 :start (- (:start rec2) (+ separation (:duration rec1)))))
  ([rec1 rec2]
    (move-before rec1 rec2 1)))

(defn lead
  "Translates record1 to end directly on the start of record2, so that record1
   overlaps record2 by 1 unit of time.  Treats record 2 as fixed, and returns 
   transformed version of record1.  Record1 is the 'precursor to record2."
  [rec1 rec2]
  (move-before rec1 rec2 0))
  
(defn move-after
  "Translates record2 to occur directly after record1.  Record2 will not overlap
   record1."
  ([rec1 rec2 separation] 
    (assoc rec2 :start (+ separation (:start rec1) (:duration rec1))))
  ([rec1 rec2] (move-after rec1 rec2 1)))

(defn follow 
  "Translates record2 to start directly on the end of record1, so that record2
   overlaps record1 by 1 unit of time.  Record2 is the 'successor to record2.
   Identical to precursor in function, except record1 is treated as fixed, 
   while record2 moves.  Returns a shifted value for record2.  Useful for 
   constructing 'trailing sets of events that must overlap by a single unit of 
   time."
  [rec1 rec2]
  (move-after rec1 rec2 0))

(defn subsume
  "Translates and scales record 1 to accomodate the time segment of both record
   one and record2."
  [rec1 rec2]
  (let [tstart (min (map :start [rec1 rec2]))
        tfinal (max (map end-time [rec1 rec2]))]
    (merge {:start tstart :duration (- tfinal tstart)}
           rec1)))

(defn subsume-by
  "Allows a user-supplied function to determine the order in which a set of 
   records should be merged.  f should be a function that returns a value 
   amenable to comparison, using clojure.core/compare."
  [f xs])

;some convenience operators....
;since we're likely dealing with tables...or record sets....it'd be nice 
;to have an operator for describing them...

;We define a little language for building abstract syntax trees that correspond
;to a computation that...for now....produces a vector of records.

;Now...there are operators in our language...
(defn following-recs [rs]
  (reduce (fn [acc x] 
            (conj acc (follow (last acc) x))) [(first rs)] (rest rs)))

(defn preceding-recs [rs]
  (reduce (fn [acc x] 
            (conj acc (lead x (last acc)))) [(first rs)] (rest rs)))

(defn chain
  "chain - takes a node list and creates a node that produces a list of nodes 
  that are chained together so that the end of one node is 'before the next.  
  Returns the concatenated, chained nodes, where chaining is defined by f.
  Default behavior ensures that any nodes in the list follow each otherso that 
  their start and end time (+ start duration) overlap."
  ([chain-func nodes]
    (fn [ctx] 
      (chain-func (map (fn [nd] (nd ctx)) nodes))))
  ([nodes] (chain following-recs nodes)))

(defn choice
  "choice - takes a node list, and creates a node that will randomly return 
   one of the nodes in the list.  Probability of selecting a node is even, 
   unless an alternate probabilities are provided."
  ([sample-func nodes]
    (fn [ctx] ((sample-func nodes) ctx)))
  ([nodes] (choice rand-nth nodes)))

(defn weighted-choice
  "Identical to choice, except it takes a map of node->probability densities.
   Where the keys are resolvable nodes, and the densities are the probabilities
   from [0 1], that a node will be chosen.  Densities must sum to 1.0 to be
   valid."
  [pdf-map]
  (assert (= 1.0 (reduce + (vals pdf-map))) 
          (str "Probability densities must sum to 1.0"))
  (let [nodes  (keys pdf-map) 
        choose (fn [] (loop [r (rand)
                             xs nodes
                             ds (vals pdf-map)]
                        (cond (= (count xs) 1)  (first xs)
                              (<= r (first ds)) (first xs)
                              :else (recur (- r (first ds)) 
                                           (rest xs) (rest ds)))))]
    (fn [ctx] ((choose) ctx))))

(defn concatenate
  "Takes an arbitray number of nodes, applies them to a context, and 
   concatenates the result into a vector."
  [nodes]
  (fn [ctx] (reduce conj [] (map (fn [f] (f ctx)) nodes))))

(defn replications
  "replicate - takes an positive integer, n, and performs n traversals of the 
   children, implicitly concatenating the results." 
  [n nodes]
  (let [rep (fn [ctx f]  (reduce conj [] (map (fn [i] (f ctx)) (range n))))]
    (fn [ctx] 
      (vec (map (partial rep ctx) (if (sequential? nodes) nodes [nodes]))))))

(defn transform
  "given a function, applies the function to each child, returning the
   result.  basically map."
  [f nodes]
  (fn [ctx] (vec (map (fn [nd] (f (nd ctx))) nodes))))

(defn merge-record
  "Given a record of values, merges the record with each node, which assumably
   resolves to a record expression."
  [rec nodes]
  (transform #(merge % rec) nodes))

(defn truncate-record
  "Uses the boundary set by base-record to truncate target-record.
   If target record can be fit into the bounds of base, a modified target-record
   is returned.  Otherwise, nil."
  [base-record target-record]
  (let [bounds (record->segment base-record)
        target (record->segment target-record)]
    (if-let [new-bounds (record->segment (clip-segment bounds target))]
      (merge target-record (segment->record new-bounds))
      nil)))

(defn with-constraints
  "Uses a pre-baked set of constraints, as provided by constraint-map, to filter
   the result of any generated nodes.  I'd like to change this, but for now 
   it'll be somewhat predefined constraints."
  [{:keys [tstart tfinal duration-max] :as constraint-map} nodes]
  (let [tstart       (or tstart Double/NEGATIVE_INFINITY)
        tfinal       (or tfinal Double/POSITIVE_INFINITY)
        duration-max (or duration-max Double/POSITIVE_INFINITY)
        global-bounds [tstart tfinal]
        constrain  (if (unbounded-segment?  global-bounds)
                          identity
                          #(map (partial truncate-record global-bounds) %))]
    (fn [ctx] 
      (->> (flatten (nodes ctx))
           (constrain) 
           (filter (complement nil?))))))    

(defn merge-context [rec nodes]
  (fn [ctx] (nodes (merge rec ctx)))) 

(defn sample-context [ctx nodes] (nodes ctx))
(defn compose [n1 n2]
  (fn [ctx] (n2 (n1 ctx))))

;This is a slight break with the original version, in that I've developed a 
;simple little-language to describe the phenomenon (actually many phenomena).

;These are our primitive nodes....
;If we don't have a set of primitive nodes, we need a way to generate them.
(def p1 {:bar1 {:name "bar1" :start 10 :duration 1}
         :bar2 {:name "bar2" :start 11 :duration 10}
         :bar3 {:name "bar3" :start 21 :duration 5}
         :bill {:name "bill" :start 30 :duration 30}
         :cat  {:name "cat" :start 2  :duration 1}
         :qux  {:name "qux" :start 50 :duration 1000}})
;let's create a set of grouped nodes...

;Rules for composing primitive nodes, which in turn create new nodes.
(def p2 {:bar {:chain  [:bar1 :bar2 :bar3]}
         :baz {:choice [:bill 
                        {:transform [{:start (stats/exponential-dist 10)
                                      :duration (stats/exponential-dist 2000)}
                                     :cat]}]}
         :foo {:transform [{:start (stats/normal-dist 10 1)}
                           {:choice {:bar 1/3
                                     :baz 1/3
                                     :qux 1/3}}]}})
;Rules for composing previous nodes into a case node.
(def p3 {:case1 {:concat [{:replications [2  [:foo]]}
                          {:replications [10 [:qux]]}
                          {:replications [3  [:baz]]}]}})

;Rules for composing everything into a sample. 
(def p4 {:sample {:replications [3 [{:constrain [{:tfinal 5000 
                                                  :duration-max 5000}
                                                  :case1]}]]}})

(defn node-type [nd] (get nd :node-type))
(defn node-data [nd] (get nd :node-data) nd)


(defn ->node         [type data] {:node-type type :node-data data})
(defn ->group        [node-seq] (->node :group {:children node-seq})) 
(defn ->leaf         [data]     (->node :leaf data))
(defn ->chain        [nodes]    (->node :chain {:children nodes}))
(defn ->replications [n nodes]  (->node :replications {:reps n 
                                                       :children nodes}))
(defn ->choice       [nodes]    (->node :choice {:children nodes}))
(defn ->transform    [f nodes]  (->node :transform 
                                  {:f (if (map? f) #(merge % f) f) 
                                   :children nodes})) 
(defn ->concatenate  [nodes]    (->node :concatenate {:children nodes}))
(defn ->constrain    [constraints nodes] 
  (->node :constrain {:constraints constraints 
                      :children nodes}))

;nodes take the form {:node-type some-type :node-data  some-data}
(def sampler1 
  (->node :start 
    (->replications 3 (->leaf :bar1))))
  
(def sampler2 
  (->node :start 
    (->replications 3
       (->choice  [:bar1
                   :bar2
                   :bar3]))))

(def nested-choices 
  (->choice  [(->choice [:qux :cat :bill])
              :bar2
              :bar3]))

(def weighted-choices
  (->choice  {:qux 0.25 
              :cat 0.25
              :bill 0.5}))
(def sampler3 
  (->node :start 
    (->replications 3
       (->choice  [(->choice [:bill :cat :qux])
                   :bar2
                   :bar3]))))

(defn forever [x] (fn [ctx] x))
(defn as-behaviors [xs] 
  (into [] (map (fn [x] (if (or (keyword? x) (fn? x)) x (forever x))))))

(defn render-children [xs] 
  (into [] (map (fn [x] (cond (or (keyword? x) (fn? x)) x 
                          (fn [ctx] (render-node x ctx)))) xs)))

(defmulti  render-node
  "A generic method for traversing a sample-tree, collecting records at each 
   step."
  (fn [node ctx] (node-type node)))

(defmethod render-node :default  [node ctx] (if-let [remaining (node-data node)]
                                              (render-node remaining ctx)
                                              node))
(defmethod render-node :leaf     [node ctx] (get ctx (node-data node)))
(defmethod render-node :chain    [node ctx] ((chain (node-data node)) ctx))
(defmethod render-node :choice   [node ctx]
  (let [data (:children (node-data node))]
    (cond (map? data) ((weighted-choice data) ctx)
          :else ((choice (render-children data)) ctx))))

(defmethod render-node :transform [node ctx]
  (let [{:keys [f children]} (node-data node)]
      ((transform f  children) ctx)))

(defmethod render-node :replications [node ctx]
  (let [{:keys [reps children]} (node-data node)]
    ((replications reps (fn [ctx] (render-node children ctx))) ctx)))

(defmethod render-node :concatenate [node ctx]
  ((concatenate (node-data node)) ctx))
;
;(defn render-graph [sample-graph root-node]
;  (let [nd (get root-node sample-graph)]
;    (render-node root-node 

;(defmethod render-node :constraint [_ node-data ctx]
;  (let [[constraints nodes] node-data]
;    (
    
;(def ops {:chain 
;          :choice 
;          :weighted-choice 
;          :transform 
;          :replications 
;          :concat
;          :constrain })

(def sample-graph (merge p1 p2 p3 p4))
(defn walk-samplegraph
  "Walks a set of nodes representing a sampling graph, starting at the 
   root node.  Evalutes each node of the sample graph in the context of the 
   graph as a whole.  Returns a sequence of results from the walk, typically 
   records."
  [graph-ctx root]
  
)


(comment ;testing 
;sample from a pdf...
(def simple-graph {:foo {:name "Foo!"}
                   :bar {:name "Bar!"}
                   :baz {:name "Baz!"}})
(def simple-nodes (keys simple-graph))

(def choices (weighted-choice {:foo 0.25 :bar 0.50 :baz 0.25}))
(def even-choices (choice (keys simple-graph)))
(def samples (frequencies (take 1000 (repeatedly #(choices simple-graph)))))
;=>{{:name "Bar!"} 514, {:name "Baz!"} 242, {:name "Foo!"} 244}
(def even-samples 
  (frequencies (take 1000 (repeatedly #(even-choices simple-graph)))))
;=>{{:name "Bar!"} 322, {:name "Baz!"} 347, {:name "Foo!"} 331}

(defn time-stamp [nodes] 
  (merge-record {:time (System/currentTimeMillis)} nodes))

(def timed-result (->> simple-graph 
                    ((add-time simple-nodes))))

(def multiple-results (->> (choice [:foo :bar])
                        (compose (time-stamp simple-nodes))
                        (replications 2)                                                                                
                        (sample-context simple-graph)))
                                            
)



;constrain - modifies the contextual constraints applied when generating node
;records.  We can probably replace this with filter.

;each of our operators can be serialized...

;fyi  we can use a similar rule-base for determining a validation network.

;Whenever we sample records...we can pass the samples through a validation 
;network, composed of simple rules, that ensures the sample maintains some 
;invariant...

;the current case is a form of "collision" that can occur between samples.
;Basically, we can define collision rules, and collision responses.
;The collision rules will match on every record, and determine if they should
;run...
;Notice that a collision requires context, specifically a relation between 
;one or more records in the sample.  The typical context is a temporal 
;intersection, but there could be other contexts.

;The original concrete algorithm attempted to insert records onto a timeline, 
;then scanned the timeline for collisions to affirm validity.
;Collisions could only occur for certain classes of records, defined by 
;demand classes, which related records from the population. 

;It occurs to me that when we're computing "overlap", we're really just 
;performing a prejudiced XOR on a subset of the records....specifically 
;records that may "overlap", where overlap occurs temporally.
;We detect temporal overlaps by 

;legacy implementation

;This is on hold temporarily....probably punt off on Dwight!
;A population of records from which to sample.
(def example-population 
  [{:group "LandPrey" :start 1 :duration 1000 :SRC "Donkey"  :quantity 2}
   {:group "LandPrey" :start 1 :duration 1000 :SRC "Rabbit"  :quantity 5}
   {:group "LandPrey" :start 1 :duration 1000 :SRC "Mouse"   :quantity 20}
   {:group "AirPrey"  :start 1 :duration 150  :SRC "Sparrow" :quantity 10}
   {:group "AirPrey"  :start 1 :duration 150  :SRC "Finch"   :quantity 30}
   {:group "WaterPrey" :start 1 :duration 1000 :SRC "Minnow" :quantity 100}
   {:group "LandPred" :start 1 :duration 300  :SRC "Fox"     :quantity 1}
   {:group "LandPred" :start 1 :duration 300  :SRC "Wolf"    :quantity 5}
   {:group "AirPred"  :start 1 :duration 500  :SRC "Hawk"    :quantity 15}
   {:group "AirPred"  :start 1 :duration 500  :SRC "Eagle"   :quantity 7}
   {:group "WaterPred" :start 1 :duration 2000 :SRC "Shark"  :quantity 1}])

;Records that describe sampling functions. 
(def legacy-generator-records 
  [{:type "RandomPrey" :frequency 10 
    :StartDistribution "Uniform"     :Start1 0 :Start2 4670 :Start3 0 
    :DurationDistribution "Vignette" :Duration1 0 :Duration2 0 :Duration3 0
    :pool ["LandPrey" 0.33 "AirPrey" 0.33 "WaterPrey" 0.33]}
   {:type "RandomPredator" :frequency 5 
    :StartDistribution "Uniform"     :Start1 0 :Start2 4670 :Start3 0 
    :DurationDistribution "Vignette" :Duration1 0 :Duration2 0 :Duration3 0
    :pool ["LandPred" 0.33 "AirPred" 0.33 "WaterPred" 0.33]}
   {:type "RandomWaterPred"  :StartDistribution "Uniform" :Start1 0 :Start2 4670 :Start3 0 
    :DurationDistribution "Vignette" :Duration1 0 :Duration2 0 :Duration3 0
    :pool ["WaterPred" 1.0]}
   {:type "RandomLandPred"  :StartDistribution "Uniform" :Start1 0 :Start2 4670 :Start3 0 
    :DurationDistribution "Vignette" :Duration1 0 :Duration2 0 :Duration3 0
    :pool ["LandPred" 1.0]}
   {:type "RandomAirPred"  :StartDistribution "Uniform" :Start1 0 :Start2 4670 :Start3 0 
    :DurationDistribution "Vignette" :Duration1 0 :Duration2 0 :Duration3 0
    :pool ["AirPred" 1.0]}])

(defn legacy-distribution
  "A patch to allow us to parse old data for defining distributions.
   Returns a map of field to field value, where field-value is a function 
   applied to a record's original field value.  The resulting value is returned
   as a modified field value, to be merged with the record."
  [name & args]
  (case (keyword (clojure.string/lower-case name))
    :uniform (let [g (stats/uniform-dist (first args) (second args))]
               (fn [_](g)))
    :vignette identity 
    :constant identity    
    :fixed    (fn [x] (first args))
    (throw (Exception. "Unknown distribution!"))))

(defn legacy-record? [r] (contains? r :StartDistribution))
(defn derive-distributions
  "A simple patch to allow us to pull in old data."
  [r]
  {:start 
     (apply legacy-distribution 
            (juxt [:StartDistribution :Start1 :Start2 :Start3] r))
   :duration 
     (apply legacy-distribution 
          (juxt [:DurationDistribution :Duration1 :Duration2 :Duration3] r))}) 

(defn legacy-record->generator-spec
  "Turns a legacy record into something that conforms to a valid generator.  
   We typically only had start and duration in the legacy spec."  
  [r]
  (merge r {:computed-fields (derive-distributions r)}))  

;(defn record->generator [r population]
;  (let [spec (if (legacy-record? r) 
;               (legacy-record->generator-spec r)
;               r)]
;    (fn [

;(defn record->run-spec [r])
  
   
;records that describe dependencies that must be enforced for any sampled 
;population.  note...the implicit assumption is that trigger and triggered are
;primary are keys generated from -an un-named field - :group.  This is a 
;weakness in the design, and is brittle.  we should make it more robust.
;There is also a problem here with the separation of concerns.....specifically, 
;we encode a second layer of dependencies between the dependencies with the 
;notion of 'priorities' and 'min-inter-arrival times.....what we really want 
;is a second set of data that indicates mutual exclusions....
(def demo-dependency-records 
  [{:trigger "AirPrey"  :triggered "AirPred"   
    :priority 2 :offset 2 :min-inter-arrival 10}
   {:trigger "LandPrey"  :triggered "LandPred"  
    :priority 1 :offset 7 :min-inter-arrival 30}
   {:trigger "LandPrey"  :triggered "AirPred"   
    :priority 1 :offset 7 :min-inter-arrival 10}
   {:trigger "WaterPrey" :triggered "WaterPred" 
    :priority 1 :offset 1 :min-interarrival  90}
   {:trigger "WaterPrey" :triggered "AirPred" 
    :priority 1 :offset 1 :min-interarrival  10}])

(defn get-dependent-set
  "Returns a hash-set of the records that must be checked for collisions due
   to dependencies.  All other records are assumed independent."
  [dependency-records]
  (into #{} (map :triggered dependency-records)))

;dependencies are constraints...that enforce the existence of one or more 
;records offset from an original record.
;distance constraints enforce the merging of records that are members of an 
;equivalence class.  Equivalent records that overlap are merged according to 
;the highest priority.  Equivalent records that are "close", <= the min 
;inter-arrival time, are merged according to a weld function.  The default 
;behavior is to stretch the duration of the earlier record to the start of 
;the second record. 
  

(defn overlap-records [rec1 rec2])

;The original algorithm implied that dependent events could be merged or 
;subsumed if multiple dependent events were found to collide, where a collision
;occured if two 'dependent events overlapped, where dependent events are 
;the set of reachable 'triggered events from the dependency records.
;these records form an implicit equivalence class....

(defn field-equal? [field v m] (= (get  m field) v))

(defn default-merge 
  [inherited-fields rec-from rec-to]
  (into rec-to (for [fld inherited-fields]
                 [fld (get rec-from fld)])))

(defn dependency-injector
  "A dependency injector, for records, provides a function that maps a record to
   a sequence of dependent records, where dependent records are drawn from a 
   population of records.  Dependent-records share the same value of key-field
   with the input record.  If inherited fields are specified, the dependent 
   records will assume the field values of the input record."
  [key-field inherited-fields dependency population 
   & {:keys [merge-func] :or {merge-func (partial default-merge 
                                                  inherited-fields)}}]
  (let [dependent-recs (filter (partial field-equal? key-field 
                                        (get dependency key-field)) population)]        
    (fn [record] (map (partial merge-func record) dependent-recs))))











