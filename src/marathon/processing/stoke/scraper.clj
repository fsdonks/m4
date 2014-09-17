;;This should be renamed.  Currently exists in futures.clj
;;Currently a draft namespace for the libaries that extend the 
;;simple stoke processor from marathon.processing.stoke.core .
;;As it turns out, there are a couple of business rules that 
;;drastically increase the complexity of the problem specification
;;even for the beer-math version of stoke.  As a consequence, 
;;I have opted to use a general mincost network flow optimization
;;to generate constrained supply solutions in accordance with 
;;the mess of business rules we're using.  The problem reduces
;;to reading, from a stoke case file, enough information to 
;;build a network that embodies the supply constraints (and 
;;possible goals or objectives).  Given such a network, our 
;;simple hierarchical fill algorithm, from the original stoke, 
;;goes about filling a set of peak demand records in the a 
;;similar way.
(ns marathon.processing.stoke.scraper
  (:require [marathon.processing.stoke.core :refer :all]
            [marathon.processing.stoke [io :as io]]
;            [marathon.processing.stoke [arrayflow :as aflow]] ;;pending
            [spork.cljgraph [core :as graph]
                            [flow :as flow]]
            [spork.util     [table :as tbl]]
            [spork.cljgui.components [swing :as gui]]))

;; (def ^:dynamic *flow-options* 
;;   (merge default-flow-opts      
;;          {:get-sources 'spork.cljgraph.flow/-flow-sources, 
;;           :get-sinks 'spork.cljgraph.flow/-flow-sinks, 
;;           :weightf 'spork.cljgraph.flow/-flow-weight, 
;;           :get-edge 'spork.cljgraph.flow/-edge-info, 
;;           :get-direction 'spork.cljgraph.flow/-get-direction}))

;;Flow Scaling
;;============
;;THis is a fucking ugly hack.  I need to seriously erase it.  We
;;can't flow in parallel with this...
(def current-scale (atom 1))
(definline set-scale! [n] `(reset! current-scale ~n))
(def scaled-mincost-flow (flow/->scaled-mincost-flow (flow/->variable-scaled-flow current-scale)))

;; (def scaled-mincost-flow 
;;   (flow/with-altered-flow 
;;     (fn [f] (flow/scale flow-scale   f)) 
;;     (fn [f] (flow/unscale flow-scale f)) (eval (flow/flow-fn flow/*flow-options*))))

;;Allows us to use a global flow scaling, and to quickly switch out scales.
(defmacro with-flow-scaling [qty & body]
  `(let [old# (deref current-scale)
         ~'_    (set-scale! ~qty)
         res#  ~@body]
     (do (set-scale! old#)
         res#)))
         
;;END PATCHES
;;===========         

(defn missing-strength [src] (Exception. (str "No strength for src " src)))

;;Rewrite
;;=======
;;Due to complexities in generating "optimal" force structure relative
;;to end-strength, parent-child, and other competing objectives, I
;;have decided to use the existing hierarchical fill function as a
;;primitive value function that forms a sub-goal for a demand-driven
;;supply generator.  The supply generator will use local search to
;;improve upon an initial solution.  Its objective is to find a
;;Supply that minimizes the following objectives: 

;;Strength  Under Budget - as defined by Total End Strength
;;Strength  Over Budget  - as defined by an End Strength Constraint Tree 
;;Orphaned  Strength     - as defined by a Parent-Child forest. 
;;Fill Cost              - as defined by a Fill Network.

;;Thus, the goal of our supply generator is to find a supply, x,  that 
;;minimizes the function:  
;;  z(x) = a*SUB(x) + b*SOB(x) + c*OS(x) + d*FillCost(x)
;;Where a,b,c,d = goal weights

;;From a high level view, Fill Cost is the mincost maxflow occuring 
;;when we solve the FillNetwork resulting from the function 
;;(make-fill-net demand s)
;;applied to a demand future, and an empty Supply solution s.

;;Current defaults to the strength built into the supply solution.
(defn strength-under-budget [supply] 
  (surplus-strength supply))

;;We have a tree of end strength constraints that link together
;;dependent pools.  For each [src,pool] key within the supply, 
;;we can accumulate the total for the pool.  Tracing the 
;;parents of each pool, we can store a quantity in each supply 
;;pool node (in the graph) that indicates the amount of supply 
;;present related to the nodes on the pool path.  Initial values
;;for the nodes start at the capacities for the nodes, and 
;;we simply decrement the strength associated with the node.  We
;;continue decrementing up to the root (Total).

(defn pool-capacities 
  [es-graph root]  
  (if-let [kids (graph/sinks es-graph root)]
    (let [outflows (for [x kids]
                     [x (flow/current-capacity es-graph root x)])]
      (reduce concat outflows (map (partial pool-capacities es-graph) kids)))))

;;Builds a map of initial node capacities relative to current
;;capacities in network g.  Assumes root node is 'Total  (change that
;;in future!)
(defn pool-map [g]  (into {'Total 0} (pool-capacities g 'Total))) 
  
;;Given a map of pool capacities, any negative values (less the Total
;;node) indicate overages and are combined and sign-flipped.
(defn pool-overages [pool-capacities] 
  (reduce (fn [acc [k v]] (if (< v 0) (- acc v) acc)) 0 (dissoc pool-capacities 'Total)))
        
;;As we run out of capacities at each node, we'll venture into 
;;negative numbers.  The total over-budget cost is the absolute 
;;value of the negative node values for each pool in the es-graph.
;;This represents over-capacitated nodes, and allows us to capture 
;;piece-wise budget constraint violations.
(defn strength-over-budget 
  ([pool-capacities es-graph s]
     (let [{:keys [src->strength supply]} s]
       (pool-overages 
        (reduce (fn [m k] 
                  (let [src   (first k)
                        pool  (second k)                
                        used (* (src->strength src) (get supply k))]
                    (if (> used 0)
                      (reduce (fn [acc p] (update-in acc [p] - used))
                              m   (conj (graph/preds es-graph pool) pool))  
                      m)))
                pool-capacities
                (keys supply)))))
  ([es-graph s] (strength-over-budget (pool-map es-graph) es-graph s)))

;;Orphan Detection
;;================
;;Orphans are elements of supply that are eligible to be members of 
;;larger group force structure, but do not evenly divide.  They are
;;the "remainders" of grouped structure.
;;Orphans are defined relative to a set of parent-child relations.
(defn build-groups [supply-groups] 
  (let [group-strength 
        (into {}
              (for [[group xs] supply-groups]
                [group [(reduce + (map #(* (:Quantity %) (:Strength %)) xs))
                        (map :Child xs)]]))
        raw (reduce (fn [g {:keys [Parent Child Quantity Strength]}]              
                      (-> g 
                          (graph/conj-arc Parent Child Quantity)
                          (graph/set-node Child Strength)))
                    graph/empty-graph
                    (reduce concat (vals supply-groups)))]
    (-> (reduce (fn [g [group [str _]]] (graph/set-node g group str)) 
                raw group-strength)
        (assoc :groups group-strength))))  

;;Given a set of supply groups, and a supply, we can find orphans.
(defn add-groups [proj]
  (assoc proj :Groups (build-groups (:SupplyGroups proj))))

(defn ordered-groups [groups]
  (for [[g xs] (sort-by #(- (first (second %))) groups)]
    [g (second xs)]))

;;Given a group distribution, and a supply of elements in a group, 
;;returns the number of whole groups that can be made, along with 
;;the remaining atomic supply.
(defn pack-group [group-dist group-supply]
  (let [max-whole (reduce min (for [k (keys group-dist)]
                                (quot (get group-supply k 0) (get group-dist k))))]
    (if (zero? max-whole) 
      [0 group-supply]
      (let [remainder (reduce (fn [m k] 
                                (update-in m [k] - (* (get group-dist k) max-whole)))
                              group-supply (keys group-dist))]
        [max-whole remainder]))))

;;We can get a decent approximation of grouping by packing everything
;;into groups greedily, and seeing how much structure is in the
;;"ungrouped" group.  This is probably a more primitive function that 
;;we should put into spork.
(defn group-eligible-supply [groups supply]
  (->> (active-supply supply)
       (group-by (fn [[k v]]               
                   (if (contains? (graph/nodes groups) (first k))
                     (first k)
                     :ungrouped)))))

(defn supply-records->supply-map [xs]
  (reduce (fn [m [[src _] qty]]
            (assoc m src (+ (get m src 0) qty)))
          {}
          xs))

;;given an entire set of supply, we can get a sense of the ungrouped 
;;structure by trying to group all groupable supply.  Anything
;;remaining is added to inorganic, or ungrouped structure.
(defn supply-grouper 
  [groups]
  (let [group-dists 
              (into {}      
                    (for [group (keys (:groups groups))]
                      [group 
                       (into {} (for [child (graph/sinks groups group)]
                                  [child (graph/arc-weight groups group child)]))]))]
    (fn [s]
      (let [gsupply    (group-eligible-supply groups s)
            ungrouped  (:ungrouped gsupply)
            groupable-supply-map 
                       (reduce merge 
                               (for [[group xs] (dissoc gsupply :ungrouped)]
                                 (supply-records->supply-map xs)))]
        (reduce (fn [acc [group dist]] 
                  (let [[qty remainder] (pack-group dist acc)]
                    (merge acc remainder)))
                groupable-supply-map
                group-dists)))))  

(defn compute-strength 
  [src->strength supply-map]
  (reduce + (map (fn [[k v]] (* (src->strength k) v)) supply-map)))

(defn orphan-scorer [groups]
  (let [get-orphans (supply-grouper groups)]
    (fn [s]
      (compute-strength (:src->strength s) (get-orphans s)))))

;;testing
(comment
(def the-group ["77300R600" [4435.0 ("77302R500" "06235R000" "07215R000" "17215R000" "05315R600" "63035R000")]])
(def group-arcs 
  [["77300R600" "77302R500" 1.0] 
   ["77300R600" "06235R000" 1.0] 
   ["77300R600" "07215R000" 3.0] 
   ["77300R600" "17215R000" 1.0] 
   ["77300R600" "05315R600" 1.0] 
   ["77300R600" "63035R000" 1.0]])

(def sample-dist
  {"77302R500" 1.0 
   "06235R000" 1.0 
   "07215R000" 3.0 
   "17215R000" 1.0 
   "05315R600" 1.0 
   "63035R000" 1.0})

(def sample-supply
  {["77302R500" 'AC] 100
   ["06235R000" 'AC] 100 
   ["07215R000" 'AC] 50 
   ["17215R000" 'AC] 100
   ["05315R600" 'AC] 100 
   ["63035R000" 'AC] 100})

(def available-supply 
  (->> sample-supply
       (group-by ffirst)
       (map (fn [[k xs]] [k (reduce + (map second xs))]))
       (into {})))

(def packed (pack-group sample-dist available-supply))
(def kids (map second group-arcs))
(def altered-supply (reduce (fn [acc [[src compo] qty]] 
                              (add-supply acc src compo qty)) s sample-supply))
)
           

;;Filling Optimally
;;=================
;;We build a demand-driven supply-generator that creates supply by
;;optimally investing constrained resources relative to a demand
;;future.  

;;Our basic mechanism for expressing both constraints and objectives 
;;will be a fairly sophisticated network.  Our intent is that
;;computing the mincost flow across the network will result in a flow
;;of resources that describes the optimal investment of force
;;structure relative to preferences embedded in the topology of the
;;network.

;;We'll then discuss two different ways to "solve" the network flow
;;problem, either hierarchically (filling from the most important
;;sinks to the least important) or simultaneosly.

;;Network Topology
;;================
;;Any mincost flow optimization is relatively simple in formulation:

;;Where G=(N,A)  a directed network with n nodes and m arcs (n x m
;;matrix), defined by a set of nodes N, and a set of arcs A
;;Each arc within A has cost Cij, capacity Uij, lower bound Lij on 
;;arc flow, 

;;Cmax = Infinity 
;;a map of flow constraints b,  where
;;b(i) = supply for node i if b(i)>0, else demand if b(i)<0 

;;minimize the function Z(X) = sum(ij)(Cij*Xij), for all i,j within A
;;Such That 
;;sum(Xij)      -       sum(Xji) = b(i) ;Constrained flow conservation
;;for all i,j within A, for all j,i within A

;;Given such a network, we can solve Z using a number of mincost flow
;;routines, namely the mincost flow solver in spork.cljgraph.flow 

;;Our goal is to define the elements that are reified into either 
;;capacitated arcs (possible flows), supply nodes, demand nodes, and 
;;arc costs.  In other words, the bulk of our effort is spent building 
;;the network from some external data.

;;In general, we can think of the desired network G, or the Fill
;;Network, as the composition of two smaller networks: 

;;Supply Dependencies
;;===================
;;The rest of the supply network encodes so-called existential, or organic
;;dependencies between elements of supply.  For many types of supply,
;;supply is preferably produced in conjunction with an organic "whole"
;;unit.  An element of supply may be referenced in a demand, but the
;;actual capability may come from a larger "bundle" resident in an
;;organic unit.  Consequently, an optimal fill should account for the
;;fact that elements of supply are existentially or organically
;;related via a tree of parent-child dependencies.  Thus, flow is more
;;efficiently obtained by investing in parent structure 

;;A dependency tree - a set of arcs between parent,child SRCs, where
;;weights are child quantities

;;A map of demand to inorganic cost - or - preferably a single cost
;;Co, which is typically IntMax.

;;A map of demand to priority 
;;A map of SRC to Strength
;;A map of FillRule to demand preference

;;A preference tree - a set of of arcs between parent, child
;;FillRules, which represents a demand's preferred supply.

;;patched...

;;Building a supply-network
;;=========================
(defn register-constraints 
  "Given a graph and a map of pools, registers capacitated arcs 
   that indicate the maximum flow between supply rules in the graph."
  [g constraints]
  (let [add-child (fn [parent acc [child capacity]]
                      (flow/-conj-cap-arc acc parent child 0 capacity))]
    (reduce (fn [acc [parent child-map]]
              (reduce (partial add-child parent) acc child-map))
            g constraints)))

;;composes a supply network
(defn build-supply-network [constraints]
  (register-constraints flow/empty-network  constraints))  

;;Conjoins a supply network onto the project resources.
(defn add-supply-network [proj]
  (assoc proj  :SupplyNetwork (build-supply-network 
                               (:StrengthConstraints proj))))      

;;__Note:__ We're storing SRCs as strings.  It'd be a nice optimization to cache them
;;in a symbol table, and then coerce them back to strings after the fact.

;;Compute an empty initial supply dimensioned by the components and srcs provided in 
;;the stoke project data.
(defn build-initial-supply [proj]
  (let [src->strength (:SRCs proj)  
        compos        (:Components proj)
        caseinfo      (:Case proj)]
    (->supply-solution (set (keys src->strength)) compos 
                       (:MaxEndStrength caseinfo)  src->strength)))

;;Building a fill network for a demand.
;;=====================================


;;Update: 
;;We were incorrectly applying the filters read in from SupplyNode 
;;filters, and effectively reversing the intent of the filter.
;;The only filters currently allowed are exclusionary filters, 
;;pre-fixed by :not in the data.  The filter should function, 
;;semantically, like filter in clojure, i.e. only src / rule 
;;pairs that are valid should pass the filter.  
;;For the cabs, we had a [:not RCAD] filter to eval.  I 
;;added an extra not inside of the the valid? local function 
;;definition, which caused us to select ONLY on the RCAD 
;;and exclude everything else.  In fact, we wanted to exclude 
;;RCAD and allow all others.  This has been fixed by removing 
;;the (not ...) and allowing the filter to act normally. 
;;I probably need to spend a little more time fleshing out the 
;;semantics of the filter, but for now it's okay.

;;generate a valid set of arcs the designate rule preferences 
;;for a demand for an src.

;;Patched to allow default rules.
(defn rules->arcs [src rules filter-map]
  (let [valid?  (cond (contains? filter-map src)
                         (fn [rule]
                           (some (fn [f] (f src rule)) 
                                 (get filter-map src)))
                      (contains? filter-map :default)
                         (fn [rule]
                           (some (fn [f] (f src rule)) 
                                 (get filter-map :default)))
                      :otherwise  identity)]
    (loop [rs (filter valid? rules)
           idx 0
           arcs []] 
      (if (empty? rs) arcs
          (recur (rest rs) (inc idx) 
                 (conj arcs [(first rs) src idx flow/posinf]))))))

;;the filled arc unique to this src
(defn filled-arc [src quantity] [src :filled 0 quantity])  

;;Note
;;====
;;We now incorporate potentital upper bounds by SRC by 
;;modifying the quantity arg for demand-arcs.  
;;Given a supplemental bit of information, we define the 
;;quantity demanded, i.e. the capacity from 'demand 
;;to :filled, to be the minimum of the quantity demanded * str 
;;in a demand record, and the upper-bound for the SRC.

;;Add a node to the network, creating capacitated arcs 
;;relative to the demand, the rules, and the quantity. 
;;Append a capacitated fill-node to the demand node, to 
;;enforce flow constraints.
(defn demand-arcs [rules filter-map src quantity]
  (conj (rules->arcs src rules filter-map) 
        (filled-arc src quantity)))
            
;;__This is a hack.__  Needs to go into cljgraph.flow as part of 
;;the library.
(defn drop-edge 
  [flow-info from to] 
  (let [res (dissoc (get flow-info from) to)]
    (if (empty? res) 
      (dissoc flow-info from) 
      (assoc flow-info  from res))))


(defn drop-demand-arcs [net src arcs]
  (-> (graph/drop-nodes net [src :filled])
      (assoc  :flow-info
        (reduce (fn [flow-info arc-info] (drop-edge flow-info (first arc-info) (second arc-info)))
                (:flow-info net) arcs))))

;;patched to use the new API
;;=======
(defn zero-flow [net]
  (reduce (fn [acc ^spork.cljgraph.flow.einfo e] 
            (if (== (.flow e) 0) acc 
                (flow/-set-edge acc (flow/set-flow e  0))))
          net 
          (flow/get-edge-infos net)))


(def netstate (atom nil))

(def errstate (atom nil))

;;Given a supply network, consisting of end-strength-constrained, 
;;related nodes, indicated by capacitated arcs, and a demand order, 
;;return the mincost flow into the demand order using the supply 
;;network.  Returns a sequence of [rule src] fills, and the amount 
;;of flow used to fill.
(defn net-fill! [supply-net filter-map preferences src rule quantity flow-func]
  (let [rules     (get preferences rule) ;notice this is a VECTOR!
        sources   (set rules) ;we recompute this every TIME.  Can we cache?
        new-arcs  (demand-arcs rules filter-map src quantity)
        supply    (flow/conj-cap-arcs supply-net new-arcs)               
        ;;this is currently our bottleneck.
        flow-result (try (flow-func (flow/transient-network supply) 'Total :filled)                      
                         (catch Exception e 
                           (do (reset! errstate {:supply supply :src src :quantity quantity :scale @current-scale})                               
                               (throw (Exception. (str "Error happened in flow: " (.getMessage e)))))))
        fills     (->> (:active flow-result)
                       (filter  #(identical? (second (first %)) src))
                       (map (fn [res] 
                              (let [fromto (first res)
                                    flow   (second res)]
                                {:src src
                                 :compo (first fromto)
                                 :quantity flow}))))
        net        (flow/persistent-network! (:net flow-result))
        total-flow (flow/total-flow net (:active flow-result))]    
    (when fills 
      {:fills fills 
       :net (zero-flow (drop-demand-arcs net src new-arcs))
       :total-flow total-flow    
       :flow-cost  (flow/total-cost net (:active flow-result))
       :filled? (== total-flow quantity)})))


;;Temporarily unpatched, reverting to / .
;;patched apply-fills, was using / rather than quot.
;;also replaced > 0 with pos?
(defn  apply-fills 
  [fills s] 
  (let [src->strength (:src->strength s)
        get-quantity  (fn [src str] 
                        (if-let [per-unit (src->strength src)]
                          (/ str per-unit)
;                          (quot str per-unit)
                          (missing-strength src)))]
    (reduce (fn [acc fill]
              (let [src   (:src fill)
                    compo (:compo fill)
                    qty   (get-quantity src (:quantity fill))]
                (if (pos? qty) (add-supply acc src compo qty) acc))) 
              s fills)))


;;We may not need a separate function for this guy...
;;Identical to apply-fills, but lets user provide a hook that's
;;called to update some external bounds.
(defn  apply-bounded-fills 
  [fills s update-bounds!] 
  (let [src->strength (:src->strength s)
        get-quantity  (fn [src str] 
                        (if-let [per-unit (src->strength src)]
                          (/ str per-unit)
;                          (quot str per-unit)
                          (missing-strength src)))]
    (reduce (fn [acc fill]
              (let [src   (:src fill)
                    compo (:compo fill)
                    qty   (get-quantity src (:quantity fill))
                    ]
                (if (pos? qty) (do (update-bounds! src qty)
                                   (add-supply acc src compo qty)) 
                    acc))) 
              s fills)))


(def ^:dynamic *debug* nil)
(defn log! [msg] 
  (when *debug* (println msg)))

;;A seperate function for ordering demands.  Should return an 
;;ordered set of demands, ordered by priority (ascending), then 
;;strength (descending).  We incorporate groups into our sorting
;;function, preferring to fill demands that are intended to be
;;organically group.  The grouping mechanism works like this: 
;;We map two extra fields onto the demands: SRCGroup GroupStrength 
;;If an src is not a member of a group, SRCGRoup = SRC, GroupStrength
;;= Strength, otherwise we look it up.  
(defn sort-demands [demand->priority demand->src src->strength src->group group->strength xs]
  (let [sortkey (fn [r] (let [src  (demand->src r)
                              g    (or (src->group src) src)
                              gstr (or (group->strength g) (src->strength src))]
                          [(demand->priority r) (- gstr) g]))]
    (sort-by sortkey xs)))

(defn  sort-demands! [demand->priority demand->src src->strength src->group group->strength xs]
  (->> (object-array xs)
       (sort-by demand->priority)
       (sort-by (fn [r] (let [src (demand->src r) g (or (src->group src) src)] 
                          (- (or (group->strength g) (src->strength src))))))
       (sort-by (fn [r] (let [src (demand->src r)] (or (src->group src) src))))))
  
;; (defn demand-key [r]
;;   (let [src  (demand->src r)
;;         g    (or (src->group src) src)
;;         gstr (or (group->strength g) (src->strength src))]
;;     [(demand->priority r) (- gstr) g]))

;debugging information for the network flow stuff.
(def last-net (atom nil))
(def state (atom nil))

(defn increment-upstream-capacity 
  "Traverses the parents of node, and increments the capacity by amt.  Edges with 
   infinite capacity are left untouched.  Edges with 0 capacity cannot be decremented 
   below 0.  Only visits predecessor edges once."
  [net node amt]
   (flow/edge-map-from net node 
    (fn [e] 
      (let [cap (flow/edge-capacity e)]
        (if (== cap flow/posinf) e
            (flow/set-capacity e (max (unchecked-add cap amt) 0))))) :direction :backward))

;;Compute an initial supply, which reflects a pre-filled supply
;;network, from a seq of fixed supply.  fixed-supply contains 
;;records of the form {:SRC :Component :Quantity}.  We basically 
;;traverse the records, applying them to the supply, and decrementing 
;;capacity in the supply-network appropriately.  Returns the 
;;supply and the supply-network as a vector.   

;;Holy....
;;I just realized, we can cache the fills.  As we traverse the set of
;;demands, if we haven't had any failed fills, then we're okay...

(defn prefill-supply [fills supply supply-network]
  (let [{:keys [src->strength max-end-strength Groups SupplyGroups]} supply]
    (reduce (fn [[s net] fill]
              (let [quantity (:Quantity fill)]
                (if (not (pos? quantity)) 
                  [s net]
                  (let [src      (:SRC fill)
                        compo    (symbol (:Component fill))                              
                        str      (* (src->strength src) quantity)]
                    [(add-supply s src compo quantity)
                     (increment-upstream-capacity net compo (- str))]))))
              [supply supply-network] fills)))

(defn bound-supply [bounds supplynet] (assoc supplynet :bounds bounds))
(defmacro logging [msg & body]
  `(do (log! ~msg)
       ~@body))

(defn hierarchically-flow-supply!
  [supply-network filter-map preferences supply demand-records
      & {:keys [demand->src source-node target-node demand-sorter scaling] 
         :or   {demand->src      :SRC
                source-node      'Total
                target-node      :filled
                demand-sorter    identity
                scaling          true}}]
  (let [{:keys [src->strength max-end-strength Groups SupplyGroups]} supply
        bounds (:bounds supply) ;new...                             
        current-bounds (transient bounds) ;the current bounding vals..                
        bounded?         (fn [src] (contains? bounds src))                
        bounded-quantity (fn [src] (get current-bounds src))
        update-bounds!   (fn [src qty] 
                           (let [bnd (get current-bounds src)
                                 newbnd (- bnd qty)]
                             (logging [:updating src :from bnd :to newbnd]
                                      (assoc! current-bounds src newbnd))))
        ;sort demands
        ordered-demands (object-array (demand-sorter  demand-records))
;        src-count        (count (set (map :SRC ordered-demands)))
        get-rule (fn [r] (symbol r))
        try-fill (if scaling 
                   (fn [net src rule qty] 
                     (logging :try-fill
                                        ; (swap! state (fn [_] [net src rule qty])))
                              (with-flow-scaling (src->strength src)
                                (net-fill! net filter-map preferences src rule qty scaled-mincost-flow))))
                   (fn [net src rule qty] 
                     (logging :try-fill
                                        ; (swap! state (fn [_] [net src rule qty])))
                       (net-fill! net filter-map preferences src rule qty flow/mincost-flow))))
        bound      (atom (alength ordered-demands))
        drop-src!  (fn [idx src] nil)
                                 
        fill-count    (atom 0)]
    (loop [idx 0
           acc supply           
           net supply-network
           dropped #{}                    
           history  []]
      (if  (or (= 0.0 (surplus-strength acc)) (>= idx @bound))
        (if  (or (= 0.0 (surplus-strength acc)) 
                 (zero? @fill-count))
          (with-meta 
            {:net  supply-network :supply acc :fills  history}
            {:terminal-net net
             :demands ordered-demands
             :filter-map filter-map 
             :preferences preferences}) ;yield the fills
          (do (reset! fill-count 0) ;reset the fills and try again...
              (logging :retrying
                       (recur 0 acc net dropped history))))
        (let [next-idx (unchecked-inc idx)
              d   (aget ordered-demands idx) ;(first ds)              
              src (demand->src d)
              _ (logging idx)]
          (if (dropped src) 
            ;skip
            (logging :skipping
                     (do (drop-src! idx src)
                         (recur next-idx acc net dropped history)))
            ;otherwise
            (let [strength (src->strength src)
                  {:keys [Quantity SourceFirst]} d  
                  bound         (bounded? src)
                  bounded-qty   (if (not bound)
                                  Quantity
                                  (min (bounded-quantity src) Quantity))
                  str-required  (* bounded-qty strength)]
              (if (or (zero? bounded-qty) (zero? str-required))
                (logging [:zero-quantity :or :zero-str src Quantity bounded-qty str-required]
                         (do (drop-src! idx src) 
                             (recur next-idx acc net (conj dropped src) history))) ;skip
                (if-let [filled      (try-fill net src (get-rule SourceFirst) str-required)]
                  (let  [fills       (:fills filled)  
                         total       (:total-flow filled)
                         cost        (:flow-cost filled)
                         next-net    (:net filled)
                         next-supply (if (not bound)
                                       (apply-fills fills acc)
                                       (apply-bounded-fills fills acc update-bounds!))
                         next-history (conj history {:demand d 
                                                     :str-required str-required
                                                     :fills fills
                                                     :total-flow total
                                                     :flow-cost  cost
                                                     :filled? (:filled? filled)})]
                    (if (:filled? filled)
                      (do (swap! fill-count inc) ;inc the fill...
                          (logging [:filled @fill-count idx src bounded-qty bound (bounded-quantity src)]
                                   (recur next-idx next-supply  next-net dropped next-history)))
                       ;skip
                      (logging [:no-longer-fillable :dropping src]
                               (do (drop-src! idx src)
                                   (recur next-idx next-supply  next-net (conj dropped src) next-history)))))
                    (logging [:unfillable :dropping src]
                             (do (drop-src! idx src)
                                 (recur next-idx acc net (conj dropped src) history))))))))))))

;;Compute the aggregate fill cost for the resulting fill.  The fill
;;cost is the total cost of flow.  Since we arrived at this solution
;;via a prioritized fill, we ended up the min cost of flowing the
;;maximal amount of supply to the highest priority, largest-sized
;;demands (etc.
(defn total-fill-cost [fill-result]
  (reduce + (map :flow-cost (:fills fill-result))))

;;Function that allows us to prioritize demands idiomatically.
(defn ->demand-sorter [groups supply] 
  (let [src->group      (fn [src] (first (graph/sources groups src)))
        group->strength (fn [group] (graph/get-node groups group))]
    (fn [xs]
      (sort-demands 
       #(or (:Priority %) 1) 
       :SRC 
       (:src->strength supply) 
       src->group group->strength xs))))

(defn ->demand-sorter! [groups supply] 
  (let [src->group      (fn [src] (first (graph/sources groups src)))
        group->strength (fn [group] (graph/get-node groups group))]
    (fn [xs]
      (sort-demands! 
       #(or (:Priority %) 1) 
       :SRC 
       (:src->strength supply) 
       src->group group->strength xs))))

(defn proj->supply-gen [proj supply demand]
  (hierarchically-flow-supply! 
   (:SupplyNetwork proj) (:SupplyNodeFilters proj) 
   (:DemandPreferences proj) supply demand :demand-sorter (->demand-sorter (:Groups proj) supply)))


(defn check-bounds 
  "Assert that no srcs are both bounded and fixed."
  [fixed bounded]
  (let [xs (set fixed)
        ys (set bounded)
        shared (clojure.set/intersection xs ys)]
    (assert (empty? shared)
            (str "SRCs cannot currently be both Fixed and Bounded : "
                 shared))))

(defn flow-filler [proj] 
  (let [init-supply (build-initial-supply proj)
        p (if (contains? proj :SupplyNetwork) proj (add-supply-network proj))
        p (if (contains? proj :Groups) proj (add-groups proj))
        [filled-supply filled-network] 
            (prefill-supply (:Fixed proj) init-supply (:SupplyNetwork p))
        _  (check-bounds (map #(:SRC %) (:Fixed p)) (keys (:Bounded p)))
        fixed-bounds (for [src (distinct (map :SRC (:Fixed p)))]
                       [src 0])                               
        bounded-supply  (assoc filled-supply :bounds (into (:Bounded p) fixed-bounds))
        p (assoc p :SupplyNetwork filled-network)]
    (with-meta 
      (fn [demand & {:keys [supply] :or {supply bounded-supply}}] 
        (proj->supply-gen p supply demand))
      {:supply bounded-supply
       :net    filled-network})))

(defn read-project [path]
  (-> (io/workbook->stoke-project path)
      (add-supply-network)
      (add-groups)))




;;Appendix
;;========

;;test

;;backburner, initial supply stuff.
(comment 
;;given a seq of supply records, xs, and a rule basis, returns a pair of 
;;[fills next-rules] as with try-fill-with-rules
(defn records->fills [xs rule-graph]
  (reduce (fn [acc {:keys [Quantity SRC Component]}]
            (let [fills    (first acc)
                  rules    (second acc)
                  [new-fills next-rules] (try-fill-with-rules 
                                           rules Component SRC Quantity)]
              [(into fills new-fills) next-rules]))
          [[] rule-graph] xs))                                 

;;Folds over a sequence of [src compo quantity] fills and adds them 
;;to supply.
(defn apply-fills [supply fills] 
  (reduce (fn [acc {:keys [SRC rule path quantity]}] 
            (let [compo (path->component (:compos supply) path)]
              (add-supply acc src compo quantity))) supply fills))
)
(comment 
(require 'marathon.processing.stoke.app)
(def the-path "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA450.stoke-acfirst.xlsx")
(def the-project (read-project the-path))
(def the-demand-path "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\cases\\case1\\peaks\\")
(def the-demands (marathon.processing.stoke.app/folder->demand-stream the-demand-path))
(def the-demand (first the-demands))
(def tiny-demand (take 1  the-demand))
(def test-demand (map #(assoc % :Quantity 300000) tiny-demand))
)


;;Pending work on wicked awesome, order o' magnitude faster 
;;fill flower.  Ran out of time.  Infrastructure is in place...
(comment 

(defn ^marathon.processing.stoke.arrayflow enable-demand-sources! 
  [^marathon.processing.stoke.arrayflow net]  
  (let [bound (.n net)
        demand-id (unchecked-dec (unchecked-dec bound))]
    (loop [source 0]
      (if (== source bound) net
          (if (== source demand-id) (recur (unchecked-inc source)) 
              (do (when (neg? (aflow/get-capacity! net source demand-id))
                    (aflow/enable-edge! source demand-id))
                  (recur (unchecked-inc source))))))))

(defn ^marathon.processing.stoke.arrayflow disable-sources! 
  [^marathon.processing.stoke.arrayflow net ^objects sources]  
  (let [bound (alength sources)
        demand-id (unchecked-dec (unchecked-dec bound))]
    (loop [idx 0]
      (if (== idx bound) net
          (do (aflow/disable-edge! (aget sources idx) demand-id)))))) 
 

;;Our standardized fill network is pretty simple...
;;We take a normal supply-network, and created zero-cost, inf capacity 
;;arcs between all of the terminal nodes.
;;This serves as the basis for a dynamic fill network, which we mutate 
;;as we fill sequentially.
;;We stub in a universal :demand node, that sits right before 
;;a universal :filled node.  This setup ensures that, under a 
;;topological sorting of the nodes, the :filled node is last, 
;;the :demand node is second-to-last.  This allows us to trivially 
;;modify the topology of the network.  We basically just read in 
;;information from the demand, particularly the rule and the src.
;;From there, we mutate the topology of the network to disable or 
;;enable edges between rule-nodes and the :demand node. 
;;We then alter the capacity of the [:demand :filled] edge to 
;;reflect the quantity demanded.
(defn standardized-fill-net [supply-network preferences]
  (let [terminals (keys preferences)
        pref-arcs (vec (for [t         terminals
                              cost-from (map-indexed vector (get preferences t))]          
                          [(second cost-from) t (first cost-from) flow/posinf]))
        fill-arcs  (vec (for [t  terminals]
                          [t :demand  0 flow/posinf]))]
    (-> supply-network
    (flow/conj-cap-arcs pref-arcs)
    (flow/conj-cap-arcs fill-arcs)
    (flow/conj-cap-arcs [[:demand :filled 1 flow/posinf]]))))

(defn alter-demand-arcs! [net rule rule->sources]
  (-> net 
      (enable-demand-sources!)
      (disable-sources! (rule->sources  rule))))

;;Experimental version using indexed nodes.
;; (defn hierarchically-flow-supply!!
;;   [supply-network filter-map preferences supply demand-records
;;       & {:keys [demand->src source-node target-node demand-sorter] 
;;          :or   {demand->src      :SRC
;;                 source-node      'Total
;;                 target-node      :filled
;;                 demand-sorter    identity}}]
;;   (let [flow-net  (aflow/net->array-net (standardized-supply-network supply-network)) ;mutable network
;;         node->num (:node->num 
;;         {:keys [src->strength max-end-strength Groups SupplyGroups]} supply       
;;         ;sort demands
;;         ordered-demands  (object-array (demand-sorter  demand-records))
;;         get-rule (memoize (fn [r] (symbol r)))
;;         try-fill (fn [net src rule qty] 
;;                    (do ;(println :try-fill)
;;                       ; (swap! state (fn [_] [net src rule qty])))
;;                        (net-fill!! net filter-map preferences src rule qty)))
;;         bound (alength ordered-demands)]
;;     (loop [idx 0
;;            acc supply           
;;            net supply-network
;;            dropped #{}                    
;;            history  []
;;            ]
;;       (if  (or (= 0.0 (surplus-strength acc)) (== idx bound))
;;         (with-meta 
;;           {:net supply-network :supply acc :fills  history}
;;             {:demands ordered-demands
;;              :filter-map filter-map 
;;              :preferences preferences}) ;yield the fills                      
;;         (let [d   (aget ordered-demands idx) ;(first ds)              
;;               src (demand->src d)
;;               next-idx (unchecked-inc idx)]
;;           (if (dropped src) 
;;               ;skip
;;               (recur next-idx acc net dropped history)
;;               ;otherwise
;;               (let [strength (src->strength src)
;;                     {:keys [Quantity SourceFirst]} d
;;                     str-required  (* Quantity strength)]
;;                 (if-let [filled      (try-fill net src (get-rule SourceFirst) str-required)]
;;                   (let  [fills       (:fills filled)  
;;                          total       (:total-flow filled)
;;                          cost        (:flow-cost filled)
;;                          next-net    (:net filled)
;;                          next-supply (apply-fills fills acc)
;;                          next-history (conj history {:demand d 
;;                                                      :str-required str-required
;;                                                      :fills fills
;;                                                      :total-flow total
;;                                                      :flow-cost  cost
;;                                                      :filled? (:filled? filled)})]
;;                     (if (:filled? filled)
;;                       (recur next-idx next-supply  next-net dropped next-history)
;;                       (recur next-idx next-supply  next-net (conj dropped src) next-history)))
;;                   (recur next-idx acc net (conj dropped src) history)))))))))              


;;Given the array of records, eliminates
(defn shift-bad-records! [^objects records last-known-good bound bad?]
  (loop [left  (max last-known-good 0)
         idx   (unchecked-inc left)]
    (if (>= idx bound) 
      left
      (let [r (aget records idx)]
        (if (not (bad? r))
          (let [next-good (unchecked-inc left)
                old-val   (aget records next-good)
                _         (aset records next-good r)
                _         (aset records idx old-val)]
            (recur next-good (unchecked-inc idx)))
          (recur left (unchecked-inc idx)))))))


;;Annealing a supply.
;;Our strategy is to examine changes in the flow relative to modifying
;;an SRC.  We tweak the SRC's supply, and solve the mincost flow JUST 
;;for that src.
;; (comment 
;; (defn optimize-supply [filler base groups]
;;   (let [base-supply     (:supply base)
;;         base-fills      (:fills  base)
;;         {:keys [demands filter-map preferences]} (meta base)        
;;         next-tweak      (supply-tweaker init-supply)
;;         group-score     (supply-grouper groups)
;;         budget-scorer   (strength-over-budget )
;;         sub-demand      (memoize (fn [src] (filter (fn [r] (= (:SRC r) src)) demand)))
;;         flow-quality    (fn [xs]
;;         base-cost       (total-fill-cost fills)
;;         base-flow       (total-flow fills)
;;         value           (fn [s] (+ (group-score s) (budget-score s)))] ;seed results.
;;     (loop [best base]
;; )  
        

(defn supply-tweaker [supply]
  (let [srcs (vec (:srcs supply))
        compos (vec (:compos supply))]
    (fn [] 
      [(rand-nth srcs) (rand-nth compos) (- (rand-int 20) 10)])))
  
(defn tweak-supply [tweaker supply]
  (apply (partial add-supply supply) (tweaker)))

)


           
