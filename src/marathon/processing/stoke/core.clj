;;Stoke is a simple tool for stochastic optimization/exploration.
;;I basically created as a one-off, quick approximation tool 
;;for analyzing static demand signals, and quickly generating
;;end-strength constrained portfolios.
(ns marathon.processing.stoke.core
  (:require [spork.opt [core :as opt]]
            [spork.opt.representation :refer [defsolution]]
            [spork.opt  [dumbanneal :as ann]]
            [spork.util [combinatoric :as c] 
                        [temporal :as temporal]
                        [stats :as stats]]))

;;The task we face in Stoke is to try to provide a fast approximation of a 
;;supply portfolio.  Basically, we want to generate a structure portfolio, 
;;and then rapidly evaluate the structure portfolios against several demand 
;;signals.

;;This is NOT a replacement for dynamic anyalysis, i.e. Marathon, but a 
;;static approximation that serves as a quick-turn or stop-gap measure, and 
;;provides insights about the extreme points.

;;We still need Marathon or another dynamic process to analyze the temporally
;;dependent effects on the system, and to account for high fidelity simulation
;;state.  In other words, Stoke is an intentionally limited subset of the 
;;capabilities in Marathon.  

;;Generating Force Structure
;;==========================
;;The first thing to do is figure out a way to generate force structure.  
;;In this case, we start with a drastically simplified mechansim, and gradually 
;;add constraints and penalties to more  closely approximate certain methods.

;;On the surface, generating a force structure is nothing more than assigning 
;;values to a set of supply.  The supply, in this case, is codified by a 
;;set of capabilities, possibly split by component.  We will adopt the 
;;representation of supply as a map of [SRC Compo] -> Value.
;;In a really simplified version, we could just use an n-element vector, 
;;with one element for each [SRC Compo] pair, and some meta data indicating 
;;the mapping from idx -> [SRC Compo].  I'll forgo that for now, but it's an 
;;option...

;;We'll adopt a really dumb (beer-math) way to build structure.  Our intution is
;;that, we only need enough structure to meet the peak demand.  Therefore we 
;;would "like" to build enough supply, distributed across [SRC Compo] to 
;;to ensure that the total supply, across [SRC Compo], will meet (exactly!) the 
;;peak demand for each SRC. 

;;Additionally, we will adopt a simple constraint for the supply: each unit of 
;;[SRC Compo] maps to an associated cost, "strength/unit".  The sum of our 
;;supply cannot exceed a (given) end strength constraint.  So strength is a 
;;shared resource.

;;Finally, we add the simple extension (to take advantage of a stochastic 
;;demand and the desire to generate different supplies), of building supply 
;;in response to some preference in the order of demand fill.  In other words, 
;;some demands will be weighted or preferred above others, and will bias the 
;;capabilities present in the supply.

;;Additional constraints and variations on preferences (primarily preferences 
;;for compo relative to demands) will emerge later.

;;The Duality of Peak Demand and Supply Evaluation
;;================================================

;;By focusing on Peak demand, generating structure according to the previous 
;;description, we eliminate a lot of complexity (and real world behavior) from 
;;the problem domain.  It's okay since we're doing a fast approximation. The 
;;other implication is that we have a simple way to evaluate any supply solution
;;against any demand signal: We compute the peak demand, and determine the 
;;relative sufficiency of each supply of [SRC] to fill its relative peak.  This
;;is equivalent to "tuning" one's supply for the "worst possible day" of the 
;;time horizon for each element of supply.  

;;A simple Value Function
;;=======================

;;The value of any supply, relative to a demand, is the percentage of unfilled 
;;peak demand.   To compensate for "larger" units typically having more import, 
;;we will weight the demand fill by the str/unit (identical to our supply 
;;constraint).  Additional weights may be added later.  So the end result is 
;;the percentage of unfilled str.  

;;A simple optimization
;;=====================

;;We can build an optimal force structure, for an arbitrary demand, and a 
;;given end strength constraint, by minimizing the unfilled strength relative
;;to the peak demand.  In other words, invest in supply, such that peak demands 
;;are filled maximally, in order of preference, and supply str <= end strength.

;;Implementation
;;==============

;;Evaluating Supply
;;=================
;;To evaluate a supply against a demand signal, we just do an accounting drill
;;and compare the total supply, by src, against the peak demand.  We convert 
;;the delta into a percentage unfilled.  This is an intentionally weak value
;;function.  

(defn evaluate-supply
  "Given a map of [src supply] and [src peak-demand], computes the percentage 
   of peak demand that can be met."
  [supply-map peak-map]
  (->> (seq peak-map)  
       (reduce (fn [[tot-supplied tot-required] [src required]]
                (let [supplied (get supply-map src 0)]
                  [(+ tot-supplied supplied)
                   (+ tot-required required)]))
               [0.0 0.0])
       (apply /)))

;;Scraping Peak Demand
;;====================
;;Given a sorted sequence of demands, of an identical SRC, we need a way 
;;to walk the timeline, and accumulate a list of active demands.  The 
;;only time the active demands change is when a new demand activates, 
;;or an existing demand deactivates. We just sweep the demands in order 
;;recording their start and stop times, and accumulate a sequence of 
;;active demands by time. This will help us compute peaks easily later.

;;I actually built a couple of functions for this, and realized they were 
;;really generic.  So I pushed them into spork.util.temporal .  The function
;;__temporal/peaks-by__ traverses the demand records, using the :SRC key as an 
;;aggregate key, and finds the peak time intervals across the entire profile.

(defn total-quantity-demanded
  "A custom peak function to determine which samples in the demand activity 
   profile are considered highest."
  [{:keys [actives] :as activity-record}]
  (reduce + (map :Quantity actives)))  

(defn future->src-activity-records
  "Computes a map of {src peak-records} for each src in the demand."
  [xs] 
  (temporal/peaks-by :SRC xs  
                     :start-func :Start  :duration-func :Duration
                     :peak-function total-quantity-demanded))

(defn src-activity-records->src-peaks
  "Reduces a map of {src peak-records}, to a map of {src peak-quantity}.  
   Used for simplified supply evaluation."
  [xs]
  (into {} 
    (for [[src {:keys [actives]}] xs]     
      [src (reduce + (map :Quantity actives))])))

(defn src-activity-records->demand-records 
  "Flattens peak src records into a vector of demand records."
  [xs]
  (->> (for [[src {:keys [actives]}] xs] actives)
       (reduce (fn [acc xs] (into acc xs)) [])))

;;We might want to turn this into a more general demand-summary or something.
;;There are probably a set of statistics it would be nice to gather.

(defn process-future
  "Converts a future into a set of peak demand records, and a table of 
   {src peak-quantity}"
  [xs] 
  (let [arecs (future->src-activity-records xs)]
    {:src-peaks      (src-activity-records->src-peaks arecs)
     :demand-records (src-activity-records->demand-records arecs)}))

;;Generating Supply
;;=================
 
;;We'll derive the srcs from the actual demand later. 
(defn ->supply-solution
  "A supply solution is a map [[src compo] quantity], and an end-strength 
   constraint.  Given a set of srcs and compos, we can enumerate an empty 
   supply.  We also pack along a mapping of src to strength."
  ([supply max-end-strength src->strength]
    (let [[srcs compos] (reduce (fn [[srcs compos] [s c]]
                                  [(conj srcs s) (conj compos c)])
                                [#{} #{}] (keys supply))]
      {:srcs srcs 
       :compos compos 
       :supply supply 
       :src->strength   src->strength
       :total-strength   (reduce + (for [[[src compo] qty] supply]
                                     (* (src->strength src) qty)))
       :max-end-strength max-end-strength}))
  ([srcs compos max-end-strength src->strength]
    {:srcs srcs 
     :compos compos
     :supply         (zipmap (for [s srcs
                                   c compos]  [s c])  (repeat 0))
     :src->strength   src->strength
     :total-strength   0
     :max-end-strength max-end-strength}))

(defn add-supply [s src compo qty & [strength]]
  (assert (contains? (:supply s) [src compo]) 
          (str "Unknown supply key" [src compo]))
  (-> s 
    (update-in [:supply [src compo]] + qty)
    (update-in [:total-strength] + (or strength 
                                       (* qty (:src->strength s) src)))))
(defn surplus-strength [s] 
  (- (:max-end-strength s) (:total-strength s)))

(defn supply-by-src [s]
  (let [supply (:supply s)]
    (reduce (fn [acc [src compo]] 
              (assoc acc src (+ (get acc src 0) (get supply [src compo]))))
            {}
            (keys supply)))) 

(defn compare-supplies [xs]
  (let [supplies (map :supply xs)
        ks (reduce clojure.set/union (map (comp set keys) supplies))        
        quantities (fn [k] (vec (map (fn [m] (get m k 0)) supplies)))] 
    (reduce (fn [m k] (assoc m k (quantities k))) {} ks))) 
 

;;__Note__ __hierarchically-fill-supply__ is currently a bottleneck, and could
;;benefit from some lower-level optimization.  It takes about 3 seconds to 
;;generate 100 forces; this should be an order of magnitude faster.

;;One way to generate a supply is to naively hierarchically fill relative to  
;;a demand future.  We can sort the demands by priority, then strength.
;;We then traverse the demands in order, filling each demand with the preferred
;;component, until we run out of end-strength.  This is a decent approximation 
;;for an initial feasible solution, or a simple replacement for optimization.
;;__This is a hideously long function, and needs to be broken up__
(defn hierarchically-fill-supply 
  [supply demand-records 
   & {:keys [demand->compo demand->priority demand->src rollover?] 
      :or   {demand->compo    :Component
             demand->priority #(or (:Priority %) 1)
             demand->src      :SRC}}]
  (let [{:keys [src->strength max-end-strength]} supply
        ;sort demands by [priority strength]
        ordered-demands  (sort-by (fn [r] [(demand->priority r) 
                                           (src->strength (demand->src r))])
                                  demand-records)
        ;attempt to fill a demand, with a given strength surplus.
        try-fill (fn [surplus src qty] 
                   (let [cost   (src->strength src)                         
                         feasible-strength (min (* cost qty) surplus)
                         filled (quot feasible-strength cost)
                         spent  (* cost filled)]
                     [filled spent]))
        ;thresholds for minimum unit strengths.
        strength-breaks (reduce (fn [acc [src strength]]
                                  (assoc acc strength
                                         (conj (get acc strength #{})  src)))
                                (sorted-map-by >)  src->strength)                                                                                      
        ;prune keys, finding a new minimum strength break
        advance-breaks (fn [new-minimum breaks drops] 
                         (loop [acc breaks
                                dropped drops]
                           (if (seq acc)
                             (let [[min-break src-set] (first  acc)]
                               (if (> min-break new-minimum)
                                   (recur (dissoc acc min-break)
                                          (clojure.set/union dropped src-set))
                                   [acc dropped]))
                             [acc dropped])))
        ;prune the feasible demand, to the largest strength we can fill.
        filter-feasibles (fn [dropped demands] 
                           (filter #(not (dropped (demand->src %))) demands))]                                                       
    (loop [acc supply           
           xs  ordered-demands
           breaks strength-breaks
           dropped #{}
           feasibles []]
      (cond (or (= 0 (surplus-strength acc)) (empty? breaks)
                (and (empty? xs) 
                     (empty? feasibles)))  
               acc ;yield the result.
            ;distribute surplus supply based on things we successfully filled.
            (and (empty? xs) (seq feasibles))               
               (recur acc feasibles breaks dropped []) 
            (empty? xs) acc 
            :else              
              (let [min-feasible-strength (first (keys breaks))
                    demand (first xs)                    
                    compo  (demand->compo demand)     
                    surplus (surplus-strength acc)
                    {:keys [SRC Priority Quantity]} demand
                    [filled spent] (try-fill surplus SRC Quantity)                   
                    new-supply (if (> filled 0)
                                 (add-supply acc SRC compo filled spent)
                                 acc)
                    next-min-feasible (- surplus spent)]
                (if (< next-min-feasible min-feasible-strength)
                  ;lower the threshold, screen out demands that are now infeasible.
                  (let [[new-breaks new-dropped] 
                          (advance-breaks next-min-feasible breaks dropped)] 
                    (recur new-supply (filter-feasibles new-dropped (rest xs)) 
                           new-breaks new-dropped feasibles))
                  ;continue filling, recording a successful fill.
                  (recur new-supply (rest xs) breaks dropped 
                         (conj feasibles demand))))))))  

;;High Level API 
;;==============
;;We'll wrap this up in a nice package...

;;I am currently wrapping the glue code here to tie in the  value function.  

(defn supply->portfolio
  "Generates a portfolio of n force structures, evaluated against k 
   demand futures, drawn from a demand generation function."
  [initial-supply futures
   & {:keys [n supply-gen] 
      :or   {n 10  supply-gen hierarchically-fill-supply}}]
  
  (->> futures
       (take n)
       (map #(supply-gen initial-supply %))))

(defn portfolio->performance 
  "Given a portfolio of x supplies and a sample size, generates k 
   demand futures, and evaluates each supply against them."
  [xs futures & {:keys [value-func]  :or {value-func evaluate-supply}}]
  (let [indexed-supply  (map-indexed vector xs)
        indexed-src-supply (map-indexed (fn [idx s] [idx (supply-by-src s)]) xs)        
        indexed-futures (->>  (map process-future futures)
                              (map-indexed vector))
        results (for [[i supply] indexed-src-supply
                      [j {:keys [src-peaks]}] indexed-futures]
                {:force i :future j :value (value-func supply src-peaks)})]
    {:performance (vec results)
     :peaks (into {} (for [[idx d] indexed-futures]  [idx (:src-peaks d)]))
     :forces (into {} indexed-supply)
     :futures (into {} indexed-futures)}))

(defn stoke
  "Given an initial supply, a supply portfolio width, n, and an evaluation 
   sample size, k, and a sequence of demand futures, generates a map of  
  
   {performance {{[0...n] [0...k] value}
    forces      {0 supply1, 1 supply2,...n supplyn}
    futures     {0 future0, 1 future1,...k futurek}}"
  ([init-supply n k futures]
    (portfolio->performance 
      (supply->portfolio init-supply (take n futures) :n n)
      (take k (drop n futures))))
  ([srcs compos max-end-strength src->strength n k futures] 
    (stoke (->supply-solution srcs compos max-end-strength src->strength)
           n k futures)))
  
;;Scraping output from our experiments.
(defn stoked->stats [results] 
  (let [force-performance (for [[f recs] (group-by :force (:performance results))]
                            {:force f :mean-value (stats/mean (map :value recs))})]
    (sort-by #(- (:mean-value %)) force-performance)))

(defn top-n
  "Yields the top n force structures from a stoked result set."
  [n stoke-results] 
   (map :force (take n (stoked->stats res))))

(defn summarize-stoke-results
  "Computes a database of summary results, including the n-best force 
   structures, the best force, and the range of values exhibited by the best 
   force structures for each src/compo."
  ([n stoke-results]
    (let [force-keys  (top-n n stoke-results)
          best        (first force-keys)        
          ranges      (for [[k supplies]  
                            (compare-supplies 
                              (vals (select-keys (:forces res)  force-keys)))]
                        [k (stats/deciles supplies)])]
      {:best             best
       :top-performers   force-keys 
       :supply-ranges    ranges}))
  ([stoke-results] (summarize-stoke-results 10 stoke-results)))

;;testing 
(comment 
(require '[marathon.processing.stoke [testdata :as data]])

(defn rand-demand-stream []
  (repeatedly #(data/demand-batch 100 data/notional-srcs)))

;;tested with 5000 records, still works great.
(def demand-stream (rand-demand-stream))

(def empty-supply 
  (->supply-solution data/empty-supply 5000  data/notional-src->strength))

(def processed-demand (process-future  (first demand-stream)))

(def test-supply 
  (hierarchically-fill-supply empty-supply (:demand-records processed-demand)))

;;=> (:supply test-supply)
;;{[:BurgerFlippers :Lifers] 0, 
;; [:BurgerFlippers :WeekendWarriors] 5977, 
;; [:MeatEaters :Lifers] 2256, 
;; [:MeatEaters :WeekendWarriors] 0, 
;; [:ButterChurners :Lifers] 0, 
;; [:ButterChurners :WeekendWarriors] 4162}

(def supply-score (evaluate-supply test-supply (:src-peaks processed-demand)))
;;=> supply-score
;;1.0
;;=> (surplus-strength test-supply)
;;7
(def n 5)
(def test-portfolio   (supply->portfolio empty-supply demand-stream :n n))
(def test-performance (portfolio->performance test-portfolio (drop n demand-stream)))


;;Some simple analysis
(def res (stoke empty-supply 25 1000   (rand-demand-stream)))
;;=>(pprint (into {} (for [[k xs]  (compare-supplies (vals (:forces res)))]
;;                      [k (stats/deciles xs)])))
;{[:ButterChurners :WeekendWarriors] (0 0 0 0 9 22 31 50 73),
; [:MeatEaters :Lifers]              (28 41 79 95 97 115 126 160 185),
; [:ButterChurners :Lifers]          (0 0 0 0 0 0 0 0 0),
; [:BurgerFlippers :WeekendWarriors] (0 3 11 24 46 67 68 75 80),
; [:BurgerFlippers :Lifers]          (0 0 0 0 0 0 0 0 0),
; [:MeatEaters :WeekendWarriors]     (0 0 0 0 0 0 0 0 0)}

;(pprint (stoked->stats res))
;=>({:force 18, :mean-value 0.7364415776187464}
;   {:force 14, :mean-value 0.7226026394863088}
;   {:force 8, :mean-value 0.7156763089993542}
;   {:force 11, :mean-value 0.6706928986481988}
;   {:force 21, :mean-value 0.6495811320773506}
;   {:force 19, :mean-value 0.6464747569164331}
;   {:force 15, :mean-value 0.64562547478374}
;   {:force 24, :mean-value 0.6386312933130006}
;   {:force 7, :mean-value 0.6326289573632613}
;   {:force 22, :mean-value 0.6291692228301519}
;   {:force 9, :mean-value 0.6187900192308236}
;   {:force 23, :mean-value 0.6149090743417369}
;   {:force 5, :mean-value 0.6118705501646068}
;   {:force 2, :mean-value 0.6084039542107609}
;   {:force 13, :mean-value 0.6014913465652778}
;   {:force 17, :mean-value 0.5664251425448432}
;   {:force 3, :mean-value 0.5288163371077762}
;   {:force 16, :mean-value 0.5096288884551696}
;   {:force 20, :mean-value 0.49229629029773}
;   {:force 4, :mean-value 0.4826440080932078}
;   {:force 6, :mean-value 0.46522297468977303}
;   {:force 12, :mean-value 0.4363424569200143}
;   {:force 0, :mean-value 0.3735575897220755}
;   {:force 1, :mean-value 0.35645599019893937}
;   {:force 10, :mean-value 0.3177398399237201})

;=> (let [top-ten (map :force (take 10 (stoked->stats res)))]      
;     (pprint (compare-supplies (vals (select-keys (:forces res)
;                                                  top-ten)))))
;{[:ButterChurners :WeekendWarriors] [0 0 0 0 22 0 0 9 0 24],
; [:MeatEaters :Lifers] [115 185 147 190 163 202 126 145 112 160],
; [:ButterChurners :Lifers] [0 0 0 0 0 0 0 0 0 0],
; [:BurgerFlippers :WeekendWarriors] [67 21 46 18 1 10 60 33 69 0],
; [:BurgerFlippers :Lifers] [0 0 0 0 0 0 0 0 0 0],
; [:MeatEaters :WeekendWarriors] [0 0 0 0 0 0 0 0 0 0]}

;=> (pprint 
;     (let [top-ten (map :force (take 10 (stoked->stats res)))]      
;       (for [[k supplies]  (compare-supplies 
;                             (vals (select-keys (:forces res)
;                                                top-ten)))]
;         [k (stats/deciles supplies)])))

;([[:ButterChurners :WeekendWarriors] (0 0 0 0 0 0 9 22 24)]
; [[:MeatEaters :Lifers]              (115 126 145 147 160 163 185 190 202)]
; [[:ButterChurners :Lifers]          (0 0 0 0 0 0 0 0 0)]
; [[:BurgerFlippers :WeekendWarriors] (1 10 18 21 33 46 60 67 69)]
; [[:BurgerFlippers :Lifers]          (0 0 0 0 0 0 0 0 0)]
; [[:MeatEaters :WeekendWarriors]     (0 0 0 0 0 0 0 0 0)])

)  


(comment
  (time (doseq [s (->>  (rand-demand-stream)
                    (map-indexed   
                      (fn [idx d] 
                        [idx (hierarchically-fill-supply empty-supply d)]))
                    (map first)
                    (take 100))]
          (println s)
          :done))
  
)
                                     