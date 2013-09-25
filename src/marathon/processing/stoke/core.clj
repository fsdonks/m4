;;Stoke is a simple tool for stochastic optimization.
;;I basically created as a one-off, quick approximation tool 
;;for analyzing static demand signals, and quickly generating
;;end-strength constrained portfolios.
(ns marathon.processing.stoke.core
  (:require [spork.opt [core :as opt]]
            [spork.opt.representation :refer [defsolution]]
            [spork.opt  [dumbanneal :as ann]]
            [spork.util [combinatoric :as c] 
                        [temporal :as temporal]]))


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
       (reduce (fn [[tot-required tot-missed] [src required]]
                (let [supplied (get supply-map src 0)]
                  [(+ tot-required required) 
                   (+ tot-missed (max (- required supplied) 0))]))
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
  [activity-record]
  (reduce + (map :Quantity (:actives activity-record))))
  

(defn future->src-records
  "Computes a map of {src peak-records} for each src in the demand."
  [xs] 
  (temporal/peaks-by :SRC xs  
                     :start-func :Start  :duration-func :Duration
                     :peak-function total-quantity-demanded))

(defn src-records->src-peaks
  "Reduces a map of {src peak-records}, to a map of {src peak-quantity}.  
   Used for simplified supply evaluation."
  [xs]
  (into {} 
    (for [[src peak-records] xs]     
      [src (reduce + (map :Quantity peak-records))])))

;;Generating Supply
;;=================
 
;;We'll derive the srcs from the actual demand later. 
(defn ->supply-solution
  "A supply solution is a map [[src compo] quantity], and an end-strength 
   constraint.  Given a set of srcs and compos, we can enumerate an empty 
   supply.  We also pack along a mapping of src to strength."
  [srcs compos max-end-strength src->strength]
  {:srcs srcs 
   :compos compos
   :supply         (zipmap (for [s srcs
                                 c compos]  [s c])  (repeat 0))
   :src->strength   src->strength
   :total-strength   0
   :max-end-strength max-end-strength})

(defn add-supply [s src compo qty & [strength]]
  (assert (contains? (:supply s) [src compo]) 
          (str "Unknown supply key" [src compo]))
  (-> s 
    (update-in [:supply [src compo]] + qty)
    (update-in [:total-strength] + (or strength 
                                       (* qty (:src->strength s) src)))))
(defn surplus-strength [s] 
  (- (:max-end-strength s) (:total-strength s)))

;;One way to generate a supply is to naively hierarchically fill relative to  
;;a demand future.  We can sort the demands by priority, then strength.
;;We then traverse the demands in order, filling each demand with the preferred
;;component, until we run out of end-strength.  This is a decent approximation 
;;for an initial feasible solution, or a simple replacement for optimization.
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
;;Testing
;;=======


;;Some possible values for a notional supply.
;;We have two capabilities:  
;;MeatEaters, who can inflict violence with extreme prejudice.  
;;ButterChurners, who perform a lot of labor intensive butter churning.  
;;We also have two general classes, or components: the lifers, dudes that are 
;;employed full-time in either profession, and part-timers or WeekendWarriors.
;;MeatEaters are mustered in 20-person elements, while ButterChurners are built
;;of 50-person teams.

(def notional-stats 
  {:src-strengths {:MeatEaters 23 :ButterChurners 55} 
   :compos        [:Lifers :WeekendWarriors]})

;;Useful aliases 
(def notional-srcs   (keys (:src-strengths notional-stats)))
(def notional-compos (:compos notional-stats))


;;Demand Generation and Manipulation
;;==================================

;;In practice, we'll be pulling in a set of demands from either an on-demand 
;;stream of stochastic futures, or a pre-generated dataset.  In either case, 
;;each data set will at least have records that contain fields associated with 
;;a start time, a duration, an SRC or capability type, and a quantity. Standard
;;demand records already codify this, and provide additional meta data (for more
;;robust value functions) if desired.

(def compo-pref {:MeatEaters      :Lifers
                 :ButterChurners  :WeekendWarriors})
(defn compo-preference [src] (get compo-pref src :Lifers))

;;For testing purposes, we generate a set of random demands.
(defn random-demand [srcs] 
  (let [src (rand-nth srcs)]
    {:Start (rand-int 4000) :Duration (rand-int 200) 
     :SRC src  
     :Quantity (rand-int 100)
     :Priority (rand-int 2)
     :Component (compo-pref src)}))

;(def random-demands (take 10 (repeatedly #(random-demand notional-srcs)))) 

;;A sample of random-demands.
(def demand-future
  [{:Start 388,  :Duration 49,  :SRC :MeatEaters, :Quantity 31, :Priority 0, :Component :Lifers}
   {:Start 3569, :Duration 174, :SRC :ButterChurners, :Quantity 67, :Priority 0, :Component :WeekendWarriors}
   {:Start 3656, :Duration 49,  :SRC :MeatEaters,  :Quantity 94,  :Priority 0,  :Component :Lifers}
   {:Start 1885, :Duration 149, :SRC :MeatEaters,  :Quantity 6,  :Priority 0,  :Component :Lifers}
   {:Start 526,  :Duration 16,  :SRC :ButterChurners,  :Quantity 98,  :Priority 1,  :Component :WeekendWarriors}
   {:Start 1996, :Duration 24,  :SRC :ButterChurners,  :Quantity 89,  :Priority 0,  :Component :WeekendWarriors}
   {:Start 2572, :Duration 154, :SRC :MeatEaters,  :Quantity 47,  :Priority 1,  :Component :Lifers}
   {:Start 30,   :Duration 49,  :SRC :MeatEaters,  :Quantity 34,  :Priority 1,  :Component :Lifers}
   {:Start 1891, :Duration 70,  :SRC :MeatEaters,  :Quantity 6,  :Priority 0,  :Component :Lifers}
   {:Start 3030, :Duration 172, :SRC :MeatEaters,  :Quantity 95,  :Priority 1,  :Component :Lifers}])
   
(comment  
;;An arbitrary upper bound on what would be a ludicrious amount of supply.
(def ludicrous-amount 4000)

(def empty-supply 
  (->supply-solution notional-srcs notional-compos 
                     490000 (:src-strengths notional-stats)))
(def filled-supply 
  (hierarchically-fill-supply empty-supply demand-future)) 



)

;;High Level API 
;;==============
;;We'll wrap this up in a nice package...


;;I am currently wrapping the glue code here to tie in the  value function.  

(defn supply->portfolio
  "Generates a portfolio of n force structures, evaluated against k 
   demand futures, drawn from a demand generation function."
  [initial-supply & {:keys [n supply-gen demand-gen] 
                     :or   {n 10
                            supply-gen hierarchically-fill-supply
                            demand-gen random-demands}}]
  'currently-implementing)

(defn portfolio->performance 
  "Given a portfolio of x supplies and a sample size, generates k 
   demand futures, and evaluates each supply against them."
  [xs futures & {:keys [value-func]  :or {value-func evaluate-supply}}]
  (let [indexed-supply  (map-indexed vector xs)
        indexed-futures (map-indexed vector futures)                           
        results     (for [[i supply] indexed-supply
                          [j demand] indexed-futures]
                      {:force i :future j :value (value-func supply demand)})]
    {:forces (into {} indexed-supply)
     :futures (into {} indexed-futures)
     :performance results}))

(defn stoke
  "Given an initial supply, a supply portfolio width, n, and an evaluation 
   sample size, k, and a sequence of demand futures, generates a map of  
  
   {forces  {0 supply1, 1 supply2,...n supplyn}
    futures {0 future0, 1 future1,...k futurek}
    results {{[0...n] [0...k] value}}"
  ([init-supply n k futures] 
    (supply->portfolio init-supply (take k futures) :n n))
  ([srcs compos max-end-strength src->strength k futures] 
    (stoke (->supply-solution srcs compos max-end-strength src->strength))))
       

                                     