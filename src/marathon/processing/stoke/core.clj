;;Stoke is a simple tool for stochastic optimization.
;;I basically created as a one-off, quick approximation tool 
;;for analyzing static demand signals, and quickly generating
;;end-strength constrained portfolios.
(ns marathon.processing.stoke.core
  (:require [spork.opt [core :as opt]
                       [representation :as rep]]
            [spork.util [combinatoric :as c]
                        [generators :as gen]]))


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
;;peak demand.  To compensate for "larger" units typically having more import, 
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

;;Our representation of supply. 

;;An arbitrary upper bound on what would be a ludicrious amount of supply.
(def ludicrous-amount 999999999)

;;Some possible values for a notional supply.
;;We have two capabilities:  
;;MeatEaters, who can inflict violence with extreme prejudice.  
;;ButterChurners, who perform a lot of labor intensive butter churning.  
;;We also have to general classes, or components: the lifers, dudes that are 
;;employed full-time in either profession, and part-timers or WeekendWarriors. 
(def notional-stats 
  {:src-strengths {:MeatEaters 20
                   :ButterChurners 50} 
   :compos [:Lifers :WeekendWarriors]})

(def notional-srcs   (keys (:src-strengths notional-stats)))
(def notional-compos (:compos notional-stats))

(defn random-demand [srcs] 
  {:Start (rand-int 4000) :Duration (rand-int 200) :SRC (rand-nth srcs)})

;(def random-demands (take 10 (repeatedly #(random-demand notional-srcs)))) 

;;A generation of random-demands.
(def some-demands
  [{:Start 6,    :Duration 165, :SRC :ButterChurners}
   {:Start 3080, :Duration 46,  :SRC :MeatEaters}
   {:Start 2135, :Duration 40,  :SRC :ButterChurners}
   {:Start 2997, :Duration 152, :SRC :ButterChurners}
   {:Start 2705, :Duration 170, :SRC :MeatEaters}
   {:Start 3365, :Duration 170, :SRC :MeatEaters}
   {:Start 1230, :Duration 193, :SRC :ButterChurners}
   {:Start 347,  :Duration 178, :SRC :MeatEaters}
   {:Start 815,  :Duration 4,   :SRC :MeatEaters}
   {:Start 2045, :Duration 167, :SRC :ButterChurners}])

(def small-demands (take 2 some-demands))


;;given a sorted sequence of demands, of a identical SRC, we need a way 
;;to walk the timeline, and accumulate a list of active demands.  The 
;;only time the active demands change is when a new demand activates, 
;;or an existing demand deactivates. We just sweep the demands in order 
;;recording their start and stop times, and accumulate a sequence of 
;;active demands by time. This will help us compute peaks easily later.
(defn temporal-profile [xs] 
  (let [add-demand  (fn [t x] {:t t :type :add  :data x})
        drop-demand (fn [t x] {:t t :type :drop :data x})
        resample    (fn [t]   {:t t :type :resampling :data nil})
        earliest    (fn [l r] (compare (:t l) (:t r)))
        handle (fn [{:keys [t type data]} [es actives state]]
                  (case type
                    :resampling [es actives :changed]
                    :add [(-> es (conj (drop-demand (+ t (:Duration data)) data))
                                 (conj (resample t)))
                          (conj actives data)
                          :added]
                    :drop [es (disj actives data) :dropped]))
        initial-events (into (sorted-set-by earliest) 
                             (map (fn [x] (add-demand (:Start x) x)) xs))]
  (gen/unfold (fn [[es _ _]]  (empty? es))  ;halt when no more events.            
              (fn [[es actives s]]                
                (let [event               (first es)
                      remaining-events    (disj es event)
                      current-time        (:t event)]
                  (handle event [remaining-events actives s]))) 
              [initial-events #{} :init])))

;;Pretty general function.
(defn activity-profile [xs]
  (->> (temporal-profile xs)
       (map (fn [[es actives s]] {:t (:t (first es)) :actives actives :s s}))
       (partition-by :t)
       (map last)
       (concat)
       (map (fn [x] [(:t x) (-> x (dissoc :t) 
                                  (dissoc :s)
                                  (assoc  :count (count (:actives x))))]))
       (into (sorted-map)))) ;;sorted map may be gratuitous       

(defn peak-demands [xs]
  (let [active-count (fn [r] (:count r))
        sorted  (->> (sort-by :Start xs)
                  (activity-profile)
                  (sort-by (fn [[t r]] (active-count r))) 
                  (reverse))
        peak (active-count (second (first sorted)))]
    (take-while (fn [[t r]] (= (active-count r) peak))
                sorted))) 

;;given a sequence of demands, we need a way to compute peak demand.
(defn get-peak-demands [xs] 
  (for [[src recs] (group-by :SRC xs)]
    (sort-by :Start recs) 

;;We'll derive the srcs from the actual demand later. 
(defn ->supply [srcs compos end-strength]
  {:solution (into {} (map vector (map vector srcs compos) (repeat 0)))
   :end-strength end-strength})
  
(defn supply-spec [srcs compos]
  (let [combos (for [s srcs
                     c compos] [s c])] 
    (reduce conj {}  
       (for [[s c] combos]
         [[s c] [0 ludicrous-amount]]))))

(defmacro supply-solution [srcs compos]
  (let [spec (supply-spec (eval srcs) (eval compos))]   
    `(~'rep/defsolution ~'supply ~spec)))
  





