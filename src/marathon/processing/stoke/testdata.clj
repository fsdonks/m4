(ns marathon.processing.stoke.testdata)

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
  {:src-strengths {:MeatEaters 23 :ButterChurners 55 :BurgerFlippers 35} 
   :compos        [:Lifers :WeekendWarriors]})

;;Useful aliases 
(def notional-srcs   (keys (:src-strengths notional-stats)))
(def notional-compos (:compos notional-stats))
(def notional-src->strength (:src-strengths notional-stats))

;;Demand Generation and Manipulation
;;==================================

;;In practice, we'll be pulling in a set of demands from either an on-demand 
;;stream of stochastic futures, or a pre-generated dataset.  In either case, 
;;each data set will at least have records that contain fields associated with 
;;a start time, a duration, an SRC or capability type, and a quantity. Standard
;;demand records already codify this, and provide additional meta data (for more
;;robust value functions) if desired.

(def compo-pref {:MeatEaters      :Lifers
                 :ButterChurners  :WeekendWarriors
                 :BurgerFlippers  :WeekendWarriors})
(defn compo-preference [src] (get compo-pref src (rand-nth notional-compos)))

;;For testing purposes, we generate a set of random demands.
(defn random-demand [srcs src->compo] 
  (let [src (rand-nth srcs)]
    {:Start (rand-int 4000) :Duration (+ 100 (rand-int 900)) 
     :SRC src  
     :Quantity (rand-int 100)
     :Priority (rand-int 2)
     :Component (src->compo src)}))

(defn demand-batch [n srcs] 
  (take 10 (take 10 (repeatedly #(random-demand srcs compo-preference)))))

;(def random-demands (demand-batch 10 notional-srcs)) 

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

;;a table of empty supply.
(def empty-supply 
  (into {} (for [src notional-srcs
                 compo notional-compos]
             [[src compo] 0])))


