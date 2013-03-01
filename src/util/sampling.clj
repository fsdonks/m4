;A generic namespace for drawing samples from populations.
(ns util.sampling)

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

;We can go about this a number of ways, but I think the simplest is to define
;the generators up front, and to make them a function of dependences...
;The simplest generator is a dependency injector, specifically if we find 
(defn dependency-injector [has-dependents? add-deps] 
  (fn [rec]
    (if (has-dependents? rec) (add-deps rec) rec)))

;we can model "normal" independent records as dependencies with a self-loop...
(defn dependencies [from xs] 
  (reduce (fn [acc [to data]] 
            (assert (nil? (get acc to)))
            (assoc acc to data)) {} xs)) 

;(defn make-dependency-injector [keyfunc add-dep depmap population]
;  (dependency-injector #(contains? depmap (keyfunc %))
;     (fn [rec] (reduce (fn [acc [k {:keys [to data]}]]
;                         (
;                       
                       
                        

