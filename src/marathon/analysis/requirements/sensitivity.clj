(ns marathon.analysis.requirements.sensitivity
  (:require [spork.util
               [io :as io]
             [table :as tbl]
             [general :as gen]]
            [marathon.analysis :as a]
            [marathon.analysis [requirements :as r] [util :as u]]
            [clojure.core.async :as async]))

(def supply-fields
  [:Type :Enabled :Quantity :SRC :Component :OITitle :Name
   :Behavior :CycleTime :Policy :Tags :SpawnTime :Location :Position
   :bound])

(defn contour-table [bound-tables]
  (->> (map :requirement bound-tables)
       tbl/concat-tables
       (tbl/order-by [:bound :SRC :Component])))

(def prev (atom nil))
(defn contours [tgt xs]
  (let [conts (r/requirements-contour tgt xs)
        _     (reset! prev conts)]
    (contour-table conts)))

;;field order gets messed up, unknown why.
(defn spit-contours [tgt xs]
  (let [out (io/file-path (io/parent-path tgt) "requirements-contour.txt")]
        (->> (contours tgt xs)
             (tbl/spit-table out))))

;;basic invocation
(comment
  (def tgt
    (io/file-path "~/Documents/taa2327/RA/m4book-2327-v8-inclusive-requirements.xlsx"))
  ;;compute the big table of requirements indexed by bound
  (def res (contours tgt (range 0 21)))
  ;;shove into excel
  (tbl/paste-records! res)

  )

;;pending.
;;we can significantly speed up the process if we cache information between runs,
;;partition the search space in halves, and prune SRCs that are insensitive
;;based on samples of the end points.

;;This is basically a bisection search along the domain of samples we're
;;examining.

;;Say we have a pending domain to examine:
;;threshold t within [l .... r].  So , {t [l r]}
;;If we claim that the response function will be monotonically increasing
;;for requirements(t), then we know that [t_l low] [t_r high] form potential bounds,
;;where low = requirements(t_l), and high = requirements(t_r).

;;1) if low = high, then we know that all values between [t_l t_r]
;;are identical due to insensitivity.

;;We can effectively prune the remaining values and assign them to low.

;;2) otherwise, we have some change over the interval.  There must be an inflection
;;point. We can continue searching for this point smartly using bisection.

;;Naively, we just bisect the remaining span and divid into 2 halves.
;;[l1 r1] [l2 r2], where
;;l1 = l + 1
;;r2 = r - 1
;;mid = (r2 - r1) / 2
;;r1 = l1 + mid
;;l2 = mid + 1

;;So we have visually a binary tree...
;;[l                  r]
;;     /          \
;; [l1    r1][l2    r2]
;;where the leaves indicate unexplored segments of t
;;for which the inflection may exist.

;;We proceed recursively as before with each segment,
;;e.g. [l1 r1] become [l r] as before.

;;if say reqs(l1) = reqs(r1), then
;;we are done with the left.
;;[l                             r] ;;level 0
;;     /              \
;; [l1         r1][l2          r2]  ;;level 1
;;                 [l3 r3][l4 r4]   ;;level 2
;;At this rate, we find any inflections in log2(n) time.
;;Pruning is maximized.

;;The result then is a segment tree:
;;{[l r] {[l1 r1] 10
;;        [l2 r2] {[l3 r3] 11}
;;                 [l4 r4] 13}
;;
;;From this we can reconstruct the entire function:
;;[10 10 10 10 10 10 11 11 11 13 13 13]
;;10 runs.

;;Can we let the data drive this?
;;So, initially, we have a bunch of points to analyze.
;;[0 1 2 3 4 ... 20]

;;Which translate into samples over a segment,
;;{[0 20] [1 ... 19]}
;;if it's insensitive, then we have
;;{:known  {0 10 ... 20 10}
;; :pending nil}

;;otherwise
;; {:known {0  10
;;          20 13}
;;  :pending [1 ... 19]}

;; {:known {0  10
;;          20 13}
;;  :pending [1 ... 19]}


;;Let's define a function called extrema-search.
;;We assume the function F is monotic.
;;We then have an initial interval to search for inflections.
;;Our search state is a map of known values:
;;{0 10, 20 13 ...}
;;Pending intervals to search,
;;[[1 ... 19]]


;;let's define a linear test function with 3 inflections over the range [0 ... 13].
(defn piecewise-linear [kvs]
  (let [kvs (vec (sort-by first kvs))]
    (fn [n]
      (reduce (fn [acc [x y]]
                (cond (< n x) (reduced acc)
                      (= n x) (reduced y)
                      :else y)) (ffirst kvs) kvs))))

(comment
  (def inflections
    {0 10 ;;implicity [0 5] -> 10
     6 11 ;;  [6 8] -> 11
     9 13}) ;;[9 ...] -> 13
  (def f (piecewise-linear inflections))
  (def coords (map #(vector % (f %)) (range 21)))
  (map f (range 12))
  ;;(10 10 10 10 10 10 11 11 11 13 13 13)
  )


;;subdivide a parent segment [l r] into 2 children,
;;s.t. the left child is [l+1 .. m-1][m .. r-1]
;;where m = (l - r) / 2
(defn subdivide [l r]
  ;;since we are computing a subdivided range over
  ;;(l r) (non inclusive), we have to account for
  ;; that in the distance.
  (let [m (quot (- r l) 2)
        mid (+ m l)]
    (when (>= m 1)
      [[(inc l)   mid]
       [(inc mid)       (dec r)]])))

;;this is the naive version.
(defn extrema-search [f xs]
  (let [init (vec (sort xs))
        segment [(first init) (last init)]
        calls   (atom 0)
        sample (fn [x] (swap! calls inc) (f x))]
    (loop [known   {}
           pending [segment]]
      (if-let [[l r] (peek pending)]
        ;;pick a segment off the stack and update, neither point should be
        ;;known.
        (let [_ (println {:visiting [l r] :pending pending :known known})
              singular? (== l r)
              fl    (sample l)
              fr    (if-not singular? (sample r) fl)]
          (recur (assoc known l fl r fr)
                 (if (== fl fr)
                   (pop pending)
                   (reduce conj (pop pending) (subdivide l r)))))
        ;;otherwise we're done, report the inflections.
        (with-meta known {:calls @calls})))))


;;given a segment and some unknowns,
;;sort the unknowns by segment, creating a pending
;;node, s.t. the actual segment is fit to the min/max of the
;;unknown points in the segment.

(defn partition-segments [lr xs]
  (let [[l r]   (first lr)
        r       (or r l)
        [ls rs] (split-with (fn [x] (<= x r)) xs)]
    (->> [(when (seq ls) {:l (first ls) :r (last ls) :unvisited ls})
          (when (seq rs) {:l (first rs) :r (last rs) :unvisited rs})]
         (filter identity))))

;;So extrema search can provide significant savings for us
;;computationally.  We still end up doing more work than necessary,
;;since we're not letting the caller's requested point spread (xs)
;;drive the search.
;;What we'd like to do is maintain some information regarding
;;containment.
#_(defn extrema-search-sparse [f xs]
  (let [init (vec (sort xs))
        segment {:l (first init)
                 :r (last init)
                 :unvisited init}
        pruned  (atom nil)
        calls   (atom 0)
        sample (fn [x] (swap! calls inc) (f x))]
    (loop [known   {}
           pending [segment]]
      (if-let [{:keys [l r unvisited] :as node} (peek pending)]
        ;;pick a segment off the stack and update, neither point should be
        ;;known.
        (let [singular? (== l r)
              fl    (sample l)
              fr    (if-not singular? (sample r) fl)
              constant? (== fl fr)
              new-known (assoc known l fl r fr)
              _        (if (and constant? (not singular?))
                         (swap! pruned concat unvisited))
              ;;when we put a segment on the stack, it should be shrink-wrapped.
              ;;We also partition the unknown points into each segment. So when
              ;;we subdivide, we pack points into the l/r segments, and
              ;;shrinkwrap the segment to be the minima/maxima of the contained
              ;;points.
              new-segments (when-not constant?
                             (-> (subdivide l r)
                                 (partition-segments (rest (butlast unvisited)))))]
          (recur new-known (reduce conj (pop pending) new-segments)))
        ;;otherwise we're done, report the inflections.
        (with-meta known {:pruned (vec @pruned) :calls @calls})))))

;;we're working with a function f(x) = y, where we expect x to be numeric
;;and y to be numeric.  The problem in practice is, we have functions that take
;;an interval and return a result that's not numeric.  We need to keep track
;;if this information.  If we supply a key function and a weight function,
;;we can project xs onto inputs for f, compute (f (key x)), and store
;;the raw result of the function assoc'd to x, while guiding the search
;;with the weight function, (weight (f (key x)))).  Then return the
;;raw results in the meta and provide a function to grab them.

(defn extrema-search-generic
  "Generalization of extrema-search-sparse; allows caller to supply additional
  functions for key and weight, where keyf will provide a function that is
  applied to numeric values in xs prior to evaluation of f, the result of which
  (f (keyf x)) will be stored in a raw map of {x (f (keyf x))}, while the search
  intervals are conducted using the weightf function which projects the raw
  result of (f (keyf x)) onto a numeric value, such that the result of the
  search is a map of {x (weight (f (keyf x)))} with metadata of the raw map
  stored in :raw."
  [f xs & {:keys [keyf weightf]
           :or {keyf identity weightf identity}}]
  (let [init (vec (sort xs))
        segment {:l (first init)
                 :r (last init)
                 :unvisited (case (count init)
                              (1 2) nil
                              (vec (rest (butlast init)))) #_init}
        pruned  (atom nil)
        calls   (atom 0)
        sample (fn [x] (swap! calls inc) (f x))]
    (loop [known   {}
           raw     {}
           pending [segment]]
      (if-let [{:keys [l r unvisited] :as node} (peek pending)]
        ;;pick a segment off the stack and update, neither point should be
        ;;known.
        (let [singular? (== l r)
              fl    (sample (keyf l))  ;raw fl
              fr    (if-not singular? (sample (keyf r)) fl) ;;raw fr
              wl    (weightf fl)
              wr    (weightf fr)
              constant? (== wl wr) ;;weights are same.
              ;;raw results from function
              new-raw   (assoc raw l fl r fr)
              ;;weighted results driving the search.
              new-known (assoc known l wl r wr)
              _        (if (and constant? (not singular?))
                         (swap! pruned concat unvisited))
              ;;when we put a segment on the stack, it should be shrink-wrapped.
              ;;We also partition the unknown points into each segment. So when
              ;;we subdivide, we pack points into the l/r segments, and
              ;;shrinkwrap the segment to be the minima/maxima of the contained
              ;;points.
              new-segments (when-not constant?
                             (-> (subdivide l r)
                                 (partition-segments (rest (butlast unvisited)))))]
          (recur new-known new-raw (reduce conj (pop pending) new-segments)))
        ;;otherwise we're done, report the inflections.
        (with-meta known {:pruned (vec @pruned) :calls @calls :raw raw})))))

(defn recover-raw
  "Helper function to allow us to get back the original values from f(x) in our
   search function, rather than the numeric weights used to guide the search."
  [search-result]
  (-> search-result meta :raw))

(comment ;;simple demo
  (def res3 (extrema-search-generic (fn [{:keys [x]}] {:bound x :result (f x)}) [0 1000]
                                    :keyf (fn [x] {:x x}) :weightf :result))
  ;;user> res3
  ;;{0 10, 1000 13}
  ;;user> (recover-raw res3)
  ;;{0 {:bound 0, :result 10}, 1000 {:bound 1000, :result 13}}
  )

;;so let's talk about our function...
;;In practice, we'll have a requirements analysis doing its
;;own thing, executing another bisection search.

;;Since we're doing function evaluations across multiple
;;thresholds, we have a lot of intermediate information covered.

;;If we look at requirments computation for an SRC as a
;;single function of 1 integer parameter (the growth factor),
;;we have the mechanism our bisection search uses.

;;When we're searching extrema, we're actually performing
;;multiple requirements analyses (bisection searches) with
;;an implicit parameter - CMDD.  As CMDD grows, it relaxes
;;the constraints on the requirement.  The implication is that
;;higher CMDD provide information on upper bounds for feasibility
;;for lower CMDD searches.  That is, for the same level of supply,
;;for a CMDD, if we know any higher CMDD indicates failure, we can
;;prove that this sample will also fail, and require growth.
;;Similarly, if we have a lower known CMDD that succeeded, we
;;can prove that this CMDD will also succeed (since it's a relaxation).

;;So, during our extrema search, where we vary CMDD, we will have multiple
;;function evaluations for a specific CMDD during the requirements analysis
;;computation.  Normally, we toss out these samples and only care about
;;the binary result (the solution to the bisection search, e.g. the
;;minimum feasible requirement).  Along the way, we leave a trail
;;of pass/fails in the form of a map inside the bisection search function.
;;If we save this map, and associate it with a specific CMDD level,
;;then we have a wealth of information for pruning unnecessary
;;function evaluations in the requirements sensitivity analysis case.

;;Functionally, we really want to maintain a map of
;;{supply {min-feasible-cmdd   min
;;         max-infeasible-cmdd max}}
;;if the current CMDD we're testing is >= the min-feasible,
;;then we can accept the node as sucessful and prune it.
;;conversely, if the CMDD is <= the max-infeasible-cmdd,
;;then we can reject it.
;;If it's in between, we have to test by evaluating the
;;function, after which we can update our existing bounds for
;;min-feasible and max-infeasible.

;;So, in the emergent framework that involves pruning,
;;we want a smarter requirements analysis function that
;;can leverage this kind of pruning information and update
;;bounds during searches.


;;We need to redefine the requirements-countour to split the RA inputs
;;by SRC, then perform an extrema search on each SRC in parallel.
;;The next step will be replacing bisecting-search with one that
;;incorporates CMDD-based pruning information.

#_
(defn requirements-by
  "Helper function for our parallel requirements computation."
  [tbls peaks search n]
  (fn [[src compo->distros]]
    ;;for each src, we create a reqstate
    (if-let [peak (peaks src)]
      (let [_ (println [:computing-requirements src :remaining (swap! n dec)])
            ;;We now pack along the peak demand for extra context.
            reqstate       (assoc (load-src tbls src compo->distros)
                                  :peak peak)
            _              (println [:growing-by :proportional :from (:minimum-supply reqstate)])
            [lower upper]  (find-bounds reqstate :init-lower 0 :init-upper peak)]
        (if (== lower upper 0)
          [src reqstate]
          [src (search reqstate :init-lower lower :init-upper upper :init-known? {upper 0})]))
      (do (println [:skipping-src src :has-no-demand])
          [src nil])
      )))

;;instead of the async version where we're doing n requirements analyses and
;;allow work stealing, we'll focus on doing the n requirements analysis [src distros]
;;synchronously, and then do the extrema search with additional constraint information
;;as we go.  Looks like requirements-by is fine.  We need to inject the extrema
;;search into here.

;;in the parlance of extrema-search-generic, this will serve as our
;;weightf function to project raw results onto a numeric weight.
(defn requirements-sum
  "Compute a simple numeric result for the total requirement for
   a given requirement state returned by a search.  Yields a
   simple sum of the quantities from the resulting supply records.
   Allows us to interface with extrema-search by providing a simple
   function to yield a useful numeric value."
  [reqstate]
  (->> (some-> reqstate
               :supply)
       (map (comp long :Quantity))
       (reduce + 0)))

#_(defn tables->requirements-sync
  "Given a database of distributions, and the required tables for a marathon 
   project, computes a sequence of [src {compo requirement}] for each src."
  [tbls & {:keys [dtype search src-filter]
           :or {search bisecting-convergence ;iterative-convergence
                dtype  :proportional
                src-filter (fn [_] true)}}]
  (let [;;note: we can also derive aggd based on supplyrecords, we look for a table for now.
        distros (into {} (->> (aggregate-distributions tbls :dtype dtype)
                              (filter (fn [[src _]]
                                        (src-filter src)))))
        peaks   (->>  (:DemandRecords tbls)
                      (tbl/table-records)
                      (filter #(and (:Enabled %)
                                    (distros (:SRC %))))
                      (demands->src-peaks))
        n       (atom (count peaks))
        src-distros->requirements  (requirements-by tbls peaks search n)]
    (mapv (fn compute-reqs [src-distros]
            ;;for each cmdd value, we want to compute multiple requirements,
            ;;and do so using extrema-search. Assume we have multiple cmdd's.
            (try  (src-distros->requirements src-distros)
                  (catch Exception e (->error src-distros e))))
          (seq distros))))



#_
(defn bisecting-convergence
  "reqstate is a map of information for a basic requirements
   run, which is used to create a parametric run based on
   the initial supply, a growth-step n, and a distribution
   of supply by component.  Reqstate also includes
   a peak field, which lets us know what the peak demand is
   for inferring the expected misses for a supply of 0."
  [reqstate & {:keys [distance init-lower init-upper log init-known]
               :or   {distance *distance-function*
                      init-lower 0
                      init-upper 10
                      log println
                     }}]
  (let [known?     (atom (or init-known {}))
        converge   (fn [dir reqs n]
                     (do (log [:converged dir n])
                         (distribute reqs (:src reqstate) n)))
        amount     (fn amt [reqs n]
                     ;(log [:amount n])
                     (get-or @known? n
                             (let [rtest (-> reqs
                                             (distribute (:src reqs) n))
                                   ;;Found no missed demands, so zero! misses!
                                   res (or (calculate-requirement rtest distance) 0)
                                   _   (swap! known? assoc n res)
                                   ;_   (when (zero? n) (println [:amount 0 res]))
                                   ]
                          res)))
        _ (assert (not (neg?  (- init-upper init-lower))) "need a valid non-negative interval!")]
    (loop [reqs      reqstate
           lower init-lower
           upper init-upper]
      (let [hw    (quot (- upper lower) 2)
            mid   (+ lower hw)]
        (if (= mid lower)
          (case (mapv zero? [(amount reqs lower) (amount reqs upper)])
            ;;In this case, BOTH guesses produce 0 misses!
            ;;We want to take whichever guess is NOT 0,
            ;;since 0 is not a valid guess.
            [true  true]
                (if (pos? lower)
                  ;;lower is a valid guess, and is the minimum!
                  (converge :left  reqs  lower)
                  ;;lower is zero!, upper is the valid guess
                  ;;and minimum! (1).
                  (converge :right reqs upper))
            [false true]
                  ;;Upper is the only valid guess, and minimum!
                  (converge :right reqs upper)
            (do (reset! rs reqstate)
              (throw (Exception. (str [:wierd-case! lower upper  @known? (:supply reqs)])))))
          (let [reqs (update reqs :iteration inc)
                res  (amount reqs mid)
                _    (log [:guessing [lower upper] :at mid :got res])]
            (if (pos? res)
              (recur reqs mid upper)
              (recur reqs lower mid))))))))


;;redefine requirements-contour in terms of extrema-search.
;;define bisecting-convergence-prune.  Extrema-search
;;expects a function that returns a simple number, so we
;;need to probably just sum the requirements.

#_
(defn requirements-by
  "Helper function for our parallel requirements computation."
  [tbls peaks search n]
  (fn [[src compo->distros]]
    ;;for each src, we create a reqstate
    (if-let [peak (peaks src)]
      (let [_ (println [:computing-requirements src :remaining (swap! n dec)])
            ;;We now pack along the peak demand for extra context.
            reqstate       (assoc (load-src tbls src compo->distros)
                                  :peak peak)
            _              (println [:growing-by :proportional :from (:minimum-supply reqstate)])
            [lower upper]  (find-bounds reqstate :init-lower 0 :init-upper peak)]
        (if (== lower upper 0)
          [src reqstate]
          [src (search reqstate :init-lower lower :init-upper upper :init-known? {upper 0})]))
      (do (println [:skipping-src src :has-no-demand])
          [src nil])
      )))


(defn interpolate [keyf lerpf xs results]
  (let [expected (atom (sort xs))
        knowns   (group-by keyf results)]
    (concat (apply concat
                   (for [[[l ls] [r rs]] (->> knowns
                                              (sort-by key)
                                              (partition 2 1))]
                     (let [[missing remaining] (->> @expected
                                                    (drop-while #(= % l))
                                                    (split-with #(< % r)))]
                       (reset! expected remaining)
                       (concat ls (mapcat (fn [k] (lerpf k ls)) missing)))))
            (knowns (first @expected)))))

;;helper function to expand our sparse results that form a discrete signal
;;into complete results interpolated by expected integer sampling points.
(defn expand-missing [xs results]
  (interpolate :bound (fn [b xs] (map #(assoc % :bound b) xs)) xs results))

;;revamped, merged with tables->requirements-sync
(defn requirements-contour-faster
  [proj xs  & {:keys [dtype search src-filter]
                   :or {search r/bisecting-convergence ;iterative-convergence
                        dtype  :proportional
                        src-filter (fn [_] true)}}]
  (let [tbls    (-> (a/load-requirements-project proj)      :tables)
        distros (into {} (->> (r/aggregate-distributions tbls :dtype dtype)
                              (filter (fn [[src _]]
                                        (src-filter src)))))
        peaks   (->>  (:DemandRecords tbls)
                      (tbl/table-records)
                      (filter #(and (:Enabled %)
                                    (distros (:SRC %))))
                      (r/demands->src-peaks))
        n       (atom (count peaks)) ;;revisit.
        src-distros->requirements  (r/requirements-by tbls peaks search n)]
    (->> (for [[src distro :as sd] distros]
           ;;for each [src distro] we want to compute a requirements analysis contour
           ;;for the points in xs.
           [src
            (extrema-search-generic
             (fn [[src distro bound :as sd]]
               (binding [r/*distance-function*    r/contiguous-distance
                         r/*contiguity-threshold* bound]
                 (assoc (second (src-distros->requirements sd)) :bound bound)))
             xs
             :keyf (fn [bound] [src distro bound])
             :weightf (fn [reqstate] (requirements-sum reqstate)))])
         (mapcat (fn [[src search-res]]
                   (let [{:keys [pruned calls]} (meta search-res)
                         p (count pruned)]
                     (println [:completed src  :pruned p :called calls
                               :reduced (gen/float-trunc (/ p (+ p calls)) 3)])
                     (for [[weight reqstate] (sort-by first (recover-raw search-res))
                           r                 (-> reqstate :supply)]
                       (assoc r :bound (reqstate :bound))))))
         (expand-missing xs))))

(defn requirements-contour-faster-async
  [proj xs  & {:keys [dtype search src-filter]
                   :or {search r/bisecting-convergence ;iterative-convergence
                        dtype  :proportional
                        src-filter (fn [_] true)}}]
  (let [tbls    (-> (a/load-requirements-project proj)      :tables)
        distros (into {} (->> (r/aggregate-distributions tbls :dtype dtype)
                              (filter (fn [[src _]]
                                        (src-filter src)))))
        peaks   (->>  (:DemandRecords tbls)
                      (tbl/table-records)
                      (filter #(and (:Enabled %)
                                    (distros (:SRC %))))
                      (r/demands->src-peaks))
        n       (atom (count peaks)) ;;revisit.
        src-distros->requirements  (r/requirements-by tbls peaks search n)]
    (->> distros
         (u/unordered-pmap
          (u/guess-physical-cores)
          (fn extrema-requirements [[src distro :as sd]]
            ;;for each [src distro] we want to compute a requirements analysis contour
            ;;for the points in xs.
            [src
             (extrema-search-generic
              (fn [[src distro bound :as sd]]
                (binding [r/*distance-function*    r/contiguous-distance
                          r/*contiguity-threshold* bound]
                  (assoc (second (src-distros->requirements sd)) :bound bound)))
              xs
              :keyf (fn [bound] [src distro bound])
              :weightf (fn [reqstate] (requirements-sum reqstate)))]))
         (mapcat (fn [[src search-res]]
                   (let [{:keys [pruned calls]} (meta search-res)
                         p (count pruned)]
                     (println [:completed src  :pruned p :called calls
                               :reduced (gen/float-trunc (/ p (+ p calls)) 3)])
                     (for [[weight reqstate] (sort-by first (recover-raw search-res))
                           r                 (-> reqstate :supply)]
                       (assoc r :bound (reqstate :bound))))))
         (expand-missing xs))))

;;testing

(comment

  (def path (io/file-path "~/workspacenew/notional/reqs-testdata-v7.xlsx"))
  ;;gives us table records with bound information now.
  ;;currently prints out pruning information as well.
  (def res (requirements-contour-faster path (range 30)))
  )
#_
(defn requirements-contour [proj xs]
    (let [tbls  (-> (a/load-requirements-project proj)
                    (:tables))]
      (vec (for [x xs]
             (binding [*distance-function* contiguous-distance *contiguity-threshold* x]
               {:bound x
                :requirement  (-> tbls
                                  (tables->requirements-async  :search bisecting-convergence)
                                  (requirements->table)
                                  (as-> res
                                      (tbl/conj-field [:bound (repeat (tbl/count-rows res) x)] res)))
                })))))
