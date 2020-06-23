(ns requirements.sensitivity
  (:require [spork.util
               [io :as io]
             [table :as tbl]]
            [marathon.analysis.requirements :as r]))

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
(defn extrema-search-sparse [f xs]
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
