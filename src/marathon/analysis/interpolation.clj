;;utils for 2d kriging interpolation...
(ns marathon.analysis.interpolation
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]))

;;https://github.com/haifengl/smile/blob/master/base/src/main/java/smile/interpolation/KrigingInterpolation2D.java

;;probably don't need anything fancy...
(m/set-current-implementation :vectorz)

;;I think we can just bolt this onto core.matrix linear-least-squares or svd...
(defprotocol IInteroplate2D
  (interpolate2d  [this  x  y]))

(defn pow2 ^double [^double x] (* x x))

(defn ->matrix [m n] (m/matrix (repeat m (m/array (repeat n 0.0)))))

;;ideally not using atoms for this, but meh.
(defn pow ^double [^doubles x1 ^doubles x2 ^doubles y ^double beta]
  (let [n     (alength x1)
        num   (atom 0.0)
        denom (atom 0.0)]
    (dotimes [i n]
      (doseq [j (range (inc i) n)]
        (let [d1 (- (aget x1 i)
                    (aget x1 j))
              d2 (- (aget x2 i) (aget x2 j))
              d  (+ (* d1 d1) (* d2 d2))
              rb (Math/pow d (/ beta 2.0))]
          (swap! num   + (* rb 0.5 (pow2 (- (aget y i) (aget y j)))))
          (swap! denom + (* rb rb)))))
    (if (zero? @denom)
      0
      (/ @num @denom))))

(defn variogram ^double [^double alpha ^double beta ^double r]
  (* alpha (Math/pow r  (/ beta 2.0))))

;; /** The first dimension of tabulated control points. */
;; private final double[] x1;
;; /** The second dimension of tabulated control points. */
;; private final double[] x2;
;; /** The linear weights. */
;; private final double[] yvi;
;; /** The parameter of power variogram. */
;; private final double alpha;
;; /** The parameter of power variogram. */
;; private final double beta;

;; public double interpolate(double x1, double x2)
;; {
;;  int n = this.x1.length;
;;  double y = yvi[n];
;;  for (int i = 0; i < n; i++) {
;;           double d1 = x1 - this.x1[i];
;;           double d2 = x2 - this.x2[i];
;;           double d = d1 * d1 + d2 * d2;
;;           y += yvi[i] * variogram(d);
;;           }
;;  return y;
;;  }
(defrecord kriging2d [^doubles x1 ^doubles x2 ^doubles yvi ^double alpha ^double beta]
  IInteroplate2D
  (interpolate2d  [this x y]
    (let [n (alength x1)
          res (atom (m/mget yvi n))]
      (dotimes [i n]
        (let [d1 (- x (aget x1 i))
              d2 (- y (aget x2 i))
              d  (+ (* d1 d1) (* d2 d2))]
          (swap! res + (* (m/mget yvi i) (variogram alpha beta d)))))
      @res)))

(defn ->interpolator2d [^doubles x1 ^doubles x2 ^doubles y ^double beta]
  (let [alpha (pow x1 x2 y beta)
        n     (alength x1)
        yv    (double-array (inc n))
        v     (->matrix (inc n) (inc n))]
    (dotimes [i n]
      (aset yv i (aget y i))
      (doseq [j (range i n)]
        (let [d1 (- (aget x1 i) (aget x1 j))
              d2 (- (aget x2 i) (aget x2 j))
              d  (+ (* d1 d1) (* d2 d2))
              var (variogram alpha beta d)]
          (m/mset! v i j var)
          (m/mset! v j i var)))
      (m/mset! v n i 1.0)
      (m/mset! v i n 1.0))
    (aset yv n 0.0)
    (m/mset! v n n 0.0)
    (->kriging2d x1 x2 (lin/least-squares v yv) alpha beta)))

(defn bounds [keyfs xs]
  (let [keyfs (if (vector? keyfs)
                (zipmap keyfs keyfs)
                keyfs)]
    (reduce (fn [acc x]
              (reduce-kv (fn blah [acc k f]
                           (let [res (f x)]
                             (if-let [bnd (acc k)]
                               (let [newbnd (if (< res (bnd 0)) (assoc bnd 0 res) bnd)
                                     newbnd (if (> res (bnd 1)) (assoc bnd 1 res) newbnd)]
                                 (if (identical? newbnd bnd)
                                   acc
                                   (assoc acc k newbnd)))
                               (assoc acc k [res res]))))
                         acc keyfs)) (zipmap (keys keyfs) (repeat
                                                           nil)) xs)))

(defn grid [data x-key y-key z-key & {:keys [bnds] :or
                                      {bnds (bounds [x-key
                                                     y-key]
                                                    data)}}]  
  (let [[xmin xmax] (bnds x-key)
        [ymin ymax] (bnds y-key)
        response (map z-key data)
        ;;If all responses are equal, the interpolation is too easy.
        equal-vals? (apply = response)
        first-z (first response)
        lerper (->interpolator2d
                (double-array (map x-key data))
                (double-array (map y-key data))
                (double-array (map z-key data))
                1.5)
        ]
    (for [i (range xmin (inc xmax))
          j (range ymin (inc ymax))]
      {x-key i y-key j z-key
       (if equal-vals?
         first-z
         (interpolate2d lerper i j))})))

;;simple testing..
(comment
(def data
  [{:x 0, :y 7, :z 0.353944544}
   {:x 0, :y 8, :z 0.290257918}
   {:x 1, :y 6, :z 0.34257919}
   {:x 1, :y 8, :z 0.39724104}
   {:x 1, :y 11, :z 0.518849521}
   {:x 2, :y 3, :z 0.29061109}
   {:x 2, :y 4, :z 0.378298823}
   {:x 2, :y 8, :z 0.462225179}
   {:x 2, :y 9, :z 0.571473099}
   {:x 3, :y 3, :z 0.316451056}
   {:x 3, :y 5, :z 0.441681637}
   {:x 3, :y 12, :z 0.706440131}
   {:x 4, :y 7, :z 0.592229147}
   {:x 4, :y 8, :z 0.668243659}
   {:x 4, :y 11, :z 0.786585298}
   {:x 5, :y 3, :z 0.467315354}
   {:x 5, :y 6, :z 0.709651935}
   {:x 5, :y 13, :z 0.876288708}
   {:x 5, :y 15, :z 0.923932882}
   {:x 6, :y 2, :z 0.577013776}
   {:x 6, :y 3, :z 0.56227162}
   {:x 6, :y 12, :z 0.89387339}
   {:x 7, :y 1, :z 0.575226827}
   {:x 7, :y 4, :z 0.597366133}
   {:x 7, :y 9, :z 0.840845889}
   {:x 7, :y 11, :z 0.920962668}
   {:x 8, :y 3, :z 0.695241283}
   {:x 8, :y 8, :z 0.913048969}
   {:x 8, :y 12, :z 0.94290986}
   {:x 9, :y 4, :z 0.601671403}
   {:x 9, :y 6, :z 0.849695586}
   {:x 9, :y 11, :z 0.994671133}
   {:x 9, :y 12, :z 0.990560292}
   {:x 10, :y 3, :z 0.835806697}
   {:x 10, :y 12, :z 0.978310502}
   {:x 10, :y 13, :z 1.0}
   {:x 10, :y 14, :z 1.0}
   {:x 11, :y 0, :z 0.7825653}
   {:x 11, :y 4, :z 0.89318489}
   {:x 11, :y 9, :z 1.0}
   {:x 11, :y 12, :z 1.0}
   {:x 12, :y 2, :z 0.861296272}
   {:x 12, :y 4, :z 0.908295282}
   {:x 12, :y 7, :z 0.951026563}
   {:x 12, :y 8, :z 1.0}
   {:x 13, :y 3, :z 0.883247636}
   {:x 13, :y 8, :z 1.0}
   {:x 13, :y 10, :z 1.0}
   {:x 13, :y 12, :z 1.0}
   {:x 14, :y 3, :z 0.96122522}
   {:x 14, :y 6, :z 1.0}
   {:x 14, :y 7, :z 1.0}
   {:x 14, :y 11, :z 1.0}
   {:x 15, :y 4, :z 0.994292237}
   {:x 15, :y 7, :z 1.0}
   {:x 15, :y 9, :z 1.0}
   {:x 15, :y 12, :z 1.0}
   {:x 16, :y 7, :z 1.0}
   {:x 16, :y 8, :z 1.0}
   {:x 16, :y 9, :z 1.0}]))
