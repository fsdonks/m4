;;utils for 2d kriging interpolation...
(ns marathon.interpolation
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as lin]))

;;https://github.com/haifengl/smile/blob/master/base/src/main/java/smile/interpolation/KrigingInterpolation2D.java

;;probably don't need anything fancy...
(m/set-current-implementation :vectorz)

;;I think we can just bolt this onto core.matrix linear-least-squares or svd...
(defprotocol IInteroplate2D
  (interpolate2d  [this  x  y]))

(declare kriging-lerp)


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
    (/ @num @denom)))

(defn variogram ^double [^double alpha ^double beta ^double r]
  (* alpha (Math/pow r  (/ beta 2.0))))

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


(comment ;;simple testing..
  (def i2d (->interpolator2d
            (double-array [1 2 3 4 5])
            (double-array [1 2 3 4 5])
            (double-array (map #(/ % 2.0) (range 5)))
            1.5))
  (->> (for [i (range 10) j (range 10)] [i j (interpolate2d i2d i j)])
       (partition 10))
  )
