;;A temporary namespace for a simple simulated annealer.  While I get the opt 
;;stuff ironed out, this will serve in the meantime.  This is basically a 
;;subset of stuff ripped from spork.opt.annealing  
(ns marathon.processing.stoke.dumbanneal
  (:require [spork.util.numerics :refer :all]))

(defn midpoint [x y] 
  (+ x (/ (-  y x) 2.0)))


(defn boltzmann-sample
  "Samples from the Boltzmann probability distribution.  oldcost is equivalent
   to the previous 'energy state', while newcost is the new candidate energy
   state.  They are named cost since we use cost functions as energy analogues.
   t is temperature.  As t decreases, the probability of accepting 'worse' 
   energy states - higher costs - decreases rapidly.  At higher temperatures, 
   the disparity between energy states has less influence on the probability.
   As t approaches infinity, the contribution of the exponential term decreases, 
   approach a p of 0.5."
  ([t delta]  (/ 1.0 (+ 1.0 (Math/pow E (/ delta t)))))
  ([t oldcost newcost]
    (/ 1.0 (+ 1.0 (Math/pow E (/ (- newcost oldcost) t))))))

(defn- boltzmann-accept?
  "Use the Boltzmann energy equation to determine probability of accepting 
   a poorer solution at a given temperature t.  Better solutions are accepted
   with P = 1.0, while poorer solutions are accepted with P = e^(-delta / t)"
  [t oldcost newcost] 
  (let [delta (- oldcost newcost)]
    (if (pos? delta) true 
      (<= (rand) (boltzmann-sample t (* -1.0 delta))))))

(defn geometric-decay 
  "Standard decay function for simulated quenching.  Each temperature decay 
   is proportional to the decay rate.  Yields a function that decays the 
   current temperature at each step."
  [decay-rate] 
  (assert (and (> decay-rate 0) (<= decay-rate 1)) 
          "decay-rate must be between (0,1]")
  (fn [t] (* decay-rate t)))

(defn asa-dist
  "Given a uniform random variate, uv, and a temperature parameter, 
   samples returns a value between [-1 1].  As temperature decreases, the 
   values pack really tightly around 0.0, although, like the Cauchy distribution
   we may still see seemingly large, but infrequent, jumps across the parameter
   space."
  ([temp y]
    (let [dir  (Math/signum (- y 0.5))]
      (* dir 
         temp 
         (- (Math/pow (+ 1.0 (/ 1.0 temp)) (Math/abs (- (* 2.0 y) 1.0)))
            1.0))))
  ([temp] (asa-dist temp (rand))))

(defn asa-stepper 
  "Given a numeric range, defined by lower and upper, returns a function 
   that performs steps across the range, yielding values between [lower upper] .
   Uses a temperature parameter, temp, and the value of a previous value, 
   x0, to compute x."
  [lower upper & {:keys [step-func] :or {step-func asa-dist}}]
  (let [width (- lower upper)
        between? (fn [^double x] (and (>= x lower) (<= x upper)))
        take-step!   (^double fn [^double temp] (* (step-func temp) width))]
    (^double fn [^double temp ^double x0]
      (loop [step (+ x0 (take-step! temp))
             n    0]
        (if (between? step) step
            (if (> n 30) 
              (throw (Exception. "Problem in asa-stepper, too many reps."))
            (recur (+ x0 (take-step! temp)) (unchecked-inc n))))))))

;;Simulated Annealing Parameters
;;==============================
;;In simulated annealing, we use a temperature analogue, temp, to 
;;provide a measure of how 'hot' the ambient annealing process is, 
;;to allow our analagous search particles to move and collide at a 
;;higher velocity, this encouraging more chaos, and more exploration 
;;of the search space.  As the search proceeds, the decay function is 
;;applied to the temperature 
(defrecord sa-params 
    [decay-function temp tmin itermax equilibration accept?])

;;Simulated annealing just overrides the default acceptance criterion,
;;and uses the Boltzmann energy equation to bias the search toward
;;exploration early, and exploitation as the search proceeds.
;;__blank-sa-params__ provides a sane set of defaults for simulated
;;annealing, and will garantee that a solve will terminate.
(def blank-sa-params 
  (->sa-params 
    (geometric-decay 0.9) 
    1000000 0.00000001 1000 1
    (fn [incumbent candidate env]
      (let [t            (get (core/solve-state env) :t)
            old-cost     (core/candidate-cost incumbent)
            new-cost     (core/candidate-cost candidate)]
        (boltzmann-accept? t old-cost new-cost)))))

(defn simple-anneal 
  [init-solution cost-function step-function & 
   {:keys [t0 tmin itermax equilibration accept? init-cost]
      :or {t0 100000 tmin 0.0000001  itermax 100000  equilibration 1  
           accept? boltzmann-accept?  init-cost (cost-function init-solution)}}]
  (loop [temp t0
         n 1
         i 0
         converged? false
         current-sol  init-solution
         current-cost init-cost          
         best-sol     init-solution
         best-cost    init-cost]
    (if converged? {:temp temp :n n :i i :current-solution current-sol
                    :best-solution best-sol :best-cost best-cost }
      (let [temp (if (= n 0) (* 0.9 temp) temp)
            n    (if (= n 0) equilibration)]
        (if (or (< temp tmin) (> i itermax)) 
          (recur temp n i true current-sol current-cost best-sol best-cost) ;exit
          (let [new-sol (step-function current-sol)
                new-cost (cost-function new-sol)]
            (let [n (dec n)
                        i (inc i)]
              (if (boltzmann-accept? temp new-sol new-cost)
                (recur temp n i converged? 
                       new-sol new-cost (if (< new-cost best-cost) new-sol best-sol)
                       (min new-cost best-cost))))))))))
