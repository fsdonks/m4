;;This probably general enough to be in another lib.
;;For now, we've established the basics though.
(ns marathon.analysis.nolh)
;;a simple transcription of the coded
;;NOLH for 12-16 factors from:

(def coded
  [47	4	24	22	9	50	52	32	63	47	36	61	13	41	53	45
   62	47	8	28	23	17	36	49	47	60	50	32	16	23	23	11
   58	24	62	15	20	56	11	30	27	38	52	57	31	34	18	58
   42	58	47	30	5	28	18	18	9	64	59	41	6	14	56	22
   60	31	13	2	7	11	17	40	40	32	5	55	42	39	46	52
   35	60	16	32	11	43	10	56	58	8	27	51	58	5	12	4
   50	13	35	3	17	22	58	28	11	29	23	59	62	48	7	36
   53	50	60	21	25	60	46	2	17	21	2	42	47	3	36	13
   45	3	2	53	27	40	26	7	52	20	55	26	22	16	34	65
   63	45	32	50	1	19	60	25	44	1	44	3	28	55	2	31
   34	2	63	35	14	29	21	45	2	16	47	13	15	4	16	54
   64	34	45	60	12	5	25	53	21	25	35	17	30	38	60	15
   36	15	22	42	26	32	13	19	62	62	10	4	64	12	41	48
   51	36	28	58	18	63	4	1	34	35	17	20	34	44	26	26
   38	22	51	62	29	15	31	61	10	54	1	29	63	20	38	56
   44	38	36	47	10	62	65	37	30	53	24	8	45	64	55	29
   56	29	26	9	44	65	51	63	38	39	21	19	27	42	44	25
   37	56	18	23	38	30	54	52	24	48	8	1	1	6	24	46
   48	26	37	20	51	58	3	27	35	52	32	6	14	57	3	34
   40	48	56	5	36	12	29	12	53	57	4	28	12	17	31	47
   54	14	27	7	64	25	2	46	20	7	28	12	40	53	65	27
   52	54	1	11	34	46	32	58	12	23	60	21	48	10	29	64
   65	27	52	17	63	9	47	9	37	22	37	18	37	36	14	17
   39	65	54	25	45	35	38	15	65	5	46	14	56	15	61	60
   41	17	7	39	53	48	61	11	7	11	18	43	9	1	58	24
   49	41	11	65	50	13	42	24	25	30	3	56	23	37	4	61
   55	7	49	52	35	59	22	62	50	17	15	39	20	8	27	9
   59	55	41	54	60	21	9	43	48	24	25	64	5	47	45	38
   61	20	9	40	42	14	27	6	15	56	57	36	59	21	47	7
   46	61	23	48	58	64	23	35	5	51	40	44	55	40	17	43
   43	9	46	37	62	27	50	50	60	63	53	35	49	7	9	16
   57	43	61	56	47	42	59	44	43	40	54	50	41	35	51	63
   33	33	33	33	33	33	33	33	33	33	33	33	33	33	33	33
   19	62	42	44	57	16	14	34	3	19	30	5	53	25	13	21
   4	19	58	38	43	49	30	17	19	6	16	34	50	43	43	55
   8	42	4	51	46	10	55	36	39	28	14	9	35	32	48	8
   24	8	19	36	61	38	48	48	57	2	7	25	60	52	10	44
   6	35	53	64	59	55	49	26	26	34	61	11	24	27	20	14
   31	6	50	34	55	23	56	10	8	58	39	15	8	61	54	62
   16	53	31	63	49	44	8	38	55	37	43	7	4	18	59	30
   13	16	6	45	41	6	20	64	49	45	64	24	19	63	30	53
   21	63	64	13	39	26	40	59	14	46	11	40	44	50	32	1
   3	21	34	16	65	47	6	41	22	65	22	63	38	11	64	35
   32	64	3	31	52	37	45	21	64	50	19	53	51	62	50	12
   2	32	21	6	54	61	41	13	45	41	31	49	36	28	6	51
   30	51	44	24	40	34	53	47	4	4	56	62	2	54	25	18
   15	30	38	8	48	3	62	65	32	31	49	46	32	22	40	40
   28	44	15	4	37	51	35	5	56	12	65	37	3	46	28	10
   22	28	30	19	56	4	1	29	36	13	42	58	21	2	11	37
   10	37	40	57	22	1	15	3	28	27	45	47	39	24	22	41
   29	10	48	43	28	36	12	14	42	18	58	65	65	60	42	20
   18	40	29	46	15	8	63	39	31	14	34	60	52	9	63	32
   26	18	10	61	30	54	37	54	13	9	62	38	54	49	35	19
   12	52	39	59	2	41	64	20	46	59	38	54	26	13	1	39
   14	12	65	55	32	20	34	8	54	43	6	45	18	56	37	2
   1	39	14	49	3	57	19	57	29	44	29	48	29	30	52	49
   27	1	12	41	21	31	28	51	1	61	20	52	10	51	5	6
   25	49	59	27	13	18	5	55	59	55	48	23	57	65	8	42
   17	25	55	1	16	53	24	42	41	36	63	10	43	29	62	5
   11	59	17	14	31	7	44	4	16	49	51	27	46	58	39	57
   7	11	25	12	6	45	57	23	18	42	41	2	61	19	21	28
   5	46	57	26	24	52	39	60	51	10	9	30	7	45	19	59
   20	5	43	18	8	2	43	31	61	15	26	22	11	26	49	23
   23	57	20	29	4	39	16	16	6	3	13	31	17	59	57	50
   9	23	5	10	19	24	7	22	23	26	12	16	25	31	15	3])

(def cols
  (->> coded
       (partition 16)
       (reduce (fn [[i j k] [x y z]]
                 [(conj i x)
                  (conj j y)
                  (conj k z)]) [[] [] []])))


(defn project [lower upper col]
  (let [span   (- upper lower)
        levels (count col)
        denom  (dec levels)]
    (mapv (fn [v]
            (let [raw (+ lower
                         (/  (* (dec v) span) denom))]
              (Math/round (double raw))))
          col)))

(defn nolh-65-by-16
  "Given a sequence (or map) of {factor-name [lower upper]},
   where lower defines the lower bound and upper defines the
   upper bound, computes the NOLH projection using the design from
   'NOLH for up to 16 factors' from
    Sanchez, S. M.  2011.  NOLHdesigns spreadsheet.
    Available online via http://harvest.nps.edu/
    [accessed 09/21/2020]
   The input sequence or map must be <= 16 entries.

   Returns a sequence of {:name factor :points xs}
   for each design with the designs of the NOLH
   projected into the space defined by lower and upper
   for each factor."
  [name-bounds]
  (assert (<= (count name-bounds) 16)
    "There must be <= 16 factors for the nolh-65-by-16 design!")
  (map-indexed (fn [idx [k [lower upper]]]
                 {:name   k
                  :points (project lower upper (nth cols idx))})
               name-bounds))

;;borrowed from clojure.math.combinatorics

;;   Copyright (c) Mark Engleberg, Rich Hickey and contributors. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;;; combinatorics.clj: efficient, functional algorithms for generating lazy
;;; sequences for common combinatorial functions.

;; by Mark Engelberg (mark.engelberg@gmail.com)
;; Last updated - July 24, 2019
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn full-factorial-records [name-bounds]
  (let [names (map first name-bounds)
        get-record (fn [xs]
                     (zipmap names xs))]
    (->> (for [[name [lower upper]] name-bounds]
           (range lower (inc upper) 1))
         (apply cartesian-product)
         (map get-record))))

(defn records [kvs]
  (let [ks     (mapv :name kvs)
        points (mapv :points kvs)
        n      (count (first points))
        get-record (fn [idx]
                     (zipmap ks
                             (map (fn [ps] (nth ps idx)) points)))]
    (map get-record (range n))))

(defn designs
  "Higher-order function that returns a sequence of {k1 v1 k2 v2 ... kn vn}
   for each point in the NOLH design defined by the input
   name-bounds, [k [lower upper]].  If the dimensions of the input are small
   enough that we do not exceed 65 levels, then we will return a full
   factorial design.  We assume 3 factors for now..."
  [name-bounds & {:keys [levels] :or {levels 65}}]
  (let [size (reduce (fn [acc [factor [lower upper]]]
                       (* acc (inc (- upper lower)))) 1 name-bounds)
        _     (println size)]
    (->>   (if (< size levels)
             (full-factorial-records name-bounds)
             (records (nolh-65-by-16 name-bounds))))))

;;Note: additional designs are available if desired.
