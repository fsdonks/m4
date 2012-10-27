;(ns marathon.sampler)

;a dumb script to rip a csv file, assumably of scheduled records, and turn 
;it into a rasterized schedule.
;Note ... if you just copy and paste text from excel, it's treated as tab
;delimited.
(require '[clojure [string :as strlib]])
(defn- parse-string [value]
  (try (Integer/parseInt value)
    (catch NumberFormatException _
      (try (Double/parseDouble value)
        (catch NumberFormatException _ value)))))

(def re-tab (re-pattern (str \tab)))

(def split-by-tab #(strlib/split % re-tab))
(defn tabstream->table [s]
  (let [lines (strlib/split-lines s )]
    {:fields (split-by-tab (first lines ))
     :records (vec (map (comp #(map parse-string %) split-by-tab)
                      ( rest lines )))}))
(defn raw->vecmap
  "Derives a sequence of indexed vectors from a list of samples. Samples are
   assumed to represent a flattened list of vector pairs."
  [raw]
  (->> raw
		(partition
		(map-indexed (fn [i v] [i v]))
		(into (sorted-map)))))

(defn intersects?
  "Detect whether time t falls on or between a time interval defined by
   start and duration."
  ([t start duration]
    (and (>= t start ) (<= t (+ start duration))))
  ([t [start duration]] (intersects? t start duration)))

(defn quarter->day
  "Convert a quarter into a mul tipl e of 90 days."
  [n] (inc (* 90 n)))

(defn day->quarter [t] (inc (quot t 90)))
(defn quarters
  "Fetch n quarters, in days."
  [n] (map quarter->day (range n)))

(defn sample
  "This is a naive sampling funct ion. It'll work for our purposes. If we
   have thousands of demands, it might be bad, but it's fine for now. Scans
   the vectors defined in vecmap, to determine which ones intersect t.  All
   intersections are collected."
  [t vecmap ]
  (reduce (fn [acc x] (if (intersects? t (get vecmap x ))
                        (assoc acc x (get vecmap x))
                         acc)) {} (keys vecmap)))
(defn get-series
  "Analyzes a sequence of samples, and a set of vectors, returning the set of
   intersecting vectors for each sample time."
  [samples vecmap]
  (for [s samples ]
          {:t s :intersections (keys (sample s vecmap))}))

(defn series->records
  "For a series of samplings, returns a tabular represntation of each active
   record. The intent is to allow the records to be joined or looked up in
  some external manner by their unique record index."
  [series]
  (reduce
    (fn [acc s ]
      (let [t (:t s)
            indices (:intersections s )]
        (reduce conj acc (map (fn [i] [t i]) indices))))
    [] series ))

(defn records->table
  "Spit [t i] records into a tabular representation, converting time back to
   quarters in the process."
  [recs ]
  (concat [["Quarter" "Index"]]
          (map (fn [[ t i ]] [(day->quarter t ) i ]) recs)))

(defn tbl->tabdelimited
  "Render a sequence sequences into a valid CSV string . "
  [tbl]
  (reduce (fn [acc rec]
            (str (apply str acc 
                    (interleave rec (repeat \tab))) \newline)) "" tbl ))

(defn raw->raster
  "Encapsulate all the processing to convert a raw sequence of segements,
   and a number of samples, into a rasterized sampling."
  [raw samples]
  (->> (get-series samples (raw->vecmap raw))
    (series->records )
    (records->table )
    (tbl->tabdelimited)))

(defn make-rasterfile
  "Encapsulate process of rasterization. Given a flattened sequence of
   vectors, which represent segments of time (i.e. events), samples the 
   segments across a number of quarters"
  [raw path & {:keys [samples] :or {samples (quarters 28)}}]
  (hock path (raw->raster raw samples)))
