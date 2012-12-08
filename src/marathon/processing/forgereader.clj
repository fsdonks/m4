;A set of utilities for processing data; derived from an ad-hoc script.
;FORGE data comes in a couple of important tables, specifically phases, 
;tracks, and events.  The forgereader provides a library for processing these
;tables, and producing 
(ns marathon.processing.forgereader
  (:require [util [table :as tbl]]
            [util [bridging :as br]])

(defn SRCtext->table  
  "Rips text from string txt, parsing under the assumption that certain 
   alphanumeric codes for SRCs should be parsed as strings, not scientific
   notated numerals."
  [txt]
  (tbl/tabdelimited->table txt :parsemode :noscience))

;NOTE remove this guy....used for testing.
(def testpath 
  (relative-path :docs
     ["TAA 15-19" "Unconstrained Runs" "Derived Data" "Demand"]))

(defn load-tables
  "Fetches canonical tables for phases, tracks, and event information from a 
   root directory, returning a map of parsed tables with corresponding entries."
  ([path]    
    (let [[p t e] (map #(SRCtext->table (slurp (relative-path
                                                 (as-directory path) (list %))))
                       ["phases.txt" "tracks.txt" "events.txt"])]
      {:phases p :tracks t :events e}))
  ([] (load-tables testpath)))

;we primarily use two foreign keys when joining forge data.
;TP and a generic index (a number) .
;TP is the time phase, and shows up in the timephase table, and the
(defn tptable->phasemap
  "Converts a time phase table, or TP table, to a sequence of phases. Since
   TPs are reported in End Days, we stitch together the timings from each
   preceding phase. In the case of an initial time phase ending on 1, we
   prepend an implicit phase that starts at 0."
  [tptbl]
  (let [{:keys [phases timeperiods ends]}
        (reduce (fn [{:keys [phases timeperiods ends]} rec]
                  {:ends (conj ends (get rec "TP End Day"))
                   :phases (conj phases (get rec "Sub-Phase"))
                   :timeperiods (conj timeperiods (get rec "TP"))})
                {:ends [] :phases [] :timeperiods []}
                (tbl/record-seq tptbl))
        schedule (let [raw (partition 2 1 ends)
                       n (ffirst raw)]
                   (if (not= n 0)
                     (cons (list 0 n) raw)
                     raw) ) ]
    {:phases (zipmap phases schedule)
     :periods (zipmap timeperiods phases)}))

;the big change here is that for each track, we're building a phasemap ...
;we need a function that consumes a sequence of change events ...
;and maps the changes to a phase record.
;If we want to compress a track, we basically drop time periods ...
;It's an extension to drop-blankevents ...
;Given an event-record, we've dropped blank timeperiods ...
(defn drop-blankevents
  "Cleans an event record of nil or blank entries."
  [event-record]
  (reduce (fn [acc k]
            (if (number? (get event-record k))
              acc
              (dissoc acc k))) event-record (keys event-record)))
(defn tpkey->time
  "Converts a timpphase key into a time represntation."
  [tpkey] (Integer/parselnt (clojure.string/replace tpkey "TP " "" )))

(defn get-time
  "Fetches the time interval associated with the time phase key."
  [[tpkey _]] (tpkey->time tpkey "TP " "" ))

(defn compare-time
  "Compares the time intervals between two time phase keys."
  [tp1 tp2] (compare (tpkey->time tp1) (tpkey->time tp2)))

(defn tp->timing
  "Maps a time phase key into a [tstart tstop] time interval."
  [tpkey {:keys [periods phases]}]
  (get phases (get periods tpkey)))

(defn eventrecord->schedule
  "Convert an event record into a schedule of events."
  [rec]
  (into (sorted-map-by compare-time) (drop-blankevents rec)))

(defn index-changes
  "For a collection of events, determines where changes occur between contiguous
   entries' values."
  [events]
  (->> (reduce
         (fn [[i vprev id phases] v]
           (if (not= v vprev) inew phase
             (let [new-id (inc id)
                   newphase {new-id {:samples [i] :data v}}]
               [(inc i) v new-id (merge phases newphase)])
             (let [phase (get phases id)
                   samples (get phase :samples [])]
               [(inc i) vprev id (assoc phases id
                              (assoc phase :samples (conj samples i)))])))
         [0 nil -1 (sorted-map)] events)
    last))

(defn compress-schedule
  "Compresses a schedule of events, eliminating redundant event values, where 
   the quantity associated with contiguous events is identical.  The resulting 
   compressed schedule is the minimal amount of information needed to cover the
   inflection points in the signal."
  ([schedule lbl]
    (let [tps (vec (keys schedule))
          events (vec (vals schedule))
          groups (index-changes events)]
      (reduce (fn [acc v] (conj acc v)) []
              (for [[phase {:keys [samples data]}] groups]
                (let [to (nth tps (first samples))
                      tf (nth tps (last samples))]
                  [(str lbl phase \space \[ to " - " tf \])
                   [to tf]])))))
  ([schedule] (compress-schedule schedule "Derived Phase ")))

(defn compress-phasemap
  "Compresses the nominal phase mapping based on the underlying schedule."
  [compressed-schedule phasemap]
  (let [phases (into {} (for [[phase [tpstart tpend]] compressed-schedule]
                          [phase [(first (tp->timing tpstart phasemap))
                                  (fnext (tp->timing tpend phasemap))]]))
        periods (->> (mapcat (fn [[phase [tpstart tpend]]]
                               [[tpstart phase] [tpend phase]])
                             compressed-schedule)
                  (reduce (fn [acc [k v]] (assoc acc k v)) {}))]
    {:phases phases
     :periods periods}))

(defn sample-demand
  "Produces a set of demand records for each period and phase according to the
   schedule."
  [group schedule {:keys [phases periods] :as phasemap} timeperiod]
  (let [p (get periods timeperiod)
        op (str group \space p)
        [tstart tfinish] (get phases p)]
    {:phase p
     :start tstart
     :duration (- tfinish tstart)
     :vignette op
     :operation OJ?
     :group group}))

(defn expand-track
  "Turn one track record into N events. For each event, there is a start,
   duration, stop. Assuming we have a phasemap that maps trackrecord i ->
   timephase, we use phasemap to create a new record with start and duration.
   If compress is true, the data from the event record will define phasing,
   where phases consist of changes in the event data. Multiple time periods
   with identical data will be compressed into a single, derived phase."
  [eventrecord trackrecord phasemap & {:keys [group compress]
                                       :or {group "Anonymous Surge Event"
                                            compress false}}]
  (let [schedule (eventrecord->schedule eventrecord)
        [ts phasemap] (if compress
                        (let [s (compress-schedule schedule)]
                              [(concat (map (comp first fnext) s))
                               (compress-phasemap s phasemap)])
                        [(keys schedule) phasemap])
        make-record (partial sample-demand group schedule phasemap)]
    (map #(merge trackrecord (make-record %) {:quantity (get schedule %)}) ts)))

;define a mapping of forge fields to our desired format:
(def forgetemplate
  [["Type" "Type" "DemandRecord"]
   ["Enabled" "Enabled" "True"]
   ["Priority" "Priority" 1]
   [:quantity "Quantity"]
   ["DemandIndex" "DemandIndex" 0]
   [:start "StartDay"]
   [ :duration "Duration"]
   ["Overlap" "Overlap" 45]
   ["SRC"]
   ["SourceFirst" "SourceFirst" "Uniform"]
   [:group "DemandGroup"]
   [:vignette "Vignette"]
   [:operation "Operation"]
   ["ARFORGEN" "Category" ]])

;define a data bridge that uses our mapping.  
(br/defbridge forge->demand forgetemplate)

(defn tbls->demandrecords
  "Given tbls, a map of tables with keys :phases :events :tracks, 
   a demand group, and an optional boolean indicating whether we should
   compress the tracks, produces a set of demandrecords in the format 
   described by the forge->demand databridge."
  [tbls group & [compress]]
  (let [phasemap (tptable->phasemap (:phases tbls))]
    (->> (mapcat #(expand-track %1 %2 phasemap :group group :compress compress)
                 (tbl/record-seq (:events tbls)) 
                 (tbl/record-seq (:tracks tbls)))
      (map (partial bridge/translate-map (br/get-bridge :forge->demand))))))

(comment 
  (def tbls (load-tables))
  (def testtrack (first (tbl/record-seq (:tracks tbls))))
  (def testevent (first (tbl/record-seq (:events tbls))))
  (def testmap (tptable->phasemap (:periodtable tbls)))
;(defn res []
;  (loop [acc {}
;         remaining forgetemplate]
;    (if-let [[k vI v2] (first remaining)]
;      (recur (assoc acc k [vI v2]) (rest remaining))
;      acc)))

;expand all of our records ....
; (defn build-tracks [srctable tracktable]
;   (map-indexed
)