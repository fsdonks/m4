;;Defines a prototype script that reads information from an M4
;;project to derive demandd peaks, supply, and then uses that
;;information to build a set of experiments that are then processing
;;via the proc.risk response suface functions.  Also contains
;;functionality to quickly emit an uber-table of all the experimental
;;designs, tha is designs in policy/supply space by component,
;;along with experimental response surfaces (%fill).

;;The next use for this is to leverage the established data protocols,
;;and swap out static (algebraic) analysis for the
;;design->performance functions for actual M4 runs.

;;Note: the script may also serve as a pedagocial tool for
;;exploiting existing libraries (spork, proc, marathon.analysis) for
;;idiomatic analytic tasks....like building and defining lots of
;;experimental designs.  For reference, I performed the entirety
;;of this development and analysis from within the stock M4 environment.
;;New users could easily do the same.  The dev experience was
;;actually pretty nice!  One may consider the presence of spork to
;;provide a fairly batteries-included experience (along with clojure.coree
;;and numerous other dependencies shipped along with M4).
(ns med.script
  (:require [proc [risk :as risk]
                  [demandanalysis :as da]
                  [charts]]
            [marathon.schemas :as schemas]
            [spork.util [io :as io]
             [table :as tbl]
             [general :as gen]]
            [spork.util.excel.core :as xl]
            [marathon.analysis :as analysis]
            [incanter.core :as i]))
;;this is where I stuck my test data. io/file-path will handle paths in
;;a smart, portable way, even path fragments.
#_(def root (io/file-path "~/Documents/med"))
#_(def root (io/file-path "~/repos/notional/"))
(def root (io/file-path "~/workspacenew/notional/"))

;;Temporary hack around Craig's function to allow me to just use M4's project
;;reading functions to shove data directly into peak compoutation.
;;We should probably extend this to work with projects, to support project
;;analysis.
(defn peaks-from
  "Given the path to a Marathon audit trail, compute peak-times-by-period
   for the enabled demand records.  Supply an optional demand-filter for the
   demand records."
  [path & {:keys [group-fn demand-filter periods demands]
           :or   {group-fn (fn [s] "All")
                  demand-filter (fn [r] true)}}]
  (let [demands (or demands (->> (proc.util/enabled-demand path)
                                 (filter demand-filter)))
        periods (or periods (proc.util/load-periods path))
        peakfn  (fn [{:keys [actives]}] (apply + (map :Quantity actives)))]
    (da/peak-times-by-period
     (fn [r] (group-fn (:SRC r))) demands periods :StartDay :Duration peakfn)))

(defn project->demand-peaks
  "Given a maratho project p, which is typically just a map with a
   map of spork.util.tables under :tables, pipes the tables to
   the aforementioned peaks-from wrapper"
  [p & {:keys [peak-filter] :or {peak-filter identity}}]
  (let [[periods demands] (->> p
                               :tables
                               ((juxt :PeriodRecords :DemandRecords))
                               (mapv tbl/table-records))
        periods (filter #(not= (:Name %) "Initialization") periods)]
    (->> (peaks-from nil :group-fn identity :periods periods :demands demands)
         (filter peak-filter))))

(defn project->surge-peaks
  "Conveniently extract only the surge peaks from a marathon project."
  [p]
  (project->demand-peaks p :peak-filter #(= (:period %) "Surge")))

;;define a path to the sample workbook.
#_(def wbpath (io/file-path root "MARATHON_WKBK_21-25_Med.xlsx"))
(def wbpath (io/file-path root "base-testdata-v7.xlsx"))

;;notional testdata
(def test-srcs
  #{"08317K000" "08979R000" "08453R000" "08976R000" "08988R000"
    "08668R000" "08528RA00" "08457R000" "08429R000" "08977R000"
    "08641RB00" "08949R000" "08670R000" "08978R000" "08488K000"
    "08473R000" "08527AA00" "08480K000" "08300R000" "08460R000"
    "08430R000" "08485R000" "08528RB00" "08641RA00" "08567RA00"
    "08420R000" "08640R000"})

;;Convenience function to load up the workbook on demand, without being
;;tied to a reload of the script.  Note the use of analysis/load-project:
;;the marathon.analysis namesapce has many useful high-level API functions
;;for working with inputs, manipulating them, simulating, collecting history,
;;etc. It's a good namespace to know!
(defn test-proj []
  (let [proj (analysis/load-project wbpath)
        tbls (:tables proj)
        f    (analysis/filter-srcs test-srcs)]
    (->> tbls
         f
         (assoc proj :tables))))

;;example usage of surge peaks.
#_(def peaks (project->surge-peaks proj))
  
(defn build-cases
  "Primary workhorse for deriving experiment cases from inputs.
   The idea heer is to manage everything in canonical marathon inputs
   (a project) and then leverage existing functions to do the work for us.
   In this instance, we wish to scrape the supply and demand to compute a series
   of design points for supply/policty/demand experiments.
   The minimal information is the src, peak-demand, and supply by component.
   Returns a sequencce of {:keys [src demand ac-supply rc-supply ng-supply]} maps."
  [proj]
  (let [compo-key #(case (name %)
                     "AC" :ac-supply
                     ("AR" "RC") :rc-supply
                     "NG" :ng-supply
                     (throw (ex-info "bad-compo!" {:compo %})))]
    (for [peak (project->surge-peaks proj)]
      (let [src (:group peak)
            supply (->> proj
                        :tables
                        :SupplyRecords
                        tbl/table-records
                        (filter #(= (:SRC %) src))
                        (group-by :Component))
            title   (-> supply vals ffirst :OITitle)]
        (into {:src src
               :oititle title
               :demand (:peak peak)
               :ac-supply 0 :rc-supply 0 :ng-supply 0}
              (for [[compo [r]] supply]
                [(compo-key compo) (:Quantity r)]))))))

;;Policy definitions that conform to the rough sketch presented in
;;proc.risk, with the departure tht they have a :policy key with the
;;name of the policty.

;;policies are in months, although it doesn't matter as long as proportions
;;are consistent....will need to adapt for M4 though for consistentcy.
;;What we'd like to eventually do is define coercions for M4 policies/policy
;;records.

;;these are duplicates from proc.risk, for now.
(def rc10 {:policy :rc10
           :bog 9
           :cyclelength 9
           :overlap 0
           :mob 0})

(def ac10 {:policy :ac10
           :bog 9
           :cyclelength 9
           :overlap 0
           :mob 0})

(def ac11 {:policy :ac11
           :bog 9
           :cyclelength 18
           :overlap 0
           :mob 0})

(def ac12 {:policy :ac12
           :bog 9
           :cyclelength 27
           :overlap 0
           :mob 0})

(defn simple-dwell-ratio
  "Computs the dwell portion of bog:dwell,
   and truncates floating-point results to
   2 decimal places.  The degenerate case of
   1:0 is defined as 0."
  [bog cyclelength]
  (let [dwell (- cyclelength bog)]
    (if (pos? dwell)
      (gen/float-trunc (/ 1.0 (/ bog dwell)) 2)
      0)))

;;var to control how fine our steps are.  defaults
;;to 1 month increments.
(def ^:dynamic *step-size* 1)

(defn policy-range
  "Given a seed policy definition (assumably a 1:0 type),
  generates a sequence of policies with increased dwell times
  until a target bound is met via proc.risk/policy-range.
  The default for our experiments is a 1:8 bog:dwell, so 1/9 works."
  [seed]
  (for [p (risk/policy-range seed :bound 1/9 :step *step-size*)]
    (let [dwellrat   (simple-dwell-ratio (:bog p) (:cyclelength p))
          policyname (str (subs (name (:policy p)) 0 2) "-1:" dwellrat)]
      (assoc p :policy policyname))))

;;we just expand out our base case into multiple cases based
;;on an initial hack....future efforts will generalize this better.
(defn case->risk-cases
  "We'd like to expand our baseline map of supply/demand into policy
  cases, or so-called risk cases.  Given the original design,
  there are actually 3 trends we'd also like to compute, where
  either supply or policy is deviated relative to a baseline of current
  supply."
  [{:keys [src demand ac-supply rc-supply ng-supply oititle] :as case}]
  (let [base {:demand demand :src src :oititle oititle
              :ac-policy ac10 :rc-policy rc10 :ng-policy rc10
              :ac-supply ac-supply :rc-supply rc-supply
              :ng-supply ng-supply}
        base+  (risk/grow-by base {:ac 1 :rc 1 :ng 1})
        base11 (risk/modify-policy base :ac ac11)]
    [(assoc base :case :current)
     (assoc base+ :case :growth)
     (assoc base11 :case :current-11)]))


;;now we want the cartesian product of our risk cases with a bunch
;;of policty modifictions
(defn risk-case->policy-excursions
  "Convenience wrapper to inject policy excursions for the rc/ng
   access policies from a base risk-case record."
  [r]
  (for [p (policy-range (:rc-policy r))]
    (assoc r :ng-policy p
             :rc-policy p)))

;;For clean output later, we leverage the spork.util.table facilities
;;for spitting outputs and defining field order.  We'll build an
;;ubertable later with information for every experiment. These are the
;;fields of that table. We don't "have" to have a field order, but if we
;;don't the order is arbitrary and could be confusing.
(def risk-fields
  [:idx :src :oititle :case :demand :total-supply :rounded-supply
   :fill :rounded-fill :lower-fill :upper-fill
   :ac-policy :rc-policy :ng-policy
   :ac-supply :ac-expected :ac-bog
   :ac-cyclelength :ac-mob :ac-overlap :ac-dwell-ratio

   :ng-supply :ng-expected :ng-bog
   :ng-cyclelength :ng-mob :ng-overlap :ng-dwell-ratio
   
   :rc-supply :rc-expected :rc-bog
   :rc-cyclelength :rc-mob :rc-overlap :rc-dwell-ratio])

;;Again, for purposes of creating our table, we take the nested-map
;;representation and unpack it into a "wide" table.  This aux function
;;helps us do that.
(defn risk-case->record [{:keys [ac-policy rc-policy ng-policy] :as r}]
  (merge (dissoc r :ac-available :rc-available :ng-available)
         {:ac-policy (:policy ac-policy)
          :ac-bog    (:bog ac-policy)
          :ac-dwell  (:dwell (:ac-available r))
          :ac-cyclelength (:cyclelength ac-policy)
          :ac-overlap  (:overlap ac-policy)
          :ac-mob     (:mob ac-policy)
          :ac-expected (:expected-supply (:ac-available r))
          :rc-policy (:policy rc-policy)
          :rc-bog    (:bog rc-policy)
          :rc-dwell  (:dwell (:rc-available r))
          :rc-cyclelength (:cyclelength rc-policy)
          :rc-overlap  (:overlap rc-policy)
          :rc-mob     (:mob rc-policy)
          :rc-expected (:expected-supply (:rc-available r))
          :ng-policy (:policy ng-policy)
          :ng-bog    (:bog ng-policy)
          :ng-dwell  (:dwell (:ng-available r))
          :ng-cyclelength (:cyclelength ng-policy)
          :ng-overlap  (:overlap ng-policy)
          :ng-mob     (:mob ng-policy)
          :ng-expected (:expected-supply (:ng-available r))}))

(defn all-cases
  "Given an input marathon project, computes all the cases - to include
   policy excursions - as a lazy sequence of maps."
  [proj]
  (for [case        (build-cases proj)
        risk-case   (case->risk-cases case)
        policy-case (risk-case->policy-excursions risk-case)]
    policy-case))

(defn pad [n & {:keys [proportion] :or
                {proportion 0.2}}]
    [(* (- 1 proportion) n)
     (* (+ 1 proportion) n)])

;;since we have a nice batch of design points,
;;we can just eval the design (much like marathon.processing.stoke)
;;in fact, we can/maybe should plug this into stoke....

;;at some point, we'd need to explain what the variables are...
;;we use a static method to analyze performance (similar to stoke),
;;but we'd like to be able to swap that out for a dynamic method
;;(using M4).
(defn fill-performance
  "Given a demand and a design, evaluates the fill for the supply relative to
  demand under policy.  Returns lots of information to inform the
  ubertable we build later.  THe primary response is 'fill'"
  [demand {:keys [ac-supply ac-policy rc-supply rc-policy ng-supply ng-policy]
           :or {ac-supply 0 rc-supply 0 ng-supply 0}}]
  (let [ac (risk/algebraic-supply ac-supply ac-policy)
        rc (risk/algebraic-supply rc-supply rc-policy)
        ng (risk/algebraic-supply ng-supply ng-policy)
        total-supply (+ (:expected-supply ac)
                        (:expected-supply rc)
                        (:expected-supply ng))
        rounded-supply  (+ (Math/round (:expected-supply ac))
                           (Math/round (:expected-supply rc))
                           (Math/round (:expected-supply ng)))
        [lower upper] (pad demand)
        ]
    {:ac-available ac
     :rc-available rc
     :ng-available ng
     :total-supply total-supply
     :rounded-supply rounded-supply 
     :fill           (double (/ total-supply demand))
     :rounded-fill   (double (/ rounded-supply demand))
     :lower-fill     (double (/ total-supply lower)) 
     :upper-fill     (double (/ total-supply upper))}))

;;convenience function
(defn design->fill
  [{:keys [demand ac-policy rc-policy ng-policy] :as design}]
  (fill-performance demand design))

;;convenience keys for creating fields in our table later.
(def compo-bdr
  {:ac-policy :ac-dwell-ratio
   :rc-policy :rc-dwell-ratio
   :ng-policy :ng-dwell-ratio})
;;We're typically computing bdr by compo.  So one way is to
;;just compute for all of them...
(defn design->bdr
  "Computes/annotates the dwell ratios for our design, returning
   a map of {:xx-dwell-ratio ...} for each component."
  [{:keys [demand ac-policy rc-policy ng-policy] :as design}]
  (reduce-kv (fn [acc k return]
               (let [policy (get design k)
                     bdr    (simple-dwell-ratio
                             (:bog policy)
                             (:cyclelength policy))]
                 (assoc acc return bdr)))
             {} compo-bdr))

(defn design->performance
  "Convenient wrapper that merges an experimental design with
   its dwell-ratios and the resulting performance information, to
   include the primary response of 'fill'"
  [d]
  (merge d
         (design->fill d)
         (design->bdr d)))

;;we need to combine the policy variations.
;;we basically expand our data here...
(defn cases->records
  "Generates records for the ubertable from our input cases.
   Appends an index field :idx for reference"
  [xs]
  (->> xs
       (map-indexed (fn [i r]
                      (-> (design->performance r)
                          (merge r)
                          (risk-case->record)
                          (assoc :idx i))))))

(defn paste-cases!
  "Convenience function that lets us paste a tab-delimited
   text table [our ubertable] of input cases to the
   system clipboard.  THis is easily pasted to Excel."
  [xs]
  (->> xs
       cases->records
       tbl/records->table
       (tbl/order-fields-by risk-fields)
       (tbl/paste-table!)))

(defn case-records->trends
  "Computes input compatible wit the draft format
   expected by proc.risk for plotting risk trends on
   an assessment surface.  Given the output of cases->records,
   basically the ubertable records, bins the into
   [src {case {:demand ... :tren {r*}}}] for plotting."
  [res]
  (for [[src ys] (group-by :src res)]
    [src
     (into {}
           (let [groups (group-by :case ys)
                 base   (first (:current groups))
                 oititle (:oititle base)]
             (for [[case zs] groups]
               (let [zs (sort-by :idx zs)]
                 [case {:demand (:demand base)
                        :trend (->> zs
                                    (map (fn [r]
                                           (assoc r :bdr (:ng-dwell-ratio r))))
                                    (risk/experiment->trend case)
                                    (#(assoc % :oititle oititle))
                                    (risk/add-label base))
                        :lower-upper (->> zs
                                          (mapv (fn [{:keys [lower-fill upper-fill ng-dwell-ratio]}]
                                                  {:lower (gen/float-trunc lower-fill 3)
                                                   :upper (gen/float-trunc upper-fill 3)
                                                   :dwell ng-dwell-ratio}))) }]))))]))

;;adding derivative trends here...
;;we want to define dashed-trends based on effectively computed fields.
;;one option is to add computed fields, then extract additional trends
;;from them later.


(def paren-re #"\(.*\)")
(def open-or-close-re #"\(|\)")

;;eliminate everything between and including open/close paranthesis.
(defn strip-expr
  [txt]
  (clojure.string/replace txt paren-re  ""))

(defn strip-parens
  [txt]
  (clojure.string/replace txt open-or-close-re ""))

;;quick hack to get us a styled trend..
(defn style-trend [tr]
  (-> tr
      (dissoc :points :point-size)
      ((fn [r] (if (:dash r) (assoc r :dash 10.0 :width 3.0) r)))
      (update :series-label strip-expr)))

;;extremely hackish way to extract force structure from trend, this is dumb,
;;but lets us move the structure from series label to title for now...
(defn get-structure [tr]
  (->> tr
       second
       vals
       (map (comp :series-label :trend))
       (map (fn [r] (first (re-seq paren-re r))))
       first
       strip-parens))

(defn ->title [src demand oititle structure]
  (str "Risk to Mission for SRC: " src \newline oititle \newline
       \newline "AC/NG/AR = " structure " , Demand = " demand " (NDS)"))

;;plot tweaks...
(defn drop-legend [chrt]
  (do (.removeSubtitle chrt (.getSubtitle chrt 0))
      chrt))
;;% in the format code will multply by 100...
;;we can just not multiply.

;;allows us to trivially format our axes...
;;TODO : move to incanter.charts or cljfreechart...
(defn custom-format [formatter]
  (proxy [java.text.NumberFormat] []
    (^String format [number ^java.lang.StringBuffer result position]
     (.append result (str (formatter number))))))

;;TODO : move to incanter.charts or cljfreechart...
(def formats
  {:raw-percent (custom-format (fn [n] (str (long n) "%")))
   :ratio       (custom-format (fn [n] (str "1:" (long n))))})

;;TODO : move to incanter.charts or cljfreechart...
(defprotocol INumberFormat
  (as-number-format [n]))
;;TODO : move to incanter.charts or cljfreechart...
(extend-protocol INumberFormat
  java.text.NumberFormat
  (as-number-format [n] n)
  java.text.DecimalFormat
  (as-number-format [n] n)
  clojure.lang.Keyword
  (as-number-format [n]
    (or (get formats n)
        (throw (ex-info "unknown number format!?>!" {:format n}))))
  clojure.lang.IFn
  (as-number-format [n]
    (custom-format n)))

;;TODO : move to incanter.charts or cljfreechart...
(defn set-format [axis fmt]
  (doto axis
    (.setNumberFormatOverride
     ^java.lang.NumberFormat (as-number-format fmt))))

;;TODO : move to incanter.charts or cljfreechart...
(defn format-x [chart fmt]
  (-> chart
      (.getPlot)
      (.getDomainAxis)
      (set-format fmt))
  chart)

;;TODO : move to incanter.charts or cljfreechart...
(defn format-y [chart fmt]
  (-> chart
      (.getPlot)
      (.getRangeAxis)
      (set-format fmt))
  chart)

;;TODO : move to incanter.charts or cljfreechart...
(defn pathify-lines [chart]
  (doseq [{:keys [renderer]} (proc.risk/series-seq (.getPlot chart))]
    (.setDrawSeriesLineAsPath renderer true))
  chart)

(defn add-lower-upper  [chart lower-upper]
  (let [percent  (fn [n] (* n 100))
        lowers   (->> lower-upper
                       (map (comp percent :lower)))        
        uppers  (->>  lower-upper
                      (map (comp percent :upper)))
        dwell   (map :dwell lower-upper)
        lower-dwell (for [[lower dwell] (map vector lowers dwell)
                          :when (<= lower 100)]
                      [lower dwell])
        upper-dwell (for [[upper dwell] (map vector uppers dwell)
                          :when (<= upper 100)]
                      [upper dwell])]
    (-> chart
        (risk/append-lines (map first lower-dwell) (map second lower-dwell) :color :black :dash 7.0 :width 3 )
        (risk/append-lines (map first upper-dwell) (map second upper-dwell) :color :black :dash 7.0 :width 3 ))))

(def custom-orange (incanter.charts/chart-color [255 192 0]))

(def custom-styles
  {:growth  {:series-label "Growth (x+1/y+1/z+1), AC1:0"
             ;:points true
             :width 5.0
             :color :blue
             ;:point-size 10.0
             }
   :current   {:series-label "Current Structure (x/y/z), AC1:0" 
               ;:points true
               :width 5.0
               ;:dash 10.0
               :color :black
               ;:point-size 10.0
               }
   :current-11 {:series-label "Current Structure (x/y/z), AC1:1"
                ;:points true
                :width 5.0                         
                :color custom-orange #_:dark-blue
                ;:point-size 10.0
                }
   :current-12 {:series-label "Current Structure (x/y/z), AC1:2"
                :points true
                :width 5.0                         
                :color :dark-blue
                :point-size 10.0}})

(defn plot-experiment
  "Plots a single element of the output from case-records->trends,
   or a risk trend."
  [[src {:keys [current #_growth current-11] } :as tr]]
  (let [demand  (:demand current)
        oititle (:oititle (:trend current))
        structure (get-structure tr)]
    (-> (risk/simple-response (mapv (comp style-trend :trend)
                                    [current #_growth current-11])
                              :title (->title src demand oititle structure))
        (add-lower-upper (:lower-upper current))
        (drop-legend)
        (format-x :raw-percent)
        (format-y :ratio)
        pathify-lines
        i/view)))

(defn spit-plots [xs & {:keys [root trend-styles] :or {root "."}}]
  (doseq [x xs]
    (let [src  (first x)
          path (io/relative-path root [(str src ".png")])
          frm  (plot-experiment x)
          _    (Thread/sleep 100)
          surface @proc.risk/surface
          _    (proc.charts/simple-save-jfree-chart surface path)
          _    (println (str "spitting " path))
          _    (doto frm (.setVisible false) (.dispose))])))

(defn plot-pptx [root & {:keys [filename] :or {filename "surfaces.pptx"}}]
  (-> (->> root
           (proc.powerpoint/find-images)
           (map (fn [nm] (io/file-path root nm)))
           (proc.powerpoint/add-pictures (proc.powerpoint/->pptx)))
      (proc.powerpoint/save-ppt (io/file-path root filename))))

(defn move [from to]
  (when (not= from to)
    (io/fcopy
     (clojure.java.io/file from)
     (clojure.java.io/file to))))

(defn emit-plots [xs & {:keys [filename retain?] :or {filename "surfaces.pptx"}}]
  (io/with-temp-dir
    (let [fp (if retain?
               (io/file-path "." "pics")
               *tmpdir*)
          from (io/file-path fp filename)
          to (io/file-path "." filename)]
      (io/make-folders! fp)
      (spit-plots xs :root fp)
      (plot-pptx fp :filename filename)
      (println [:moving from to])
      (move from to))))

;;this is the big shebang
;;call it with a path to an m4 project, and it'll result in charts!
(defn wb->plots [wb & {:keys [outpath retain? trend-styles]
                       :or {trend-styles custom-styles}}]
  (let [wbpath (if (string? wb) wb
                   (or (-> wb :paths :Parameters first)
                       (throw (ex-info "Expected a workbook path or a project!"
                                       {:wb (type wb)}))))
        outpath (or (when outpath (io/file-path outpath))
                    (io/file-path (io/parent-path wbpath)
                                  "surfaces.pptx"))
        filename (io/fname outpath)]
    (binding [proc.risk/*trend-styles* trend-styles]
      (-> (if (string? wb)
            (analysis/load-project wbpath)
            wb)
          all-cases
          cases->records
          case-records->trends
          (emit-plots :retain? retain? :filename filename))
      (move (io/file-path "." filename)
            (io/file-path outpath)))))
