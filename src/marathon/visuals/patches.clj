;;A port of the patch-chart/laydown visualization from squirm. 
;;We need to establish a portable format for generating these dudes.
(ns marathon.visuals.patches
  (:require [spork.sketch :as sketch]
            [spork.geometry.shapes :as s]
            [spork.graphics2d.canvas :as canv]
            [clojure.core.reducers :as r]
            [spork.util.table :as tbl]))

;;(table-by  :unitid  :Quarter (fn [r] [(:category r) (:operation r)]))

;;So, we need to get quarterly samples by unit.
;;each unit has a history at that time.

;;Given locations of units we typically
;;derive histories...

;;In theory, I should be able to create a
;;locations table .... I think we have a
;;generic data format that'd be nice to follow...

;;WE need to get a structure like this
;;[q1, q2, q3, qn]
;;for each unit out of the history.
;;That's what makes the tracks.

;;given our location history...
;;we should be able to do this...
;;We just split out the records by unit...
;;by time (filtering time by 90 day intervals,
;;or, using the subsampling of
;;the mode of the state over said 90 day
;;period).

;;So, where we were using category/operation
;;to define our demands, we're now
;;using StartState and operation (I think).
;;Since StartState maps to an underlying
;;state, and we don't have to worry about
;;transitions, we can map colors to the
;;state.  I "think" we also need to
;;add labels for locations...

;;Note: once we have a table built with
;;this cellular data, we can manipulate
;;it pretty easily (ala find/replace).
;;It'd just a matter of rendering cells
;;with the appropriate values and coloring.

;;fold over the samples, taking as a sample
;;the mode of the data for the specified interval.
(defn modal-sampler [xs interval tf keyf valf]
  (into []  (comp (partition-by (fn [r] (quot (tf r) interval)))
                  (mapcat (fn [rs]
                         (let [t (tf (first rs))]
                           (for [[u entries] (group-by keyf rs)]
                             (let [v (->> entries
                                          (map valf)
                                          (frequencies)
                                                 (reduce-kv (fn [[v0 n0 :as acc] v n]
                                                              (if (> n n0)
                                                                [v n]
                                                                acc)) [nil 0])
                                                 (first))]                               
                               {:t t :key u :mode v }))))))
        xs))

(def locschema
  {:id :text
   :name :text
   :locationname :text
   :location :text
   :positionpolicy :text
   :src :text
   :component :text
   :t :int
   })

(defn patch-sampler [xs]
  (modal-sampler xs 91 :t
                 (fn [r] {:src       (:src r)
                          :name      (:name r)
                          :component (:component r)})
                 (juxt :positionpolicy :locationname)))

(def testpath "c:/users/tspoon/Documents/srm/locsamples.txt")
(defn lines->samples [path]
  (-> (spork.util.general/line-reducer path)
      (spork.util.table/lines->records   locschema)
      (patch-sampler)))

;;this is the default value function...
;;Notice in category, it's SRM data...
;;operation is the name.
(defn valuef [r] [(:category r) (:operation r)])

;;this is more or less a pivot table.
(defn table-by [rowkey colkey valuef recs]
  (let [rows (atom #{})
        cols (atom #{})        
        add-rowcol! (fn [r c]
                      (do (swap! rows conj r)
                          (swap! cols conj c)))
        rowcols   (fn [recs]
                    {:rows @rows :cols @cols
                      :rowcols recs})]
    (->>  recs
          (reduce (fn [rowcols r]
                    (let [row (rowkey r)
                          col (colkey r)
                          v   (valuef r)]
                      (do (add-rowcol! row col)
                          (conj rowcols 
                                {:row row 
                                 :col col 
                                 :value v}))))
                  [])
          (rowcols))))

(defn rowcols->vecs [{:keys [rows cols rowcols]}]
  (for [[row rs] (group-by :row rowcols)]
    [row (persistent!  (reduce (fn [acc r] (conj! acc  (:value r)))
                               (transient   [])
                               (sort-by :col rs)))]))


(comment 
(def ^:dynamic *attributes*
  {"Committed"        {:SurgeType "Committed"}
   "FCCommitted"      {:SurgeType "Committed"}
   "FCMission"        {:SurgeType "Mission"  }
   "Mission"          {:SurgeType "Mission"  }
   "Demand1"          {:SurgeType "Committed"}
   "Demand2"          {:SurgeType "Mission"  }
   "Ready_Deployable" {:SurgeType "Ready"    }})
)

(def ^:constant +chunk-width+  150)
(def ^:constant +chunk-height+ 14)
(def ^:dynamic *chunk-txt*)

;; (canv/color-by (canv/gradient-right l r);;                (s/->rectangle :white 0 0 +chunk-width+ +chunk-height+)))    

;; (defn ->transition [l r]        
;;   (canv/color-by (canv/gradient-right l r)
;;                  (s/->rectangle :white 0 0 +chunk-width+ +chunk-height+)))


;; (defn ->added-label  [txt label-color color x y w h]
;;   (let [r          (s/->rectangle  color x y w h)
;;         half-dur   (/ w 2.0)
;;         centerx    (+ x 1)
;;         scalex     1.0 
;;         centery    0]
;;      (sketch/scale scalex 1.0 (sketch/uncartesian 
;;                                (sketch/->label txt centerx centery :color label-color)))))


;; (defn ->transition 
;;   ([l r] (->transition l r "Transition"))
;;   ([l r lbl]
;;      (canv/color-by (canv/gradient-right l r)
;;                     (sketch/->labeled-box lbl :black :white 0 0 +cunk-width+ +chunk-height+))))

;; (defn ->transition 
;;   ([l r]      (canv/color-by (canv/gradient-right l r)
;;                              (s/->rectangle :white 0 0 +chunk-width+ +chunk-height+)))
;;   ([l r lbl]
;;      (canv/color-by (canv/gradient-right l r)
;;                     (sketch/->labeled-box lbl :black :white 0 0 +chunk-width+ +chunk-height+))))

;; (defn transition-type [^String loc]
;;   (if (.contains loc "Transition")
;;       (clojure.string/replace loc "Transition" "")))

(def palette
  {:DarkYellow	[255	255	0]
   :LightYellow	[255	255	153]
   :DarkGreen	[84	130	53]
   :LightGreen	[146	208	80]
   :DarkBlue	[0	0	255]
   :LightBlue	[0	255	255]})

(def colors 
  {"MP_DA_C1"		:DarkBlue
   "MA_DA_C1"		:DarkBlue
   "MA_DA_C2"		:DarkBlue
   "MD_DA_C1"		:DarkBlue
   "MD_DA_C2"		:DarkBlue
   "MP_NDA_C3"		:LightBlue
   "MA_NDA_C3"		:LightBlue
   "MD_NDA_C3"		:LightBlue
   "R_C1"		:DarkGreen
   "R_C2"		:LightGreen
   "PB_C3"		:DarkYellow
   "PB_C4"		:DarkYellow
   "PT_C4"		:LightYellow
   "PL_C4"		:LightYellow
   ":recovery"          :DarkYellow})

(comment 
(defn loc->color 
  ([loc location->attributes]
     (if (vector? loc)  (or (loc->color (first loc)) (loc->color (second loc)))
         (case loc
           "Label"   :grey
           "Ready"   :green
           "Prepare" :yellow
           "TransitionReady" [:light-sky-blue :green]
           "TransitionReady_NotDeployable" [:light-sky-blue :green]
           "TransitionPrepare" [:orange :yellow]
           "Mission_NotDeployable" :light-sky-blue
           "Mission_Deployable" :light-sky-blue
           (if (.contains ^String loc "Transition")
             :transition
             (case     (:SurgeType (location->attributes loc))
               "Committed" :orange
               "Mission"    :light-sky-blue
               "Mission_Deployable" :light-sky-blue
               "Ready"     :green
               "Prepare"   :yellow        
               (throw (Exception. (str "unknown mission type: " loc))))))))
  ([loc] (loc->color loc #(get *attributes* %))))
)
(defn loc->color
  ([loc] (or (get colors loc) (throw (Exception. (str [:unknown-location loc])))))
  ([loc & xs] (loc->color loc)))
  
       
;; (defn id-transitions [xs]
;;   (loop [acc  []
;;          prev nil
;;          xs   xs]
;;     (if (empty? xs) acc
;;         (let [x (first xs)]
;;           (case x 
;;             ;we hit a transition
;;             :transition 
;;             (let [to (fnext xs)]
;;               (recur 
;;                (conj acc [prev to])
               
;;                to
;;                (rest xs)))
;;             (recur (conj acc x)
;;                    x
;;                    (rest xs)))))))

;; (defn clean-transitions [xs]
;;   (into []
;;         (r/map (fn [x]
;;                  (if (vector? x)
;;                    (let [[l r] x]
;;                      (if (or (or (nil? l) (nil? r))
;;                              (or (identical? l :transition)
;;                                  (identical? r :transition)))
;;                        (case [l r]
;;                          [nil :light-sky-blue]           [:green  :light-sky-blue]
;;                          [nil :orange]         [:green  :orange]
;;                          [nil :yellow]         [:orange :yellow]
;;                          [nil :green]          [:light-sky-blue   :green]
;;                          [nil :transition]     [:light-sky-blue :green]
;;                          [:light-sky-blue   nil]         [:light-sky-blue   :green]
;;                          [:orange nil]         [:orange :yellow]
;;                          [:green  nil]         [:green  :light-sky-blue]
;;                          [:yellow nil]         [:yellow :green]
;;                          [:transition nil]     [:light-sky-blue :green]
;;                          [:transition :light-sky-blue]   [:green  :light-sky-blue]
;;                          [:transition :orange] [:green  :orange]
;;                          [:transition :yellow] [:orange :yellow]
;;                          [:transition :green]  [:light-sky-blue   :green]
;;                          [:light-sky-blue   :transition] [:light-sky-blue   :green]
;;                          [:orange :transition] [:orange :yellow]
;;                          [:green  :transition] [:green  :light-sky-blue]
                         
;;                          ;(throw (Exception. (str x)))
;;                          (do ;(println (str "unknown color type:" x))
;;                              :grey)
;;                          )
;;                        (if (= l r) l
;;                            x)))
;;                      x))
;;                xs)))

(defn table->colors [rowcols]
  (->> rowcols
;      (r/map #(clean-transitions (id-transitions (mapv loc->color %))))
       (r/map #(mapv loc->color %))
       (into [])))

(defn ->chunk 
  ([color lbl]
     (if (vector? color)
       (->transition (first color) (second color))
       
                                        ;    (s/->rectangle coloro 0 0 +chunk-width+ +chunk-height+)
       (sketch/->labeled-box lbl :black color 0 0 +chunk-width+ +chunk-height+)))
  ([color] (->chunk color "")))

;; (def colors 
;;   [:green
;;    :light-sky-blue
;;    :orange 
;;    :yellow
;;   [:green  :light-sky-blue]
;;   [:green  :orange]
;;   [:green  :yellow]
;;   [:orange :yellow]
;;   [:light-sky-blue   :green]
;;   [:light-sky-blue   :green]
;;   [:light-sky-blue   :green]
;;   [:orange :yellow]
;;   [:green  :light-sky-blue]
;;   [:yellow :green]
;;   [:light-sky-blue   :green]
;;   [:green  :light-sky-blue]
;;   [:green  :orange]
;;   [:orange :yellow]
;;   [:light-sky-blue   :green]
;;   [:light-sky-blue   :green]
;;   [:light-sky-blue   :yellow]
;;   [:yellow :light-sky-blue]
;;   [:orange :yellow]
;;   [:green  :light-sky-blue]
;;   [:yellow :orange]])

;; (def color-set (set colors))
  
(def chunks 
  (zipmap colors 
          (map (comp sketch/outline ->chunk) colors))) 

(defn as-chunk 
  [x & [lbl]]
  (if (contains? palette x)
    (sketch/outline (if lbl (->chunk x lbl) (->chunk x)))
    (do (println (str "Unknown chunk!" x))
        (->chunk :red))))

(def knownlocs ;#{"Prepare" "Ready" "Ready_Deployable" "TransitionReady_NotDeployable" "DeMobilization" "Mission_Deployable"})
  (set (keys colors)))

(defn table->labels [rowcols]
  (for [row   rowcols]
    (vec (for [[l r] row]
           (cond (= l r) ""
                 (contains? knownlocs r) ""
                 :else r)))))

;; (defn sketch-history [rowcols]
;;   (let [tbl     (table->colors rowcols)
;;         labels  (table->labels rowcols)
;;         cells   (sketch/stack
;;                         (mapv (fn [xs] (sketch/shelf (mapv as-chunk xs))) tbl))
;;         width   (:width (.shape-bounds cells))]
;;     cells))


;;note we can use our table view from piccolo here as well...
;;in fact, we can convert this to a zui pretty easily.
(defn sketch-history [rowcols]
  (let [tbl     (table->colors rowcols)
        labels  (table->labels rowcols)
        cells   (sketch/stack
                 (map-indexed (fn [i row]
                                (let [labs (nth labels i)]
                                  (sketch/shelf
                                   (map-indexed (fn [col x]
                                                  (as-chunk x (nth labs col))) row)))) tbl))
        width   (:width (.shape-bounds cells))]
    cells))

(defn strike-through 
  ([shp color]  
     (let [bounds  (spork.graphics2d.canvas/shape-bounds shp)
           x       (:x bounds)
           y       (+ (:y bounds) (/ (:height bounds) 2.0))
           ln      (spork.geometry.shapes/->line color
                                                 x
                                                 y
                                                 (+ x (dec (:width bounds))) y)]
       [shp 
        ln]))
  ([shp] (strike-through shp
             (apply canv/->color-rgba 
                    (spork.graphics2d.canvas/random-color)))))

;;this is a little adapter function 
(defn clean-rec [{:keys [t key mode] :as r}]
  (let [{:keys [name src component]} (:key r)]
    {:t 0 :src src :name name :component component
     :state (first mode)
     :Quarter (quot t 91)}))

;;Visualization api
(defn chunk-table [basedata sortkey]
    (let [row-order (atom [])]
      (->> basedata
           (table-by  (juxt :component :src :name)  :Quarter :state)
           (rowcols->vecs)
           (sort-by (comp sortkey first))
           (reduce (fn [acc [r xs]]                   
                     (do (swap! row-order conj r)
                         (conj acc xs))) [])
           ((fn [r] (with-meta r {:row-order @row-order}))))))
(comment ;testing 
  (def res         (lines->samples testpath))
  (def grps        (group-by #(-> % :key :src) res))
  (def binders     (get grps "Binder"))
  (def ct          (chunk-table (map clean-rec binders) :component))
)


(def ^:dynamic *table-rate* 91)
(comment 
(defn sand-tables [ds]
    (let [basedata  (->> ds 
                         (sample-every *table-rate*) 
                         (non-ghosts)
                         ($where ($fn [Quarter] (<= Quarter 24))))]
      (for [[k ds] (sort-by (comp vec vals first) ($group-by [:compo :SRC] basedata))]
        (let [unit-map  (reduce (fn [acc r] (assoc acc (:unitid r) r)) {} (:rows ds))
              sortkey   (fn [uid] (let [info  (get unit-map uid)]
                                     [(:compo info) (:SRC info) uid]))
              ctbl      (chunk-table ds sortkey)
              row-order (get (meta ctbl) :row-order)
              names     (map #(:name (get unit-map %))  row-order)]
          [k names ctbl]))))

(defn render-sand-tables [ds]
  (let [sts (sand-tables (util/as-dataset ds))]    
    (sketch/delineate
     (into 
      [
       ;(chunk-headers (nth 2 (first sts)))
       ]
      (for [[{:keys [SRC compo]}  labels chunks] sts]        
        (let [hist   (patches/sketch-history chunks)
              height (:height (.shape-bounds hist))]
          (sketch/beside (sketch/->labeled-box (str [SRC compo]) :black :light-gray 0 0 114 height)
                         hist)))))))
)
