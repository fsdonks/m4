;;Ns for convenient vnv operations for comparing
;;marathon runs and general scripting.
(ns marathon.vnv
  (:require [marathon
             [analysis :as a]
             [demo :refer :all]
             [run :as run]]
            [spork.entitysystem.store :as store]
            [marathon.ces     [core :as core]]
            [spork.util       [io :as io] [table :as tbl]]
            [proc.example :as proc]))

(def threepath (hpath "\\Documents\\marv\\vnv\\m3v6\\"))
(def fourpath  (hpath "\\Documents\\marv\\vnv\\m4v6\\"))

(def taa-ints
  {:BCTS    ["BCTS"
             ["47112K000"
              "77202K000"
              "77202K100"
              "87312K000"]]
   :GSAB    ["GSAB"
             ["01225K000"]]
   :CAB     ["CAB"
             ["01302K000"]]
   :CSSB    ["CSSB"
             ["63426K000"]]
   :QMSUPP  ["QM Supply Co"
             ["10473R100"]]
                                        ;     :QMWATER ["QM Water Supp Co"
                                        ;              ["10460R000"]]
   :QMPOL   ["QM Pol"
             ["10527RA00"
              "10527RC00"
              "10527RF00"
              "10527RG00"]]
   :PATRIOT ["ADA Patriot"
             ["44635K000"]] ;in scope
                                        ;  :AVENGER ["ADA Avenger"
                                        ;            ["44615R600"]
   }) ;in scope

(def branches
  {:14 ["14" ["14420R000" "14423R000" "14527RB00" "14537K000"]],
   :01
   ["01"
    ["01205K000" "01225K000" "01225K100" "01285K000" "01285K100"
     "01302K000" "01402K000" "01418K000" "01435K000" "01447K000"
     "01605K100" "01607K000" "01612K000" "01612K100" "01622K000"
     "01623K000" "01625E000" "01635K000" "01636K000" "01639K000"
     "01655E000" "01695E000" "01707R000" "01802G000" "01808G000"
     "01815G000" "01825G000" "01927K000" "01959R000" "01980G000"
     "01982G000"]],
   :37 ["37" ["37342R200" "37600K000"]],
   :07
   ["07"
    ["07195K000" "07215K000" "07215K100" "07315K000" "07315K100"
     "07802K000" "07805K000" "07815R000"]],
   :27
   ["27"
    ["27473K000" "27520R000" "27523RA00" "27523RB00" "27523RC00"
     "27540R000" "27543RA00" "27560R000" "27563RA00" "27563RB00"
     "27563RC00" "27563RD00" "27563RE00" "27570R000" "27573RB00"
     "27573RC00" "27583RA00" "27583RB00" "27583RC00" "27773K000"]],
   :42
   ["42"
    ["42524RA00" "42524RB00" "42524RC00" "42524RD00" "42529RA00"
     "42529RE00"]],
   :45 ["45" ["45413L000" "45423L000" "45500GB00" "45607L000"]],
   :03
   ["03"
    ["03310R000" "03323K000" "03396K000" "03420R300" "03470R000"
     "03492K000" "03520R000" "03579RA00"]],
   :77 ["77" ["77202K000" "77202K100"]],
   :31 ["31" ["31812R100" "31813G200" "31815RA00" "31825R100"]],
   :40
   ["40"
    ["40500RA00" "40510RC00" "40702R100" "40793R000" "40795R100"
     "40802R000" "40806R000" "40810R000" "40820R100" "40830R000"]],
   :52 ["52" ["52400K300" "52403K300" "52410K300"]],
   :05
   ["05"
    ["05315K500" "05315K600" "05315K700" "05315K800" "05402K000"
     "05427R000" "05428R000" "05429R000" "05435K000" "05437R200"
     "05437RC00" "05438R000" "05439R000" "05473R000" "05510RA00"
     "05510RB00" "05520RA00" "05520RB00" "05520RC00" "05520RD00"
     "05520RE00" "05530RA00" "05530RH00" "05530RI00" "05543RH10"
     "05543RH20" "05567RA00" "05567RB00" "05601KT00" "05601RH00"
     "05603KT00" "05616K000" "05617R000" "05618R000" "05640R000"
     "05800R000"]],
   :12
   ["12"
    ["12410R100" "12413R000" "12567RA00" "12567RB00" "12567RE00"
     "12567RK00" "12567RL00" "12682R000"]],
   :11
   ["11"
    ["11307R500" "11307R600" "11623R000" "11632R000" "11693R000"
     "11902R000" "11975K000"]],
   :10
   ["10"
    ["10380R000" "10414R000" "10417R000" "10420R100" "10436R000"
     "10460R000" "10473R100" "10490R000" "10527RA00" "10527RC00"
     "10527RF00" "10527RG00" "10538RA00" "10538RB00" "10538RF00"
     "10548RA00" "10548RB00" "10560RA00" "10560RB00" "10560RM00"
     "10560RN00" "10567RA00" "10567RC00" "10567RG00" "10602L000"
     "10649R000" "10473R000"]],
   :06
   ["06"
    ["06235K100" "06235K300" "06325K000" "06333K000" "06333K100"
     "06385K000" "06425R000" "06433K000" "06455R000" "06465K000"
     "06475K000" "06602R000" "06603A000" "06604A000"]],
   :30
   ["30"
    ["30620R100" "30625R000" "30702R000" "30705R200" "30715R200"
     "30725R100" "30730R200" "30815R100" "30815R200" "30815R300"
     "30815R400" "30832R000" "30837R000" "30840A100" "30875R100"]],
   :53
   ["53" ["53602R100" "53605R300" "53605R400" "53612R300" "53615R300"]],
   :43 ["43" ["43429R000" "43547RA00" "43613R000"]],
   :16 ["16" ["16500FC00" "16500FD00" "16500LA00" "16500LB00"]],
   :44
   ["44" ["44601R600" "44602K000" "44635K000" "44655K000" "44693R000"]],
   :87 ["87" ["87000K100" "87312K000"]],
   :35 ["35" ["35500R000"]],
   :55
   ["55"
    ["55506RA00" "55507RA00" "55522KA00" "55522KB00" "55530RJ00"
     "55560RD00" "55560RE00" "55587RA00" "55588RA00" "55603KB00"
     "55606R000" "55612R000" "55716R000" "55719R000" "55727R100"
     "55727R200" "55727R300" "55728R100" "55728R200" "55728R300"
     "55739R000" "55740K000" "55779R000" "55789R000" "55812K000"
     "55816K000" "55819R000" "55820K000" "55829K000" "55838K000"
     "55848K000"]],
   :47 ["47" ["47112K000"]],
   :20 ["20" ["20518RA00" "20518RB00" "20518RC00"]],
   :17 ["17" ["17195K000" "17215K000" "17215K100" "17315K000"]],
   :19
   ["19"
    ["19402K000" "19473K000" "19476K000" "19539RA00" "19539RB00"
     "19539RC00" "19543RE00" "19601R000" "19601RA00" "19646K000"
     "19653R000" "19717R000" "19882K000" "19886K000" "19888R000"]],
   :41
   ["41"
    ["41702R100" "41710R000" "41730G000" "41730R000" "41736R100"
     "41737R100" "41740R100" "41745R000" "41750R100"]],
   :90 ["90" ["90376K000" "90472K000" "90588RA00" "90873R000"]],
   :33
   ["33"
    ["33712G100" "33712G200" "33715G200" "33725G200" "33735G200"
     "33736G100" "33737G100" "33737G200" "33757G100"]],
   :51 ["51" ["51000R000" "51100R100" "51632R000" "51659R000"]],
   :63
   ["63"
    ["63025K000" "63035K000" "63037K600" "63045K000" "63047K600"
     "63055K000" "63058K000" "63302K000" "63346R200" "63347R000"
     "63347R100" "63347R200" "63347R300" "63355R000" "63375K000"
     "63417K300" "63426K000" "63475K000" "63475K100" "63702K000"
     "63702K100" "63862R000" "63867R000" "63868R000"]],
   :02
   ["02"
    ["02523KB00" "02523KC00" "02523KD00" "02543KA00" "02553KA00"
     "02573KA00"]],
   :34 ["34" ["34402K000" "34425K000" "34645R300"]],
   :09
   ["09"
    ["09430R000" "09436K000" "09513KA00" "09513KB00" "09537RA00"
     "09537RB00" "09574RA00" "09574RB00" "09632K000" "09647K000"
     "09747R000"]],
   :08
   ["08"
    ["08300R000" "08317K000" "08420R000" "08429R000" "08430R000"
     "08453R000" "08457R000" "08460R000" "08473R000" "08480K000"
     "08485R000" "08488K000" "08527AA00" "08528RA00" "08528RB00"
     "08567RA00" "08640R000" "08641RA00" "08641RB00" "08668R000"
     "08670R000" "08949R000" "08976R000" "08977R000" "08978R000"
     "08979R000" "08988R000"]]})

(defn src [& xs] (select-keys taa-ints (vec xs)))
(defn branch [& xs] (select-keys branches (vec xs)))

(defn sample-charts [path & {:keys [interests]
                             :or   {interests taa-ints}}]
  (do (proc/run-sample! path :interests interests)
      (proc/do-charts-from path :interests interests)))

(comment
(in-ns 'proc.util)
(defn read-tsv-dataset
  [path]
  (let [fields (atom nil)
        rs     (into []
                 (spork.util.table/tabdelimited->records
                  path))]
    (incanter.core/dataset (vec (keys (first rs))) rs)))

(in-ns 'marathon.vnv)
)

(defn sample-charts [path & {:keys [interests]
                             :or   {interests taa-ints}}]
  (do (proc/run-sample! path    :interests interests)
      (proc/do-charts-from path :interests interests)))

(defn re-run4 [& {:keys [interests]}]
  (run/do-audited-run (str fourpath "testdata-v6.xlsx") fourpath)
  (proc/run-sample! fourpath :interests (or interests (src :BCTS))))

(defn re-run3 [& {:keys [interests]}]
  (proc/run-sample! threepath :interests (or interests (src :BCTS))))

(defn re-run [& {:keys [interests]}]
  (do (re-run4 :interests interests)
      (re-run3 :interests interests)))

(defn compare-bcts []
  (proc/do-charts-from  threepath  :interests   (src :BCTS))
  (proc/do-charts-from  fourpath   :interests   (src :BCTS)))

(defn run-branch [br]
  (re-run :interests (branch br)))
(defn compare-branch [br]
  (proc/do-charts-from  threepath  :interests (branch br))
  (proc/do-charts-from  fourpath   :interests (branch br)))

;;useful helpers.
(defn fill->info [{:keys [rule fillPath pathlength source] :as fd}]
  (into  {:rule rule
          :fillPath fillPath
          :pathlength pathlength}
         (seq (dissoc source :policy :statedata :locationhistory))))


;;homebrew diffing tools.
;;simple record-based diff function.
(defn diff-by
  ([fields l r]
   (let [parsers (if (map? fields)
                   fields
                   {})
         parse (fn [fld v]
                 (if-let [f (parsers fld)]
                   (f v)
                   v))

         fields (if (map? fields)
                  (vec (keys fields))
                  fields)]
     (reduce (fn [acc fld]
               (let [x (parse fld (get l fld))
                     y (parse fld (get r fld))]
                 (if (not= x y)
                   (conj acc {:field fld
                              :l x
                              :r y})
                   acc)))
             [] fields)))
  ([l r] (diff-by (keys l) l r)))


(def depfields
  {:Location (fn [^String x] (when x (.replace x "_Deployable" "")))
   :Demand   identity
   :DwellBeforeDeploy identity
   :BogBudget identity
   :CycleTime identity
   :DeployInterval identity
   :Unit identity
   :FollowOn #(when % (clojure.string/upper-case %))})

;;Note: we need a little bit smarter indexing here,
;;since we have multiple srcs interleaved in the deployment
;;output.  We can just sort by a unique key...

;;In this case, if we assign an index to each record,
;;we can sort by the idx and the SRC to get a consistent
;;ordering for diffing.

(defn enumerate [rs]
  (map-indexed (fn [i r] (assoc r :index i)) rs))

(defn diff-deployments
  "Somewhat of a monster, pending refactor.  We sort
   the records now (note eager/forcing....) by 
   demandtype and index (order of appearance).  
   That leaves us with a dataset that can be 
   diffed in-order."
  [& {:keys [lpath rpath fields]
      :or {lpath (str threepath "AUDIT_Deployments.txt")
           rpath (str fourpath "AUDIT_Deployments.txt")
           fields depfields}}]
  (let [sortf    (juxt :DemandType :index)
        ls  (->> (spork.util.table/tabdelimited->records lpath)
                 (into [])
                 (enumerate)
                 (filter  #(not= (:Demand %) "NotUtilized"))
                 (sort-by sortf)
                 #_(reset! lefts)
                 )
        rs  (->> (spork.util.table/tabdelimited->records rpath)
                 (into [])
                 (enumerate)
                 (sort-by sortf)
                 #_(reset! rights))
        lc (count ls)
        rc (count rs)
        diff (- lc rc)
        padded-ls (when (neg? diff)
                    (take (Math/abs diff) (repeat {})))
        padded-rs (when (pos? diff)
                    (take (Math/abs diff) (repeat {})))        
        ]
    (->> (map vector (concat ls padded-ls)
                     (concat rs padded-rs)) 
         (map #(apply diff-by depfields %))
         (map-indexed
          (fn [n r] (when (not (empty? r))
                          [n r])))
         (filter identity))))




