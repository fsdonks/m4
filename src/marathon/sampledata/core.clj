(ns marathon.sampledata.core)

(defn find-marathon []
  (let [paths (->> (clojure.string/split 
                     (java.lang.System/getProperty "java.class.path")
                     #";")
                  (filter #(.contains % "marathon")))]
    (if-let [jar-file (some #(when (.contains % ".jar") %) paths)]
      jar-file
      (some #(when (.contains % "\\src") %) paths))))

(defn find-marathon-data []
  (let [path (find-marathon)]
    (if (.contains path ".jar")
        (throw (Exception. "dunno how to get resources in jar yet"))
        path)))
  
(def datasets 
  {:supply  "\\marathon\\sampledata\\project\\SupplyRecords.txt"
   :demand  "\\marathon\\sampledata\\project\\DemandRecords.txt"
   :policy  "\\marathon\\sampledata\\project\\PolicyRecords.txt"               
   :deployments "\\marathon\\sampledata\\project\\Deployments.txt"
   :relations  "\\marathon\\sampledata\\project\\RelationRecords.txt"
   :periods  "\\marathon\\sampledata\\project\\PeriodRecords.txt"
   :composites "\\marathon\\sampledata\\project\\CompositePolicyRecords.txt"})

(defn get-dataset
  "Fetches a keyed dataset of embedded sample data.  Returns a string of the 
   encoded data.  Cleaning and formatting is up to the caller!."
  [datakey & {:keys [datapath]
                              :or   {datapath (find-marathon-data)}}]
  (if-let [path (get datasets datakey)]
    (slurp (str datapath path))))
