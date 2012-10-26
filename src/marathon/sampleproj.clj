(ns marathon.sampleproj
  (:require [clojure [string :as strlib] [set :as setlib]]
            [clojure.contrib [json :as json]]
            [clojure.java [io :as io]]
            [clojure [pprint :as pp]]))

(defn as-directory [s] 
  (if (= (subs s (dec (count s))) "\\")
    s
    (str s "\\")))

(def home-path (System/getProperty "user.home"))

(defn deep-copy 
	"Copies all files in sourcedir to targetdir.  Creates folders as needed"
	[sourcedir targetdir] 
	(let [source-paths (map #(.getPath %) (file-seq (io/file sourcedir)))
		  dest-paths   (map #(str targetdir (subs % (count sourcedir))) source-paths)
		  pathmap (zipmap source-paths dest-paths)]
	  (doseq [kv pathmap]
		(let [[kf vf] (map io/file kv)]		
			(when (not (.exists vf)) (io/make-parents vf))
			(when (.isFile kf) (io/copy kf vf))))))

(defn relative-path
  "Given a root path (a string), and a list of strings, generates a relative
   path.  Auxillary function for making paths easier to deal with."
  [root pathlist]
  (apply str (as-directory root) (butlast (interleave pathlist (repeat "\\")))))

(defmacro with-path
  "Given a root directory, and a collection of bindings in the form 
   [path [subdir1 subdir2...file]], evals body inside an expression 
   with *root* bound to the root path, and each binding available as 
   a fully-realized file path (relative-path *root* %) is called on 
   each pathlist)."
  [root bindings body]
  (let [binds (mapcat 
                (fn [[nm pathlist]] 
                  (list nm (list 'relative-path '*root* pathlist)))
                    (partition 2 bindings))]               
    `(let [~'*root* ~root
           ~@binds]
       ~body)))

(def common-paths {:home home-path 
                   :docs (relative-path home-path ["Documents"])
                   :javapath (System/getProperty "sun.boot.library.path")
                   :startdir (System/getProperty "user.dir")
                   :tempdir (System/getProperty "java.io.tmpdir")
                   :path   (strlib/split (System/getProperty "java.library.path")
                             #";")
                   :classpath (strlib/split (System/getProperty "java.class.path")
                               #";")
                   :javahome  (System/getProperty "java.home")})

(defn common-path
  "Fetch a common path from the JVM session.  valid keys are
   :home      - The user's home directory. 
   :docs      - The user's Documents folder (typically a windows thing, may drop).
   :javapath  - The path to java executables.
   :startdir  - The directory the JVM started in.
   :tempdir   - The directory the JVM will use for temporary files.
   :path      - A list of paths on the system Path.
   :classpath - A list of paths on the Class Path.
   :javahome  - The path to the Java Runtime Environment."
  [key] (get common-paths key))                           

(def ^:dynamic *current-dir* (common-path :startdir))

(defmacro with-currentdir
  "Supplies the most recent binding of *current-dir* as the root to with-path, 
   operating identically to with-path."
  [binds & body]
  `(with-path *current-dir* ~binds ~@body))

(defn load-script 
  "Identical to load-file, except it assumes the filename is relative to a 
   scripts directory relative to the current dir."
  [fname]
  (load-file (relative-path *current-dir* ["scrips" fname])))

;loaded from the 1.2 version of clojure.java.io, dunno why it was dropped.
(defn delete-file-recursively
  "Delete file f. If it's a directory, recursively delete all its contents.
   Raise an exception if any deletion fails unless silently is true."
  [f & [silently]]
  (let [f (io/file f)]
    (if (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-file-recursively child silently)))
    (io/delete-file f silently)))

;Generic file utilities that will be moved into a standalone file.
(defn make-folders!
  "make-parents requires a file.  This supplies a dumb file, and creates the 
   folder structure necessary."
  ([root pathlist]
    (io/make-parents (relative-path root pathlist)))
  ([root]  (make-folders! root ["blah.txt"])))

(defn clear-folders!
  "Ensures that every file in the folder path is wiped out.  A predicate 
   function pred may be supplied, which should be of type 
   pred::java.io.File -> boolean."
  ([pred path]
	  (doseq [f (filter pred (file-seq (io/file path)))]
	    (io/delete-file f)))
  ([path] (clear-folders! 
            (fn [f] (not (.isDirectory f))) path)))

(defn vector-map
  "Helper function to keep vector->vector transforms."
  [f v]
  (reduce #(conj %1 (f %2)) [] v))

(defn hock
  "A variation of spit.  hock takes the same args as spit, but ensures that 
   the parents in the path exist."
  [path contents & options]
  (let [f (io/file path)]
    (do 
	    (if (.exists f)
	      (io/delete-file path)
	      (make-folders! path []))
     (if (seq options)
       (spit f contents options)
       (spit f contents)))))

(defn list-path
  "Split the file path into a vector of strings."
  [fl] 
  (strlib/split  
    (if (string? fl) 
      fl
     (.getPath fl)) #"\\"))
  
(defn butlast-vec
  "Similar to butlast, but uses vector operations to avoid sequence op 
   overhead."
  [v]
  (when (seq v)
    (subvec v 0 (dec (count v)))))

;(defn file->mapentry [path] 
;  (let [f (file path)
;        nm (.getName f)]
;    [nm (read-string (slurp f))]))
;        
;(defn folders->map 
;  "Given a root directory, constructs a hashmap.  Filenames correspond to keys 
;   in a hashmap.  Folders are seen as nested hashmaps, the contents of which are 
;   assoced similarly."
;  [rootpath] 
;  (let [get-ext (fn [fname] (re-find 
;    (loop [m {}
;           paths [rootpath]]
;      (when (seq paths)
;	      (let [currentpath (first paths)
;	            currentmap  (get mapq currentpath)
;	            {:keys [maps nonmaps]} (get-mapkeys currentmap)]             
;         (do (doseq [fl nonmaps] 
;               (let [target (relative-path currentpath [(str (key->filename fl) 
;                                                             ".clj")])
;                     data (get currentmap fl)
;                     _    (println ["hocking " target data])]
;               (hock target data)))  
;            (recur (reduce (fn [acc fldrkey] 
;                            (assoc acc (relative-path currentpath 
;                              [(key->filename fldrkey)]) 
;                                   (get currentmap fldrkey)))
;                        (dissoc mapq currentpath) maps)))))))  


(def prefixmap {:keyword "KEY_"
                :vector "VEC_"
                :list "LIST_"})

(def suffixmap {:clj ".clj"
                :json ".json"
                :csv ".csv"})

(defn recognizer
  "Given a map of :keyword to regex string, converts strings into regex patterns.
   Used for pattern matching factories."
  [m] 
  (for [[k v] m]
     [k (re-pattern v)]))

(def regmap (recognizer prefixmap))
(def sufregmap (recognizer suffixmap))

(defn drop-prefix [s pre] 
  (subs s (count (get prefixmap pre))))

(defn matcher
  "Given a string and a recognizer map, applies each recognizer in the map until
   it finds a result.  Returns the relative key of the recognizer that matched.
   Used for string-based dispatching."
  [s rmap] 
  (loop [remaining (seq rmap)]
    (if-let [[t re] (first remaining)]      
	    (if (re-find re s)
	      t
	      (recur (rest remaining)))
     :string)))

(defn filename->type
  "Matches a filename to a prefix type, either a clojure keyword, or a :string."
  [fname]  (matcher fname regmap))
(defn filename->ext
  "Matches a filename extensions to a known suffix type (clj, json, csv)"  
  [fname] (matcher fname sufregmap))

(defn drop-ext
  "Chops the extension off a filename.  If the filename has no extension, fname
   is returned.  Extensions are assumed to be of the form [filename.extension]
   If . characters are used in the file name, infers that the last is adjacent
   to the extension.  Thus, names like 'this.is.a.really.bad.name.txt'  will 
   return 'this.is.a.really.bad.name' "
  [fname]
  (let [res 
           (let [pieces (strlib/split fname #"\.")]
             (apply str  (butlast (interleave (butlast pieces) (repeat \.)))))]
    (if (empty? res) fname res)))
    

(defmulti key->filename 
  "coerces a key in a map into a compatible filesystem key" class)
(defmethod key->filename clojure.lang.Keyword [s]
  (str (get prefixmap :keyword) (subs (str s) 1)))
(defmethod key->filename java.lang.String [s] 
  s)

(defmulti filename->key
  "returns a compatible associative key from a path."
  filename->type)

(defmethod filename->key :keyword [s] 
  (keyword (drop-prefix s :keyword)))

(defmethod filename->key :string [s] 
  (drop-ext s))

(defn drop-keys [m keys] 
  (reduce #(dissoc %1 %2) m keys))

(defn get-mapkeys [m]
    (group-by 
      #(if (map? (get m %)) 
           :mapkeys 
           :nonmapkeys) (keys m)))

(defn map->folders!
  "Unwraps a hashmap to a directory.  If rootpath does not exist, creates the 
   structure.  Keys in the map correspond to file/foldernames.  Where values are 
   hashmaps, map->folders! is called recursively with a relative path.  Values 
   that are not associatve are serialized based on filetype (normally .clj files
   with clojure strings).  If condensed "
  [m rootpath & {:keys [filetype pretty? logged? clear? condensed?]   
                 :or   {filetype :clj pretty? false logged? false
                        clear? true condensed? true}}]
  
  (let [ext (str \. (subs (str filetype) 1))
        _   (if clear? (if (.exists (io/file rootpath))
                             (delete-file-recursively rootpath)))
        fatwriter (fn [root m] 
                    (doseq [k (keys m)] ;every key gets a file.
                        (hock (relative-path root [(key->filename k)])
                              (with-out-str (prn (get m k))))))
        thinwriter (fn [root m] ;writes map as a single file
                     (let [target (relative-path root [(str "mapentries" ".clj")])
                           data (#(with-out-str 
                                     (if pretty? (pp/pprint %) (prn %)))
                                     m)                                                
                           _    (if logged? (println ["hocking " target data]))]
                       (hock target data)))
        writef     (if condensed? thinwriter fatwriter)]
    (loop [mapq {rootpath m}]
      (when (seq mapq)
	      (let [currentpath (first (keys mapq))
	            currentmap  (get mapq currentpath)
	            {:keys [mapkeys nonmapkeys]} (get-mapkeys currentmap)]                  
         (do  (println ["Files :" (drop-keys currentmap mapkeys)
                        "Folders: " (drop-keys currentmap nonmapkeys)])
              (writef currentpath (drop-keys currentmap mapkeys))
	            (recur (reduce (fn [acc fldrkey] 
	                            (assoc acc (relative-path currentpath 
	                              [(key->filename fldrkey)]) 
	                                   (get currentmap fldrkey)))
	                        (dissoc mapq currentpath) mapkeys))))))))


;define a file-backed-map....
;each file in the directory structure is the name of a key in the map...
;directories are maps.
(defn folders->map
  "Reads a folder located at path, and derives a hashmap from the folder 
   structure.  Can be used with or without map->folders! , as the derivation
   is general enough.  Deriving a hashmap follows 2 simple rules: 
   all files in the current directory are read.  Files are assumed to be 
   string-serialized clojure expressions [for now], which are read and 
   consumed into a dynamically-built hashmap.  Each directory forms a level of 
   the hashmap, with the files serving as containers for map entries, either 
   [k v] pairs, or {} maps.  {} maps are decompsed into a sequence of [k v] 
   pairs.  All [k v] pairs are reduced using assoc-in, where the key path 
   for assoc-in is relative to the directory structure and the key of the [k v]
   pair.  Directories are seen as special [k v] pairs, where the directory name
   is itself a key.  Thus, directories correspond to nested maps. "
  [path & {:keys [loadf extensions-regex logged?] 
                           :or {loadf (fn [p] 
                                        (let [s (slurp p)]
                                          (if (= s "")
                                            nil 
                                            (read-string s))))
                                extensions-regex 
                                      #"\.clj|\.CLJ|\.json|\.JSON"
                                logged? false}}]                                      
  (let [drop-path #(subs % (inc (count path)))
        get-ext #(re-find extensions-regex %)
        drop-ext #(subs % 0 (- (count %) (count (get-ext %)) ))
        fls (filter #(.isFile %) (file-seq (io/file path)))]
    (reduce (fn [accmap fl]
              (let [fpath     (.getPath fl)
                    fname     (.getName fl)
                    rawpath   (list-path (drop-path fpath))
                    cleanpath (vector-map filename->key (butlast-vec rawpath))]
                (do (if logged? (println (str "processing " fpath)))
	                  (let [data (loadf fpath)]
	                    (if (map? data)
	                      (reduce (fn [acc [k v]]
                                     (assoc-in acc (conj cleanpath k) v)) 
	                              accmap data)
                      (assoc-in  accmap (conj cleanpath (first data)) 
                                 (rest data))))))) {} fls)))

(defprotocol IResource
  (load-resource  [r])
  (read-resource  [r])
  (write-resource [r])
  (resource-type  [r]))

;canonical resources....
(def resource-core 
  (zipmap
	  ["Parameters" 
	   "DefaultPolicyRecords" 
	   "AllSupply"  
	   "AllDemand"  
	   "AllRelations"  
	   "Relations" 
	   "Periods" 
	   "Demand Records" 
	   "Requirement List"]
   (repeat ::table)))


;things we must have - derived from aforementioned core resources.
(def basic-dependencies (into #{} (keys resource-core)))

;projects have names, a version, a set of required resources (dependencies), 
;and a resource map, a map of {resource-name resource}, where resource is 
;anything from a simple parameter, to a path, to an inline data structure, 
;or any number of things.  
(defrecord project [name version dependencies resources])
(defn make-project [name & {:keys [version dependencies resources] 
                             :or {version 0.1
                                  dependencies basic-dependencies
                                  resources {}}}]
  (->project name version dependencies resources))

(defn load-project
  "Load a project.  Ensure that resources cover dependencies 
   (= #{} (diff dependencies resourcekeys))
   Load resources, and return a valid project structure 
   that contains all required structures.  This is a primitive, just-above-raw
   processing state, where we've basically just collected the data and collated
   it.  Static processing comes later."
  [{:keys [dependencies resources] :as prj}]
  (let [unresolved (setlib/difference dependencies (into #{} (keys resources)))]
    (if (seq unresolved) 
      (throw (Exception. (str "Unresolved dependencies! " unresolved)))
      (let [loaded (reduce (fn [m reskey res]
                             (assoc m reskey (load-resource res))) resources)]
        (assoc project :resources loaded)))))        

(defn test-project
  "Perform a sequence of tests on the project to verify its integrity."
  [prj testcoll]
  (reduce (fn [p tst] (tst p)) testcoll)) 

(derive ::csv        ::table)
(derive ::json       ::map)
(derive ::json-table ::table) 
(derive ::worksheet  ::table)
(derive ::workbook   ::db) 

;:clj ;eval'd, could be [:csv :json :json-table :worksheet....etc.]
              

(declare loader)
;a simple container for resources.....name is a unique identifier, path is an 
;optional path to the resource, type is intended as a contextual identifier for
;the pathed resource.  data is either nil, or an eval'd form of some reader 
;that converts path and type into actionable data (something we can consume).
(defrecord resource [name path type data]
  IResource 
  (load-resource [r] (if (not (nil? data)) 
                       r
                       (resource. name path type (loader type path))))
  (read-resource [r] data)
  (write-resource [r] nil)
  (resource-type [r] type))

;concrete stuff.
(defrecord table [fields records])
(derive table ::table)

(defn split-by [delim s]
  (strlib/split  s (re-pattern delim)))

(defn csv-line [s] (map strlib/trim (split-by "," s)))
(defn tab-line [s] (map strlib/trim (split-by (str \tab) s)))

(defn- parse-string [value]
  (if (re-matches #"\d+" value)
    (try (Integer/parseInt value)
         (catch NumberFormatException _ value))
    (try (Double/parseDouble value)
         (catch NumberFormatException _ value))))

(defn parse-records [record->vector records]
	(persistent! 
	  (reduce 
     (fn [acc raw-record]
        (conj! acc 
           (vector-map parse-string (record->vector raw-record)))) 
       (transient []) records)))

(defn delimited->table
  "Rip a delimited table into "
  [linef path]
  (with-open [source (io/reader path)] 
	  (let [lines (line-seq source)
	        fields (zipmap (linef (first lines)) (iterate inc 0))
	        unparsed   (rest lines)]
	    (->table fields (parse-records linef unparsed)))))    

(def csvfile->table #(delimited->table csv-line %))
(def tabfile->table #(delimited->table tab-line %))

(defn jsonfile->table [path]
  (with-open [rdr (io/reader path)]
    (let [obj (json/read-json-from rdr :true :true nil)]
      (when (map? obj)
        (->table (get obj :fields (get obj :column-names))  
                 (parse-records identity 
                    (get obj :records (get obj :rows))))))))

(defn cljfile->table [path] 
  (with-open [rdr (io/reader path)]
    (let [obj (read-string rdr)]
      (when (map? obj)
        (if (= (get obj :type) :table) 
          (->table (get obj :fields) (get obj :records)))))))



(defn project-properties
  "Fetches any root-level key that is not a resource.  Typically simple 
   parameters."
  [proj]
  (dissoc proj :resources))

(defn clear-project!
  "Clear the resources from a project."
  [projectroot]
  (clear-folders! projectroot))

(defn project->file! [proj rootpath]
  (let [f (io/file rootpath)]
    (if (.isDirectory f)
      (hock (relative-path rootpath ["project.clj"]) proj)
      (hock rootpath proj))))

(defn project->folders! 
  [proj rootpath & [readable?]]
  (map->folders! proj rootpath :pretty? readable?))

(defn folders->project [rootpath]
  (let [{:keys [name version dependencies resources]} (folders->map rootpath)]
    (->project name version dependencies resources)))

(comment 

(def testroot 
  "C:\\Users\\thomas.spoon\\Documents\\Marathon_NIPR\\OngoingDevelopment\\")
(def csvpath (relative-path testroot ["DemandTrends.csv"]))
(def jsonpath (relative-path testroot ["AllDemands.json"]))

;Sample Resources
(def parameters 
  {:resourcetype :map
   "LastDay"	0
   "LastDayDefault"	2550
   "Interval Length"	365
   "Number of Intervals"	0
   "ApplyAdjustments"	true
   "StartDate"	"5/24/2012 23:20"  ;this should be parameterized a bit better.
   "DefaultACPolicy"	"ACEnablerTAA"
   "DefaultRCPolicy"	"RCTAA_ReMob_Enabler"
   "DefaultNGPolicy"	"RCTAA_ReMob_Enabler"
   "DefaultGhostPolicy"	"Ghost365_30"
   "ProjectPath"	"Default"
   "SpecialACPolicy"	"ACTAA"
   "SpecialRCPolicy"	"RCTAA_ReMob"
   "SpecialNGPolicy"	"RCTAA_ReMob"
   "SpecialGhostPolicy"	"Ghost365_45"
   "DefaultSupplyPriority"	"AC"})

(def defaultpolicyrecords 
  {:resourcetype :worksheet 
   :path "MPI 23 May Extension - PolicyMod.xlsm"})

(def alldemand {:resourcetype :table
   :fields ["Type" "Enabled" "Priority" "Quantity" "DemandIndex" 
                 "StartDay" "Duration" "Overlap" "SRC" "SourceFirst" 
                 "DemandGroup" "Vignette" "Operation" "Category"]
   :records
	[["DemandRecord" true 1 1 46 901 1080 45 "SRC1" "AC" "Small" "OP21" "Foundation"] 
   ["DemandRecord" true 1 1 46 541 90 45 "SRC1" "AC" "A1" "OP1" "Foundation"]
   ["DemandRecord" true 1 1 46 721 180 45 "SRC1" "AC" "A2" "OP2" "Foundation"]
   ["DemandRecord" true 1 1 46 1081 1440 45 "SRC1" "AC" "A3" "OP3" "Foundation"] 
   ["DemandRecord" true 1 1 46 1261 270 45 "SRC1" "AC" "A4" "OP4" "Foundation"]
   ["DemandRecord" true 1 1 46 1261 1260 45 "SRC1" "AC" "A5" "OP5" "Foundation"] 
   ["DemandRecord" true 1 1 46 1441 1080 45 "SRC1" "AC" "A6" "OP6" "Foundation"]
   ["DemandRecord" true 1 1 46 1711 810 45 "SRC1" "AC" "A7" "OP7" "Foundation"] 
   ["DemandRecord" true 1 1 46 451 16 45 "SRC1" "AC" "XL" "OP8" "Surge"] 
   ["DemandRecord" true 1 3 46 467 56 45 "SRC1" "AC" "XL" "OP9" "Surge"]
   ["DemandRecord" true 1 4 46 523 40 45 "SRC1" "AC" "XL" "OP10" "Surge"] 
   ["DemandRecord" true 1 4 46 563 32 45 "SRC1" "AC" "XL" "OP11" "Surge"]
   ["DemandRecord" true 1 4 46 595 368 45 "SRC1" "AC" "XL" "OP12" "Surge"]
   ["DemandRecord" true 1 3 46 963 88 45 "SRC1" "AC" "XL" "OP13" "Surge"]
   ["DemandRecord" true 1 1 46 1051 279 45 "SRC1" "AC" "XL" "OP14" "Surge"]
   ["DemandRecord" true 1 1 46 481 73 45 "SRC1" "AC" "Large" "OP15" "Surge"]
   ["DemandRecord" true 1 1 46 554 64 45 "SRC1" "AC" "Large" "OP16" "Surge"]
   ["DemandRecord" true 1 1 46 618 48 45 "SRC1" "AC" "Large" "OP17" "Surge"] 
   ["DemandRecord" true 1 1 46 666 112 45 "SRC1" "AC" "Large" "OP18" "Surge"]
   ["DemandRecord" true 1 1 46 811 90 45 "SRC1" "AC" "Medium" "OP19" "Foundation"] 
   ["DemandRecord" true 1 1 46 901 720 45 "SRC1" "AC" "Medium" "OP20" "Foundation"]
   ["DemandRecord" true 1 3 46 91 90 45 "SRC2" "AC" "A1" "O1" "Foundational"] 
   ["DemandRecord" true 1 3 46 631 90 45 "SRC2" "AC" "A2" "O2" "Foundational"]
   ["DemandRecord" true 1 1 46 2341 180 45 "SRC2" "AC" "A3" "O3" "Foundational"]
   ["DemandRecord" true 1 2 46 451 16 45 "SRC2" "AC" "Large" "A4" "O4" "Surge"]
   ["DemandRecord" true 1 2 46 467 56 45 "SRC2" "AC" "Large" "A5" "O5" "Surge"]
   ["DemandRecord" true 1 2 46 523 40 45 "SRC2" "AC" "Large" "A6" "O6" "Surge"] 
   ["DemandRecord" true 1 2 46 563 32 45 "SRC2" "AC" "Large" "A7" "O7" "Surge"]
   ["DemandRecord" true 1 2 46 595 368 45 "SRC2" "AC" "Large" "A8" "O8" "Surge"] 
   ["DemandRecord" true 1 2 46 963 88 45 "SRC2" "AC" "Large" "A9" "O9" "Surge"]
   ["DemandRecord" true 1 2 46 1051 279 45 "SRC2" "AC" "Large" "A10" "O10" "Surge"]
   ["DemandRecord" true 1 2 46 481 73 45 "SRC2" "AC" "Small" "A11" "O11" "Surge"] 
   ["DemandRecord" true 1 2 46 554 64 45 "SRC2" "AC" "Small" "A12" "O12" "Surge"] 
   ["DemandRecord" true 1 2 46 618 48 45 "SRC2" "AC" "Small" "A13" "O13" "Surge"]
   ["DemandRecord" true 1 2 46 666 112 45 "SRC2" "AC" "Small" "A14" "O14" "Surge"]
   ["DemandRecord" true 1 2 46 1 90 45 "SRC3" "AC" "R1" "O1" "Foundational"]
   ["DemandRecord" true 1 1 46 1 2520 45 "SRC3" "AC" "R2" "O2" "Foundational"] 
   ["DemandRecord" true 1 1 46 1 2520 45 "SRC3" "AC" "R3" "O3" "Foundational"] 
   ["DemandRecord" true 1 3 46 271 90 45 "SRC3" "AC" "R4" "O4" "Foundational"] 
   ["DemandRecord" true 1 1 46 361 2160 45 "SRC3" "AC" "R5" "O5" "Foundational"] 
   ["DemandRecord" true 1 2 46 451 270 45 "SRC3" "AC" "R6" "O6" "Foundational"] 
   ["DemandRecord" true 1 2 46 721 180 45 "SRC3" "AC" "R7" "O7" "Foundational"] 
   ["DemandRecord" true 1 1 46 721 180 45 "SRC3" "AC" "R8" "O8" "Foundational"]
   ["DemandRecord" true 1 1 46 991 1530 45 "SRC3" "AC" "R9" "O9" "Foundational"]
   ["DemandRecord" true 1 2 46 1081 1440 45 "SRC3" "AC" "R10" "O10" "Foundational"]
   ["DemandRecord" true 1 1 46 1261 180 45 "SRC3" "AC" "R11" "O11" "Foundational"]
   ["DemandRecord" true 1 11 46 1261 90 45 "SRC3" "AC" "R12" "O12" "Foundational"] 
   ["DemandRecord" true 1 3 46 1261 1260 45 "SRC3" "AC" "R13" "O13" "Foundational"] 
   ["DemandRecord" true 1 3 46 1441 1080 45 "SRC3" "AC" "R14" "O14" "Foundational"]
   ["DemandRecord" true 1 1 46 1711 810 45 "SRC3" "AC" "R15" "O15" "Foundational"]
   ["DemandRecord" true 1 2 46 2071 450 45 "SRC3" "AC" "R16" "O16" "Foundational"]
   ["DemandRecord" true 1 17 46 451 16 45 "SRC3" "AC" "Large" "R17" "O17" "Surge"]
   ["DemandRecord" true 1 47 46 467 56 45 "SRC3" "AC" "Large" "R18" "O18" "Surge"]
   ["DemandRecord" true 1 62 46 523 40 45 "SRC3" "AC" "Large" "R19" "O19" "Surge"]
   ["DemandRecord" true 1 67 46 563 32 45 "SRC3" "AC" "Large" "R20" "O20" "Surge"] 
   ["DemandRecord" true 1 88 46 595 368 45 "SRC3" "AC" "Large" "R21" "O21" "Surge"]
   ["DemandRecord" true 1 64 46 963 88 45 "SRC3" "AC" "Large" "R22" "O22" "Surge"]
   ["DemandRecord" true 1 36 46 1051 279 45 "SRC3" "AC" "Large" "R23" "O23" "Surge"]
   ["DemandRecord" true 1 1 46 1330 765 45 "SRC3" "AC" "Large" "R24" "O24" "Surge"]
   ["DemandRecord" true 1 2 46 1 540 45 "SRC3" "AC" "V1" "R25" "O25" "Foundational"]
   ["DemandRecord" true 1 2 46 541 900 45 "SRC3" "AC" "V1" "R26" "O26" "Foundational"]
   ["DemandRecord" true 1 3 46 451 270 45 "SRC3" "AC" "V2" "R27" "O27" "Foundational"]
   ["DemandRecord" true 1 10 46 451 270 45 "SRC3" "AC" "V2" "R28" "O28" "Foundational"]
   ["DemandRecord" true 1 28 46 481 73 45 "SRC3" "AC" "Small" "R29" "O29" "Surge"]
   ["DemandRecord" true 1 28 46 554 64 45 "SRC3" "AC" "Small" "R30" "O30" "Surge"]
   ["DemandRecord" true 1 33 46 618 48 45 "SRC3" "AC" "Small" "R31" "O31" "Surge"]
   ["DemandRecord" true 1 25 46 666 112 45 "SRC3" "AC" "Small" "R32" "O32" "Surge"]
   ["DemandRecord" true 1 1 46 778 270 45 "SRC3" "AC" "Small" "R33" "O33" "Surge"]
   ["DemandRecord" true 1 1 46 901 900 45 "SRC3" "AC" "V3" "R34" "O34" "Foundational"]
   ["DemandRecord" true 1 7 46 811 90 45 "SRC3" "AC" "V4" "R35" "O35" "Foundational"]
   ["DemandRecord" true 1 8 46 901 1080 45 "SRC3" "AC" "V4" "R36" "O36" "Foundational"]]})

(def allrelations 
  {:resourcetype :table
   :fields["Type"	"Relation"	"Donor"	"Recepient"	"Cost"	"Enabled"]
   :records [["RelationRecord"	"sub"	"Ghostable"	"SRC1"	0	true]
             ["RelationRecord"	"sub"	"Ghostable"	"SRC2"	0	true]
             ["RelationRecord"	"sub"	"Ghostable"	"SRC3"	0	true]]})

(def relations 
  {:resourcetype :table
   :fields  ["Type"	"Relation"	"Donor"	"Recepient"	"Cost"	"Enabled"]
   :records [["RelationRecord"	"sub"	"Ghost"	"Ghostable"	4	true]
             ["RelationRecord"	"sub"	"Ghostable"	"SRC2"	0	true]]})
(def periods 
  {:resourcetype :table 
   :fields ["Type"	"Name"	"FromDay"	"ToDay"]
   :records [["PeriodRecord"	"Initialization"	0	0]
             ["PeriodRecord"	"PreSurge"	1	450]
             ["PeriodRecord"	"Surge"	451	991]
             ["PeriodRecord"	"PostSurge"	992	:inf]]})

(def demandrecords 
	 {:resourcetype :table
    :fields ["Type" "Enabled" "Priority" "Quantity" "DemandIndex" 
             "StartDay" "Duration" "Overlap" "SRC" "SourceFirst" 
             "DemandGroup" "Vignette" "Operation" "Category"]
    :records	
		 [["DemandRecord" true 1 3 46 91 90 45  "SRC2" "AC" "A1" "O1" "Foundational"]
		  ["DemandRecord" true 1 3 46 631 90 45 "SRC2" "AC" "A2" "O2" "Foundational"]
		  ["DemandRecord" true 1 1 46 2341 180 45 "SRC2" "AC" "A3" "O3" "Foundational"] 
		  ["DemandRecord" true 1 2 46 451 16 45 "SRC2" "AC" "Large" "A4" "O4" "Surge"]
		  ["DemandRecord" true 1 2 46 467 56 45 "SRC2" "AC" "Large" "A5" "O5" "Surge"] 
		  ["DemandRecord" true 1 2 46 523 40 45 "SRC2" "AC" "Large" "A6" "O6" "Surge"]
		  ["DemandRecord" true 1 2 46 563 32 45 "SRC2" "AC" "Large" "A7" "O7" "Surge"] 
		  ["DemandRecord" true 1 2 46 595 368 45 "SRC2" "AC" "Large" "A8" "O8" "Surge"]
		  ["DemandRecord" true 1 2 46 963 88 45 "SRC2" "AC" "Large" "A9" "O9" "Surge"]
		  ["DemandRecord" true 1 2 46 1051 279 45 "SRC2" "AC" "Large" "A10" "O10" "Surge"] 
		  ["DemandRecord" true 1 2 46 481 73 45 "SRC2" "AC" "Small" "A11" "O11" "Surge"] 
		  ["DemandRecord" true 1 2 46 554 64 45 "SRC2" "AC" "Small" "A12" "O12" "Surge"]
		  ["DemandRecord" true 1 2 46 618 48 45 "SRC2" "AC" "Small" "A13" "O13" "Surge"] 
		  ["DemandRecord" true 1 2 46 666 112 45 "SRC2" "AC" "Small" "A14" "O14" "Surge"]]})

(def requirementlist 
  {:resourcetype :table
   :fields   ["SRC"	"Completed"]
   :records [["SRC2"	false]]})

(def marathonproject 
  {:name "Notional Data A"
   :version 1.0 
   :resources 
   {"Parameters" parameters        
    "DefaultPolicyRecords" defaultpolicyrecords        
    "AllSupply" nil ;resource is listed but absent.
    "AllDemand" alldemand  
    "AllRelations" allrelations       
    "Relations" relations    
    "Periods" periods    
    "DemandRecords"   demandrecords       
    "RequirementList" requirementlist}})
     
;(defn project->jsonfile [path]
;  (let [_ (io/delete-file path)]
;    (with-open [fl (make-parents path)
    
(def newproject 
  (make-project "TOMTest!" 
                :version 1.0
                :resources {"Parameters" parameters        
                            "DefaultPolicyRecords" defaultpolicyrecords        
                            "AllSupply" nil ;resource is listed but absent.
                            "AllDemand" alldemand  
                            "AllRelations" allrelations       
                            "Relations" relations    
                            "Periods" periods    
                            "DemandRecords" demandrecords       
                            "RequirementList" requirementlist}))
)


;import java.io.*;
;import java.util.zip.*;
;
;public class Zip {
;   static final int BUFFER = 2048;
;   public static void main (String argv[]) {
;      try {
;         BufferedInputStream origin = null;
;         FileOutputStream dest = new 
;           FileOutputStream("c:\\zip\\myfigs.zip");
;         ZipOutputStream out = new ZipOutputStream(new 
;           BufferedOutputStream(dest));
;         //out.setMethod(ZipOutputStream.DEFLATED);
;         byte data[] = new byte[BUFFER];
;         // get a list of files from current directory
;         File f = new File(".");
;         String files[] = f.list();
;
;         for (int i=0; i<files.length; i++) {
;            System.out.println("Adding: "+files[i]);
;            FileInputStream fi = new 
;              FileInputStream(files[i]);
;            origin = new 
;              BufferedInputStream(fi, BUFFER);
;            ZipEntry entry = new ZipEntry(files[i]);
;            out.putNextEntry(entry);
;            int count;
;            while((count = origin.read(data, 0, 
;              BUFFER)) != -1) {
;               out.write(data, 0, count);
;            }
;            origin.close();
;         }
;         out.close();
;      } catch(Exception e) {
;         e.printStackTrace();
;      }
;   }
;}


;(defn map->folders!
;  "Unwraps a hashmap to a directory.  If rootpath does not exist, creates the 
;   structure.  Keys in the map correspond to file/foldernames.  Where values are 
;   hashmaps, map->folders! is called recursively with a relative path.  Values 
;   that are not associatve are serialized based on filetype (normally .clj files
;   with clojure strings). "
;  [m rootpath & {:keys [filetype pretty? logged? clear?]   
;                 :or   {filetype :clj pretty? false logged? false clear? true}}]
;  
;  (let [ext (str \. (subs (str filetype) 1))
;        _   (if clear? (if (.exists (io/file rootpath))
;                             (delete-file-recursively rootpath)))]
;    (loop [mapq {rootpath m}]
;      (when (seq mapq)
;	      (let [currentpath (first (keys mapq))
;	            currentmap  (get mapq currentpath)
;	            {:keys [maps nonmaps]} (get-mapkeys currentmap)]             
;         (do (let [target (relative-path currentpath [(str "mapentries" ".clj")])
;                   data (#(if pretty? (with-out-str (pp/pprint %)) %) 
;                          (submap maps currentmap))  
;                   _    (if logged? (println ["hocking " target data]))]
;               (hock target data))  
;            (recur (reduce (fn [acc fldrkey] 
;                            (assoc acc (relative-path currentpath 
;                              [(key->filename fldrkey)]) 
;                                   (get currentmap fldrkey)))
;                        (dissoc mapq currentpath) maps))))))))

