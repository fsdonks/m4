;TARGET FOR REMOVAL
;This Namespace is outdated, and was a failed attempt to generate xml 
;programmatically in accordance with Excel 2007 .xlsx specs.  It's 
;not bad for learning from a library standpoint.

;a library for maniuplating xlsx files programatically...
(ns marathon.archiving.excel
  (:require [clojure.java [io :as io]]
            [util [io :as iou] [xml :as xml]]))
             

(defn to-base10
  "Converts x, a number in base frombase, to base10.  Returns a single base10 
   number."
  [frombase x]
  (if (= frombase 10) x
    (->> (seq (str x))
      (map #(Integer/parseInt (str %)))
      (map-indexed (fn [i n] (* (Math/pow frombase i) n)))         
      (reduce +))))
     
(defn convert-base
  "Returns a list of numbers in the appropriate base.  Assumes x is in base 10."
  [tobase x] 
  (loop [n x
         nums '()]
    (if (zero? n)
      nums      
      (let [[q r] [(quot n tobase) (rem n tobase)]]
        (recur q (conj nums r))))))
      
(defn base->base
  "Convert bases.  Returns a list of numbers in the new base."
  [x y]
  (fn [n] 
    (convert-base y (to-base10 x n))))

(defn col->alpha
  "Convert a column to its base26 alphabetic number, as a string."
  [n]
  (let [abcs (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        get-letters (base->base 10 26)]
    (apply str (map #(get abcs (dec %)) (get-letters n)))))  

(defn rowcol->address
  "Build a string address from a row and column."
  [row col]
  (str (col->alpha col) row)) 

(def schema "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
;basic workbook...
;<?xml version="1.0" encoding="utf-8"?>
;<x:workbook xmlns:x="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
;    <x:sheets>
;        <x:sheet name="mySheet" 
;                 sheetId="1" 
;                 r:id="Rddc7711f116045e5" 
;                 xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" />
;    </x:sheets>
;</x:workbook>

;	<sheets>
;		<sheet name="table1" sheetId="1" r:id="rId1" />
;		<sheet name="Sheet2" sheetId="2" r:id="rId2" />
;		<sheet name="Sheet3" sheetId="3" r:id="rId3" />
;	</sheets>

 
(defn workbook [sheets]
  (let [sheetf (fn [sheetname sheetid]
                (xml/make-xelm "sheet" {:name sheetname :sheetId sheetid 
                                        :r:id (str "rId" sheetid)} nil))
        s (map-indexed (fn [i s] (sheetf s (inc i))) sheets)]   
  (xml/make-xelm "workbook" {:xlmns schema 
                             :xmlns:r "http://schemas.openxmlformats.org/officeDocument/2006/relationships"} 
       (xml/make-xelm "sheets" {} s)))) 

;id maps sheet to workbook.

;relations
;<?xml version="1.0" encoding="utf-8"?>
;<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
;    <Relationship Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" 
;                  Target="/xl/worksheets/sheet.xml" 
;                  Id="Rddc7711f116045e5" />
;</Relationships>

(defn relationship 
  ([sheetkey physicalname]
	  (xml/make-xelm "Relationship" 
	     {:Id sheetkey
        :Type "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"
	      :Target (str "/xl/worksheets/" physicalname ".xml")}))
  ([physicalname] (relationship physicalname physicalname)))

(defn relations [sheetkeys]
  (xml/make-xelm "Relationships" 
     {:xmlns "http://schemas.openxmlformats.org/package/2006/relationships"}
     (map-indexed (fn [i k] (relationship (str "rId" (inc i)) k)) sheetkeys)))

(defn worksheet [sheetname & itms] 
  (xml/make-xelm "worksheet" 
   {:name sheetname
    :xmlns "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}                        
     itms))

(defn sheetData [& rows] (xml/make-xelm "sheetData" {} rows))

(defn value [v] (xml/make-xelm "v" {} v))
(defn txt [t]  (xml/make-xelm "t" {} t))
(defn intxt [t] (xml/make-xelm "is" {} (txt t)))
(defn row [rowkey & cells] (xml/make-xelm "row" {:r rowkey} cells))
(defn cell 
  ([row col val type]
      (xml/make-xelm "c" {:r (rowcol->address row col) :t type}
           (case  type 
             "inlineStr" (intxt val)
             "s" (txt val)
             (value val))))        
  ([row col val] 
    (cell row col val (if (string? val) "inlineStr" "v"))))


;worksheet
;<?xml version="1.0" encoding="utf-8"?>
;<x:worksheet xmlns:x="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
;    <x:sheetData>
;        <x:row r="1">
;            <x:c r="A1" 
;                 t="n">                  ;r = [A..z][0..n], t = [s, n, b]
;                <x:v>100</x:v>  
;            </x:c>
;        </x:row>
;    </x:sheetData>
;</x:worksheet>

(defn indexed-seq 
  ([s] (indexed-seq 0 s))
  ([idxstart s] (map-indexed (fn [i v] [(+ i idxstart) v]) s)))

(def tsttable {:fields ["First" "Last"]
               :records [["Tom" "Spoon"]
                         ["Bill" "Shatner"]]})
(defn tbl->sheet 
  ([tblname tbl]
	  (worksheet tblname
	    (apply sheetData 
		    (for [[i r] (indexed-seq 1 (concat [(:fields tbl)] (:records tbl)))]
		      (apply row i
		         (map (fn [[j c]] (cell i j c)) (indexed-seq 1 r)))))))
  ([tbl] (tbl->sheet tbl "Sheet1")))

;This is all boilerplate bs.  I'm including it exactly.
(defn appxml [sheetnames] 
  (str  
  "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>
   <Properties xmlns='http://schemas.openxmlformats.org/officeDocument/2006/extended-properties'" 
             " xmlns:vt='http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes'> "
             "<Application>Microsoft Excel</Application>" \newline
							"<DocSecurity>0</DocSecurity>" \newline
							"<ScaleCrop>false</ScaleCrop>" \newline
							"<HeadingPairs>" \newline
								"<vt:vector size='2' baseType='variant'>" \newline
									"<vt:variant>" \newline
										"<vt:lpstr>Worksheets</vt:lpstr>" \newline
									"</vt:variant>" \newline
									"<vt:variant>" \newline
										"<vt:i4>3</vt:i4>" \newline
									"</vt:variant>" \newline
								"</vt:vector>" \newline
							"</HeadingPairs>" \newline
             
             (with-out-str
	             (xml/xemit-element
	               (xml/make-xelm "TitlesOfParts" {} 
	                  (xml/make-xelm "vt:vector" {:baseType "lpstr" :size 3}
	                     (map #(xml/make-xelm "vt:lpstr" {} %) sheetnames))))) \newline
						  "<LinksUpToDate>false</LinksUpToDate>" \newline
							"<SharedDoc>false</SharedDoc>" \newline
							"<HyperlinksChanged>false</HyperlinksChanged>" \newline
							"<AppVersion>12.0000</AppVersion>" \newline
						"</Properties>"))

(def corexml  
"<?xml version='1.0' encoding='UTF-8' standalone='yes'?>
<cp:coreProperties xmlns:cp='http://schemas.openxmlformats.org/package/2006/metadata/core-properties' 
  xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:dcterms='http://purl.org/dc/terms/' xmlns:dcmitype='http://purl.org/dc/dcmitype/' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
	<dc:creator>tom</dc:creator>
	<cp:lastModifiedBy>clojure</cp:lastModifiedBy>
	<dcterms:created xsi:type='dcterms:W3CDTF'>2012-07-07T21:57:01Z</dcterms:created>
	<dcterms:modified xsi:type='dcterms:W3CDTF'>2012-07-07T21:57:29Z</dcterms:modified>
</cp:coreProperties>")

(defn contents [sheetnames]
  (str 
  "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>
	<Types xmlns='http://schemas.openxmlformats.org/package/2006/content-types'>
		<Default Extension='rels' ContentType='application/vnd.openxmlformats-package.relationships+xml' />
		<Default Extension='xml' ContentType='application/xml' />
		<Override PartName='/xl/workbook.xml' ContentType='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml' />
		<Override PartName='/docProps/app.xml' ContentType='application/vnd.openxmlformats-officedocument.extended-properties+xml' />"
  \newline
  (apply str  
      (map #(with-out-str (xml/xemit-element 
        (xml/make-xelm "Override" {:PartName (str "/xl/worksheets/" % ".xml")
                               :ContentType "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"})))
           sheetnames))
	 "<Override PartName='/docProps/core.xml' ContentType='application/vnd.openxmlformats-package.core-properties+xml' />
	</Types>"))

(def mainrels   
  "<?xml version='1.0' encoding='UTF-8' standalone='yes' ?> 
  <Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>
  <Relationship Id='rId3' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties' Target='docProps/app.xml' /> 
  <Relationship Id='rId2' Type='http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties' Target='docProps/core.xml' /> 
  <Relationship Id='rId1' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument' Target='xl/workbook.xml' /> 
  </Relationships>")
  
;build a workbook from one or more tbls
(defn tbls->xlsx
  ([tbls tblnames]
    {"_rels" {".rels" mainrels}
     "[Content_Types].xml" (contents tblnames)
     "docProps" {"app.xml" (appxml tblnames)
                  "core.xml" corexml}
     "xl" {"_rels" {"workbook.xml.rels" (xml/spit-xml (relations tblnames))}
           "workbook.xml" (xml/spit-xml (workbook tblnames))
           "worksheets"  (into {} 
	                           (for [[n t] 
	                               (map (fn [a b] [a b]) tblnames tbls)]
	                             [(str n ".xml") 
                                 (xml/spit-xml (tbl->sheet n t))]))}})
 ([tbls] (tbls->workbook tbls
            (take (count tbls) (map #(str "table" %) (iterate inc 1))))))  




