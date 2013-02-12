;an abstract set of functions for working on maps, treating them as 
;simple tag databases.  tags are categories applied to multiple 
;subjects.  Subjects are unique identifiers that are subjected to a tag.
(ns util.tags)

(defprotocol ITagStore 
  (get-tags [store subject])
  (get-subjects [store tag])
  (add-tag [store tag])
  (add-subject [store subject])
  (drop-tag [store tag])
  (drop-subject [store subject]))

(defn get-tags [m subject] (get-in m [:subjects subject]))
(defn get-subjects [m tag] (get-in m [:tags tag]))
(defn add-tag [m tag] (assoc-in m [:tags tag] {}))
(defn add-subject [m subject] (assoc-in m [:subjects subject] {}))

(defn tag-subject [m tag subject]
  {:tags (assoc-in m [:tags tag] subject) 
   :subjects (assoc-in m [:subjects subject] tag)})

(defn untag-subject [m tag subject]
  {:tags (dissoc-in m [:tags tag] subject) 
   :subjects (dissoc-in m [:subjects subject] tag)})
           
