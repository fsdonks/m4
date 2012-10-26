;a clojure wrapper for working with the clipboard. 
;REALLY useful for interactive work (i.e. munging data using excel for 
;interop).

(ns marathon.clipboard)
(import '[java.awt.datatransfer Clipboard DataFlavor Transferable 
                                StringSelection])
(import '[java.awt Toolkit])

(def toolkit (Toolkit/getDefaultToolkit))
(defn ^Clipboard get-clipboard []
  (.getSystemClipboard toolkit))