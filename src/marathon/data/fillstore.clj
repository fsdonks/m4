(ns marathon.port.data.fillstore)

;'Filling demands with suitable supply is important enough to merit its own object.  I devoted a ton
;'of effort into solving this (hopefully elegantly and extensibly).
;Option Explicit
;'TOM Change 11 Mar 2011
;Public fillgraph As TimeStep_FillGraph
;Public fillfunction As TimeStep_FillFunction
;Public Fills As Dictionary
;Public rendergraphs As Boolean
;Public name As String
;'Decouple
;Public OutOfScope As Dictionary
;
;'TOM change 25 April 2012
;'Public StockWatcher As TimeStep_ObserverStockWatcher
;Public allgraphs As Boolean
;Public Enum FillPhase
;    useEveryone = 0
;    onlyUseFollowons = 1
;End Enum

(defrecord fillstore [name fillgraph fillfunction 
                      fills rendergraphs outofscope
                      allgraphs])
(def empty-fillstore (fillstore. "FillStore" nil nil 
                                  {} false {} 
                                  false))
                                  

