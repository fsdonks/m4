
;;The entity factory is a port of legacy functions used to read 
;;entity information from tabular sets of data according to 
;;the marathon data schemas.  The factory's primary function is 
;;to project records onto entities (i.e. units and demands) 
;;as well as serve as a hub for registering entities and 
;;scheduling things like initial starting conditions.  
;;The entityfactory could probably be broken apart, 
;;but it's legacy functionality would have to go somewhere. 
;;For now, we'll keep it in a similar namespace, but as 
;;opportunities arise, we may be able to abastract out 
;;functionality.
(ns marathon.sim.entityfactory
  (:require [marathon [schemas :as r]]
            [spork.util [table :as tbl]]))
