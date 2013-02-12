;This is a -currently- centralized set of data definitions used by 
;marathon.  The initial port is from Marathon v 3.7602983. 
(ns marathon.data
  (use [util.record :only [defrecord+ with-record]]
       [util.metaprogramming :only [defmany keyvals->constants]]))

;Substitutions are going to be managed as a special object...
;These are general states used to describe information about the location.
;We can actually store all kinds of details about the location in the actual
;Graph node's data. Since the structure contains variants, we could have more
;useful, descriptive
;I opted to change these to string constants, since they're more declarative 
;and require no conversion





