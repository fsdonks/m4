;;A manifest for a sampl project.
;;Ideally, this map should provide a mapping of files to intended inputs for a 
;;resource in a marathon project.
{:parent-project nil
 :files {:deployments      "Deployments.txt"     ;output
         :in-scope         "InScope.txt"         ;output 
         :out-of-scope     "OutOfScope.txt"      ;output                     
         :supply-records   "SupplyRecords.txt"   ;input
         :demand-records   "DemandRecords.txt"   ;input
         :period-records   "PeriodRecords.txt"   ;input
         :relation-records "RelationRecords.txt" ;input
         :src-tag-records  "SRCTagRecords.txt"   ;input
         :parameters       "Parameters.txt"
         :composite-policy-records "CompositePolicyRecords.txt" 
         :policy-defs      "PolicyDefs.txt"
         :policy-records   "PolicyRecords.txt"}}