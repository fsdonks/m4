(ns marathon.ces.sampledata
  (:require [spork.util [table :as tbl]]
            [marathon [schemas :as s]]))

(def policy-templates
  {"AC11"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 182},
    "Train" {"Available" 183},
    "Available" {"Reset" 365},
    "Deployed" {"Overlapping" 365},
    "Overlapping" {"Reset" 0}},
   "Arcs"
   {"Reset->Train" 182,
    "Train->Available" 183,
    "Available->Reset" 365,
    "Deployed->Overlapping" 365,
    "Overlapping->Reset" 0},
   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Available" "Train->Available"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"Reset" "Overlapping->Reset"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelling",
    "Available" "Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping"},
   "class" "genericgr
aph",
   "sources"
   {"Reset" {"Available" 365, "Overlapping" 0},
    "Train" {"Reset" 182},
    "Available" {"Train" 183},
    "Deployed" {},
    "Overlapping" {"Deployed" 365}},
   "Disabled" {}},
  "overlap" 0,
  "name" "AC11",
  "cyclelength" 730,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 0},
 "AC12"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 182},
    "Train" {"Ready" 183},
    "Ready" {"Available" 365},
    "Available" {"Reset" 365},
    "Deployed" {"Overlapping" 365},
    "Overlapping" {"Reset" 0}},
   "Arcs"
   {"Reset->Train" 182,
    "Train->Ready" 183,
    "Ready->Available" 365,
    "Available->Reset" 365,
    "Deployed->Overlapping" 365,
    "Overlapping->Reset" 0},
   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Ready" "Train->Ready"},
    "Ready" {"Available" 
"Ready->Available"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"Reset" "Overlapping->Reset"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelling",
    "Ready" "Dwelling",
    "Available" "Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping"},
   "class" "genericgraph",
   "sources"
   {"Reset" {"Available" 365, "Overlapping" 0},
    "Train" {"Reset" 182},
    "Ready" {"Train" 183},
    "Available" {"Ready" 365},
    "Deployed" {},
    "Overlapping" {"Deployed" 365}},
   "Disabled" {}},
  "overlap" 0,
  "name" "AC12",
  "cyclelength" 1095,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 0},
 "AC13"
 {"MinDwell" 
0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 182},
    "Train" {"Ready" 183},
    "Ready" {"Available" 460},
    "Available" {"Reset" 270},
    "Deployed" {"Overlapping" 270},
    "Overlapping" {"Reset" 0}},
   "Arcs"
   {"Reset->Train" 182,
    "Train->Ready" 183,
    "Ready->Available" 460,
    "Available->Reset" 270,
    "Deployed->Overlapping" 270,
    "Overlapping->Reset" 0},
   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Ready" "Train->Ready"},
    "Ready" {"Available" "Ready->Available"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"Reset" "Overlapping->Reset"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelling",
    "Ready" "Dwelling",
    "Available" "
Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping"},
   "class" "genericgraph",
   "sources"
   {"Reset" {"Available" 270, "Overlapping" 0},
    "Train" {"Reset" 182},
    "Ready" {"Train" 183},
    "Available" {"Ready" 460},
    "Deployed" {},
    "Overlapping" {"Deployed" 270}},
   "Disabled" {}},
  "overlap" 0,
  "name" "AC13",
  "cyclelength" 1095,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 0},
 "ACFFG"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 91},
    "Train" {"Ready" 91},
    "Ready" {"Available" 183},
    "Available" {"Reset" 365},
    "Deployed" {"Overlapping" 365},
    "Overlapping" {"Reset" 0}},
   "Arcs"
   {"Reset->Train" 91,
    "Train->Ready" 91,
    "Ready->Available" 183,
    "Available->Reset" 365,
    "Deployed->Overlapping" 365,
    "Overlapping->Reset" 0},

   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Ready" "Train->Ready"},
    "Ready" {"Available" "Ready->Available"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"Reset" "Overlapping->Reset"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelling",
    "Ready" "Dwelling",
    "Available" "Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping"},
   "class" "genericgraph",
   "sources"
   {"Reset" {"Available" 365, "Overlapping" 0},
    "Train" {"Reset" 91},
    "Ready" {"Train" 91},
    "Available" {"Ready" 183},
    "Deployed" {},
    "Overlapping" {"Deployed" 365}},
   "Disabled" {}},
  "overlap" 0,
  "name" "ACFFG",
  "cyclelength" 730,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndSt
ate" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 0},
 "Ghost"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Deployable" {"Waiting" 0},
    "BehaviorChange" {},
    "Deployed" {"Overlapping" 365},
    "Spawning" {"Deployable" 0},
    "ReturnToDeployable" {"Deployable" 0},
    "Waiting" {"Deploying" 1000000000},
    "NotDeployable" {"Deployed" 0},
    "Overlapping" {"ReturnToDeployable" 0},
    "Deploying" {"NotDeployable" 0}},
   "Arcs"
   {"Spawning->Deployable" 0,
    "Deployable->Waiting" 0,
    "Waiting->Deploying" 1000000000,
    "Deploying->NotDeployable" 0,
    "NotDeployable->Deployed" 0,
    "Deployed->Overlapping" 365,
    "Overlapping->ReturnToDeployable" 0,
    "ReturnToDeployable->Deployable" 0},
   "neighbors"
   {"Spawning" {"Deployable" "Spawning->Deployable"},
    "Deployable" {"Waiting" "Deployable->Waiting"},
    "Waiting" {"Deploying" "Waiting->Deploying"},
    "Deploying
" {"NotDeployable" "Deploying->NotDeployable"},
    "NotDeployable" {"Deployed" "NotDeployable->Deployed"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping"
    {"ReturnToDeployable" "Overlapping->ReturnToDeployable"},
    "ReturnToDeployable"
    {"Deployable" "ReturnToDeployable->Deployable"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Deployable" "Deployable",
    "BehaviorChange" "BehaviorChange",
    "Deployed" "Bogging",
    "Spawning" "Spawning",
    "ReturnToDeployable" "Nothing",
    "Waiting" "Nothing",
    "NotDeployable" "NotDeployable",
    "Overlapping" "Overlapping",
    "Deploying" "Deploying"},
   "class" "genericgraph",
   "sources"
   {"Deployable" {"Spawning" 0, "ReturnToDeployable" 0},
    "BehaviorChange" {},
    "Deployed" {"NotDeployable" 0},
    "Spawning" {},
    "ReturnToDeployable" {"Overlapping
" 0},
    "Waiting" {"Deployable" 0},
    "NotDeployable" {"Deploying" 0},
    "Overlapping" {"Deployed" 365},
    "Deploying" {"Waiting" 1000000000}},
   "Disabled" {}},
  "overlap" 0,
  "name" "Ghost",
  "cyclelength" 1000000000,
  "MaxDwell" 0,
  "StartState" "Deployable",
  "Recovery" 0,
  "EndState" "ReturnToDeployable",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 0},
 "RC12"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 365},
    "Train" {"Available" 365},
    "Available" {"Reset" 365},
    "Deployed" {"Overlapping" 270},
    "Overlapping" {"DeMobilization" 0},
    "DeMobilization" {"Reset" 95}},
   "Arcs"
   {"Reset->Train" 365,
    "Train->Available" 365,
    "Available->Reset" 365,
    "Deployed->Overlapping" 270,
    "Overlapping->DeMobilization" 0,
    "DeMobilization->Reset" 95},
   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Available" "Train->Availa
ble"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"DeMobilization" "Overlapping->DeMobilization"},
    "DeMobilization" {"Reset" "DeMobilization->Reset"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelling",
    "Available" "Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping",
    "DeMobilization" "DeMobilizing"},
   "class" "genericgraph",
   "sources"
   {"Reset" {"Available" 365, "DeMobilization" 95},
    "Train" {"Reset" 365},
    "Available" {"Train" 365},
    "Deployed" {},
    "Overlapping" {"Deployed" 270},
    "DeMobilization" {"Overlapping" 0}},
   "Disabled" {}},
  "overlap" 0,
  "name" "RC12",
  "cyclelength" 1095,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG
" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 95},
 "RC14"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 365},
    "Train" {"Ready" 365},
    "Ready" {"Available" 730},
    "Available" {"Reset" 365},
    "Deployed" {"Overlapping" 270},
    "Overlapping" {"DeMobilization" 0},
    "DeMobilization" {"Reset" 95}},
   "Arcs"
   {"Reset->Train" 365,
    "Train->Ready" 365,
    "Ready->Available" 730,
    "Available->Reset" 365,
    "Deployed->Overlapping" 270,
    "Overlapping->DeMobilization" 0,
    "DeMobilization->Reset" 95},
   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Ready" "Train->Ready"},
    "Ready" {"Available" "Ready->Available"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"DeMobilization" "Overlapping->DeMobilization"},
    "DeMobilization" {"Reset" "DeMobilization->Reset"}},
   "dotAble" false
,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelling",
    "Ready" "Dwelling",
    "Available" "Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping",
    "DeMobilization" "DeMobilizing"},
   "class" "genericgraph",
   "sources"
   {"Reset" {"Available" 365, "DeMobilization" 95},
    "Train" {"Reset" 365},
    "Ready" {"Train" 365},
    "Available" {"Ready" 730},
    "Deployed" {},
    "Overlapping" {"Deployed" 270},
    "DeMobilization" {"Overlapping" 0}},
   "Disabled" {}},
  "overlap" 0,
  "name" "RC14",
  "cyclelength" 1825,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 95},
 "RC15"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 730},
    
"Train" {"Ready" 365},
    "Ready" {"Available" 730},
    "Available" {"Reset" 365},
    "Deployed" {"Overlapping" 270},
    "Overlapping" {"DeMobilization" 0},
    "DeMobilization" {"Reset" 95}},
   "Arcs"
   {"Reset->Train" 730,
    "Train->Ready" 365,
    "Ready->Available" 730,
    "Available->Reset" 365,
    "Deployed->Overlapping" 270,
    "Overlapping->DeMobilization" 0,
    "DeMobilization->Reset" 95},
   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Ready" "Train->Ready"},
    "Ready" {"Available" "Ready->Available"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"DeMobilization" "Overlapping->DeMobilization"},
    "DeMobilization" {"Reset" "DeMobilization->Reset"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelli
ng",
    "Ready" "Dwelling",
    "Available" "Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping",
    "DeMobilization" "DeMobilizing"},
   "class" "genericgraph",
   "sources"
   {"Reset" {"Available" 365, "DeMobilization" 95},
    "Train" {"Reset" 730},
    "Ready" {"Train" 365},
    "Available" {"Ready" 730},
    "Deployed" {},
    "Overlapping" {"Deployed" 270},
    "DeMobilization" {"Overlapping" 0}},
   "Disabled" {}},
  "overlap" 0,
  "name" "RC15",
  "cyclelength" 2190,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 95},
 "RCFFG"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"Reset" {"Train" 365},
    "Train" {"Ready" 365},
    "Ready" {"Available" 730},
    "Available" {"Reset" 365},
    "Deployed" {"Overlapping" 270},
    "Overlapping" {"DeMobilization" 0},
    "DeMobilization" {"Reset" 95}},
   "Arcs"
   {"Reset->Train" 365,
    "Train->Ready" 365,
    "Ready->Available" 730,
    "Available->Reset" 365,
    "Deployed->Overlapping" 270,
    "Overlapping->DeMobilization" 0,
    "DeMobilization->Reset" 95},
   "neighbors"
   {"Reset" {"Train" "Reset->Train"},
    "Train" {"Ready" "Train->Ready"},
    "Ready" {"Available" "Ready->Available"},
    "Available" {"Reset" "Available->Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"DeMobilization" "Overlapping->DeMobilization"},
    "DeMobilization" {"Reset" "DeMobilization->Reset"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"Reset" "Dwelling",
    "Train" "Dwelling",
    "Ready" "Dwelling",
    "Available" "Dwelling",
    "Deployed" "Bogging",
    "Overlapping" "Overlapping",
    "DeMobilization" "DeMobilizing"},
   "class" "genericgraph",
   "sources"
   
{"Reset" {"Available" 365, "DeMobilization" 95},
    "Train" {"Reset" 365},
    "Ready" {"Train" 365},
    "Available" {"Ready" 730},
    "Deployed" {},
    "Overlapping" {"Deployed" 270},
    "DeMobilization" {"Overlapping" 0}},
   "Disabled" {}},
  "overlap" 0,
  "name" "RCFFG",
  "cyclelength" 1825,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 95},
 "RC14ReMob"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"DeMobilization" {"Reset" 0},
    "Recovered" {"DeMobilization" 95},
    "Ready" {"Available" 730},
    "Deployed" {"Overlapping" 270},
    "Train" {"Ready" 365},
    "Available" {"Reset" 365},
    "Reset" {"Train" 365},
    "Recovery" {"Recovered" 365},
    "Overlapping" {"Recovery" 0}},
   "Arcs"
   {"Deployed->Overlapping" 270,
    "Reset->Train" 365,
    "Train->Ready" 365,
    "Ready->Available" 730,
    "Ava
ilable->Reset" 365,
    "Recovered->DeMobilization" 95,
    "DeMobilization->Reset" 0,
    "Recovery->Recovered" 365,
    "Overlapping->Recovery" 0},
   "neighbors"
   {"DeMobilization" {"Reset" "DeMobilization->Reset"},
    "Recovered" {"DeMobilization" "Recovered->DeMobilization"},
    "Ready" {"Available" "Ready->Available"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Train" {"Ready" "Train->Ready"},
    "Available" {"Reset" "Available->Reset"},
    "Reset" {"Train" "Reset->Train"},
    "Recovery" {"Recovered" "Recovery->Recovered"},
    "Overlapping" {"Recovery" "Overlapping->Recovery"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"DeMobilization" "DeMobilizing",
    "Recovered" "Recovered",
    "Ready" "Dwelling",
    "Deployed" "Bogging",
    "Train" "Dwelling",
    "Available" "Dwelling",
    "Reset" "Dwelling",
    "Recover
y" "Recovering",
    "Overlapping" "Overlapping"},
   "class" "genericgraph",
   "sources"
   {"DeMobilization" {"Recovered" 95},
    "Recovered" {"Recovery" 365},
    "Ready" {"Train" 365},
    "Deployed" {},
    "Train" {"Reset" 365},
    "Available" {"Ready" 730},
    "Reset" {"Available" 365, "DeMobilization" 0},
    "Recovery" {"Overlapping" 0},
    "Overlapping" {"Deployed" 270}},
   "Disabled" {}},
  "overlap" 0,
  "name" "RC14ReMob",
  "cyclelength" 1825,
  "MaxDwell" 0,
  "StartState" "Reset",
  "Recovery" 0,
  "EndState" "Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 95},
 "RCOpSus"
 {"MinDwell" 0,
  "StopDeployable" 0,
  "StartDeployable" 0,
  "PositionGraph"
  {"sinks"
   {"OS_Reset" {"OS_Train" 365},
    "OS_Train" {"OS_Ready" 365},
    "OS_Ready" {"OS_Available" 730},
    "OS_Available" {"OS_Reset" 365},
    "Deployed" {"Overlapping" 270},
    "Overlapping" {"DeMobilization" 0},
    "DeMobilization" {"Promotion" 95},
    "
Promotion" {}},
   "Arcs"
   {"OS_Reset->OS_Train" 365,
    "OS_Train->OS_Ready" 365,
    "OS_Ready->OS_Available" 730,
    "OS_Available->OS_Reset" 365,
    "Deployed->Overlapping" 270,
    "Overlapping->DeMobilization" 0,
    "DeMobilization->Promotion" 95},
   "neighbors"
   {"OS_Reset" {"OS_Train" "OS_Reset->OS_Train"},
    "OS_Train" {"OS_Ready" "OS_Train->OS_Ready"},
    "OS_Ready" {"OS_Available" "OS_Ready->OS_Available"},
    "OS_Available" {"OS_Reset" "OS_Available->OS_Reset"},
    "Deployed" {"Overlapping" "Deployed->Overlapping"},
    "Overlapping" {"DeMobilization" "Overlapping->DeMobilization"},
    "DeMobilization" {"Promotion" "DeMobilization->Promotion"}},
   "dotAble" false,
   "delimiter" 0,
   "directed" true,
   "rendersubs" true,
   "name" "",
   "debugme" false,
   "currentDelimiter" "->",
   "fontsize" 24,
   "nodes"
   {"OS_Reset" "Dwelling",
    "OS_Train" "Dwelling",
    "OS_Ready" "Dwelling",
    "OS_Available" "Dwelling",
    "Deployed" "Bogging",    
    "Overlapping" "Overlapping",
    "DeMobilization" "DeMobilizing",
    "Promotion" "PolicyChange"},
   "class" "genericgraph",
   "sources"
   {"OS_Reset" {"OS_Available" 365},
    "OS_Train" {"OS_Reset" 365},
    "OS_Ready" {"OS_Train" 365},
    "OS_Available" {"OS_Ready" 730},
    "Deployed" {},
    "Overlapping" {"Deployed" 270},
    "DeMobilization" {"Overlapping" 0},
    "Promotion" {"DeMobilization" 95}},
   "Disabled" {}},
  "overlap" 0,
  "name" "RCOpSus",
  "cyclelength" 1825,
  "MaxDwell" 0,
  "StartState" "OS_Reset",
  "Recovery" 0,
  "EndState" "OS_Available",
  "MaxBOG" 0,
  "EndIndex" 0,
  "class" "TimeStep_Policy",
  "MaxMOB" 95}})


;; (def ghost  
;;   {"PositionGraph" 
;;    {"sources" {"ReturnToDeployable"  {"Overlapping\n" 0}, 
;;                "NotDeployable"       {"Deploying" 0}, 
;;                "Deployable"          {"ReturnToDeployable" 0, "Spawning" 0}, 
;;                "Deploying"           {"Waiting" 1000000000}, 
;;                "Spawning"            {}, 
;;                "Waiting"             {"Deployable" 0}, 
;;                "Deployed"            {"NotDeployable" 0}, 
;;                "BehaviorChange"      {}, 
;;                "Overlapping"         {"Deployed" 365}}, 
;;     "Arcs" {"ReturnToDeployable->Deployable" 0, 
;;             "Deployed->Overlapping" 365, 
;;             "NotDeployable->Deployed" 0, 
;;             "Deploying->NotDeployable" 0, 
;;             "Overlapping->ReturnToDeployable" 0, 
;;             "Waiting->Deploying" 1000000000, 
;;             "Deployable->Waiting" 0, 
;;             "Spawning->Deployable" 0}, 
;;     "class" "genericgraph", 
;;     "sinks" {"ReturnToDeployable" {"Deployable" 0}, 
;;              "NotDeployable"      {"Deployed" 0}, 
;;              "Deployable"         {"Waiting" 0}, 
;;              "Deploying"          {"NotDeployable" 0}, 
;;              "Spawning"           {"Deployable" 0}, 
;;              "Waiting"            {"Deploying" 1000000000}, 
;;              "Deployed"           {"Overlapping" 365}, 
;;              "BehaviorChange" {}, 
;;              "Overlapping"        {"ReturnToDeployable" 0}}, 
;;     "nodes" {"ReturnToDeployable" "Nothing", 
;;              "NotDeployable" "NotDeployable", 
;;              "Deployable" "Deployable", 
;;              "Deploying" "Deploying", 
;;              "Spawning" "Spawning", 
;;              "Waiting" "Nothing", 
;;              "Deployed" "Bogging", 
;;              "BehaviorChange" "BehaviorChange", 
;;              "Overlapping" "Overlapping"}, 
;;     "directed" true, 
;;     "currentDelimiter" "->"}, 
;;    "cyclelength" 1000000000, 
;;    "class" "TimeStep_Policy", 
;;    "EndIndex" 0, 
;;    "overlap" 0, 
;;    "StopDeployable" 0, 
;;    "EndState" "ReturnToDeployable", 
;;    "name" "Ghost", 
;;    "MaxMOB" 0, 
;;    "StartState" "Deployable", 
;;    "MaxDwell" 0, "MinDwell" 0, 
;;    "StartDeployable" 0, 
;;    "Recovery" 0, 
;;    "MaxBOG" 0})
  

(def policy-records 
"TimeStamp	Type	TimeInterval	PolicyName	Template	MaxDwell	MinDwell	MaxBOG	StartDeployable	StopDeployable	Overlap 	Recovery	BOGBudget	Deltas	Remarks
4/23/2012 18:52	PolicyRecord	Day	AC12Strict	AC12	1095	730	365	730	731	45	0	Auto	{}	AC12 policy, with 1 day of availability
4/23/2012 18:52	PolicyRecord	Day	AC12	AC12	1095	730	365	550	910	45	0	Auto	{}	AC12 policy, with +/- 6 months of availability
4/23/2012 18:52	PolicyRecord	Day	AC12Loose	AC12	1095	365	365	365	1095	45	0	Auto	{}	AC12 policy, able to deploy out of ready, up to end of available
4/23/2012 18:52	PolicyRecord	Day	AC13Strict	AC13	1095	825	270	825	826	45	0	Auto	{}	AC13 policy, with 1 day of availability
4/23/2012 18:52	PolicyRecord	Day	AC13	AC13	1095	825	270	645	1005	45	0	Auto	{}	AC13 policy, with +/- 6 months of availability
4/23/2012 18:52	PolicyRecord	Day	AC13Loose	AC13	1095	365	270	365	1095	45	0	Auto	{}	AC13 policy, able to deploy out of ready, up to end of available
4/23/2012 18:52	PolicyRecord	Day	AC11	AC11	730	365	365	365	730	0	0	Auto	{}	AC11 policy, 1 year unavailable, 1 year available
4/23/2012 18:52	PolicyRecord	Day	RC14Strict	RC14	1825	730	270	1460	1461	45	0	Auto	{}	RC14 policy, with 1 day of availability
4/23/2012 18:52	PolicyRecord	Day	RC14	RC14	1825	730	270	1280	1640	45	0	Auto	{}	RC14 policy, with +/- 6 months of availability
4/23/2012 18:52	PolicyRecord	Day	RC14Loose	RC14	1825	730	270	730	1735	45	0	Auto	{}	RC14 policy, able to deploy out of ready, up to end of available
4/23/2012 18:52	PolicyRecord	Day	RC15Strict	RC15	2190	1095	270	1825	1826	45	0	Auto	{}	RC15 policy, with 1 day of availability
4/23/2012 18:52	PolicyRecord	Day	RC15	RC15	2190	1095	270	1645	2005	45	0	Auto	{}	RC15 policy, with +/- 6 months of availability
4/23/2012 18:52	PolicyRecord	Day	RC15Loose	RC15	2190	1095	270	1735	1915	45	0	Auto	{}	RC15 policy, able to deploy out of ready, up to end of available
4/23/2012 18:52	PolicyRecord	Day	RC12	RC12	1095	365	270	730	1005	0	0	Auto	{}	RC 12 policy, 2 years unavailable, 1 available.
4/23/2012 18:52	PolicyRecord	Day	GhostPermanent12	AC12	1095	730	365	550	910	45	0	Auto	{}	Ghost policy.  Unused.
4/23/2012 18:52	PolicyRecord	Day	GhostPermanent13	AC13	1095	825	270	645	1005	45	0	Auto	{}	Ghost policy.  Unused.
4/23/2012 18:52	PolicyRecord	Day	Ghost365_45	Ghost	999999999	0	365	0	999999999	45	0	Auto	{}	Ghost policy for 1 year deployments.
4/23/2012 18:52	PolicyRecord	Day	Ghost270_45	Ghost	999999999	0	270	0	999999999	45	0	Auto	{}	Ghost policy for 9 month deployments.
4/23/2012 18:52	PolicyRecord	Day	BOGForever	Ghost	999999999	0	999999999	0	999999999	0	0	Auto	{}	Policy that sends a unit into BOG state forever.
4/23/2012 18:52	PolicyRecord	Day	AC13_Enabler	AC13	1095	825	270	645	1005	30	0	Auto	{}	AC13 with 30 day overlap.
4/23/2012 18:52	PolicyRecord	Day	AC12Loose_Enabler	AC12	1095	365	365	365	1095	30	0	Auto	{}	AC12Loose with 30 day overlap
4/23/2012 18:52	PolicyRecord	Day	RC15_Enabler	RC15	2190	1095	270	1645	2005	30	0	Auto	{}	RC15 with 30 day overlap
4/23/2012 18:52	PolicyRecord	Day	RC14Loose_Enabler	RC14	1825	730	270	730	1735	30	0	Auto	{}	RC14Loose with 30 day overlap
4/23/2012 18:52	PolicyRecord	Day	Ghost365_30	Ghost	999999999	0	365	0	9999999	30	0	Auto	{}	Ghost with 30 day overlap
4/23/2012 18:52	PolicyRecord	Day	RC14Loose_3Year	RC14	2190	730	270	730	2100	45	0	Auto	{\"Available\" 365}	Added for Trudy
4/23/2012 18:52	PolicyRecord	Day	RC14ReMob	RC14ReMob	2555	730	270	730	2465	45	365	540	{\"Available\" 730}	Added for Trudy
4/23/2012 18:52	PolicyRecord	Day	RC14Loose_3Year_Enabler	RC14	2190	730	270	730	2100	45	0	Auto	{\"Available\" 365}	Added for Trudy
4/23/2012 18:52	PolicyRecord	Day	RC14ReMob_Enabler	RC14ReMob	2555	730	270	730	2465	45	365	540	{\"Available\" 730}	Added for Trudy
7/24/2012 11:11	PolicyRecord	Day	MaxUtilization	MaxUtilization	999999999	0	365	0	999999999	45	0	Auto	{}	Unconstrained Policy for AC
7/24/2012 11:11	PolicyRecord	Day	MaxUtilization_Enabler	MaxUtilization	999999999	0	365	0	999999999	30	0	Auto	{}	Unconstrained Policy for AC Enablers
7/24/2012 11:11	PolicyRecord	Day	NearMaxUtilization	NearMaxUtilization	999999999	730	270	730	999999999	45	0	Auto	{}	Unconstrained Policy for RC, injects some minimum dwell
7/24/2012 11:11	PolicyRecord	Day	NearMaxUtilization_Enabler	NearMaxUtilization	999999999	730	270	730	999999999	30	0	Auto	{}	Unconstrained Policy for RC Enablers, injects some minimum dwell
7/24/2012 11:11	PolicyRecord	Day	FFGACRoto	ACFFG	730	730	270	182	730	45	0	Auto	{\"Deployed\" -95}	FFG policy for AC
7/24/2012 11:11	PolicyRecord	Day	FFGACRoto_Enabler	ACFFG	730	730	270	182	730	30	0	Auto	{\"Deployed\" -95}	FFG policy for AC Enablers
7/24/2012 11:11	PolicyRecord	Day	FFGRCRoto	RCFFG	1825	730	270	730	1735	45	0	Auto	{}	RC14 policy, able to deploy out of ready, up to end of available
7/24/2012 11:11	PolicyRecord	Day	FFGRCRoto_Enabler	RCFFG	1825	730	270	730	1735	30	0	Auto	{}	RC14 policy, able to deploy out of ready, up to end of available
7/24/2012 11:11	PolicyRecord	Day	FFGMission	FFGMission	999999999	999999999	365	0	999999999	45	0	Auto	{}	Mission Pool Policy for Mission Units.  Keeps them from deployable supply.
7/24/2012 11:11	PolicyRecord	Day	FFGMission_Enabler	FFGMission	999999999	999999999	365	0	999999999	30	0	Auto	{}	Mission Pool Policy for Mission Units.  Keeps them from deployable supply.
7/24/2012 11:11	PolicyRecord	Day	RCOpSus	RCOpSus	1825	730	270	1216	1735	45	0	Auto	{}	FFG Operational Sustainment Policy.")

(def composite-policy-records 
  "Type	CompositeName	CompositionType	Period	Policy
PolicyScheduleRecord	ACSchedule	Periodic	PreSurge	AC13
PolicyScheduleRecord	ACSchedule	Periodic	Surge	AC11
PolicyScheduleRecord	ACSchedule	Periodic	PostSurge	AC12
PolicyScheduleRecord	RCSchedule	Periodic	PreSurge	RC15
PolicyScheduleRecord	RCSchedule	Periodic	Surge	RC12
PolicyScheduleRecord	RCSchedule	Periodic	PostSurge	RC14
PolicyScheduleRecord	ACScheduleLoose	Periodic	PreSurge	AC13Loose
PolicyScheduleRecord	ACScheduleLoose	Periodic	Surge	AC11
PolicyScheduleRecord	ACScheduleLoose	Periodic	PostSurge	AC12Loose
PolicyScheduleRecord	RCScheduleLoose	Periodic	PreSurge	RC15Loose
PolicyScheduleRecord	RCScheduleLoose	Periodic	Surge	RC12
PolicyScheduleRecord	RCScheduleLoose	Periodic	PostSurge	RC14Loose
PolicyScheduleRecord	ACBinary	Periodic	PreSurge	AC13
PolicyScheduleRecord	ACBinary	Periodic	Surge	AC11
PolicyScheduleRecord	RCScheduleMyles	Periodic	PreSurge	RC15
PolicyScheduleRecord	RCScheduleMyles	Periodic	Surge	AC11
PolicyScheduleRecord	RCScheduleMyles	Periodic	PostSurge	RC14
PolicyScheduleRecord	ScheduleBuster	Periodic	First	AC13
PolicyScheduleRecord	ScheduleBuster	Periodic	Second	AC12
PolicyScheduleRecord	ScheduleBuster	Periodic	Third	AC11
PolicyScheduleRecord	ScheduleBuster	Periodic	Fourth	AC12Loose
PolicyScheduleRecord	ScheduleBuster	Periodic	Fifth	AC13Loose
PolicyScheduleRecord	ACTAA	Periodic	PreSurge	AC13
PolicyScheduleRecord	ACTAA	Periodic	Surge	AC11
PolicyScheduleRecord	ACTAA	Periodic	PostSurge	AC12Loose
PolicyScheduleRecord	RCTAA	Periodic	PreSurge	RC15
PolicyScheduleRecord	RCTAA	Periodic	Surge	RC12
PolicyScheduleRecord	RCTAA	Periodic	PostSurge	RC14Loose
PolicyScheduleRecord	ACEnablerTAA	Periodic	PreSurge	AC13_Enabler
PolicyScheduleRecord	ACEnablerTAA	Periodic	Surge	AC11
PolicyScheduleRecord	ACEnablerTAA	Periodic	PostSurge	AC12Loose_Enabler
PolicyScheduleRecord	RCEnablerTAA	Periodic	PreSurge	RC15_Enabler
PolicyScheduleRecord	RCEnablerTAA	Periodic	Surge	RC12
PolicyScheduleRecord	RCEnablerTAA	Periodic	PostSurge	RC14Loose_Enabler
PolicyScheduleRecord	RCTAA_Floating3Year	Periodic	PreSurge	RC15
PolicyScheduleRecord	RCTAA_Floating3Year	Periodic	Surge	RC12
PolicyScheduleRecord	RCTAA_Floating3Year	Periodic	PostSurge	RC14Loose_3Year
PolicyScheduleRecord	RCTAA_Floating3Year_Enabler	Periodic	PreSurge	RC15_Enabler
PolicyScheduleRecord	RCTAA_Floating3Year_Enabler	Periodic	Surge	RC12
PolicyScheduleRecord	RCTAA_Floating3Year_Enabler	Periodic	PostSurge	RC14Loose_3Year_Enabler
PolicyScheduleRecord	RCTAA_ReMob	Periodic	PreSurge	RC15
PolicyScheduleRecord	RCTAA_ReMob	Periodic	Surge	RC12
PolicyScheduleRecord	RCTAA_ReMob	Periodic	PostSurge	RC14ReMob
PolicyScheduleRecord	RCTAA_ReMob_Enabler	Periodic	PreSurge	RC15_Enabler
PolicyScheduleRecord	RCTAA_ReMob_Enabler	Periodic	Surge	RC12
PolicyScheduleRecord	RCTAA_ReMob_Enabler	Periodic	PostSurge	RC14ReMob_Enabler
PolicyScheduleRecord	ACTAA1519	Periodic	PreSurge	FFGACRoto
PolicyScheduleRecord	ACTAA1519	Periodic	Surge	MaxUtilization
PolicyScheduleRecord	ACTAA1519_Enabler	Periodic	PreSurge	FFGACRoto_Enabler
PolicyScheduleRecord	ACTAA1519_Enabler	Periodic	Surge	MaxUtilization_Enabler
PolicyScheduleRecord	RCTAA1519	Periodic	PreSurge	FFGRCRoto
PolicyScheduleRecord	RCTAA1519	Periodic	Surge	NearMaxUtilization
PolicyScheduleRecord	RCTAA1519_Enabler	Periodic	PreSurge	FFGRCRoto_Enabler
PolicyScheduleRecord	RCTAA1519_Enabler	Periodic	Surge	NearMaxUtilization_Enabler
PolicyScheduleRecord	MissionPoolTAA1519	Periodic	PreSurge	FFGMission
PolicyScheduleRecord	MissionPoolTAA1519	Periodic	Surge	MaxUtilization
PolicyScheduleRecord	MissionPoolTAA1519_Enabler	Periodic	PreSurge	FFGMission_Enabler
PolicyScheduleRecord	MissionPoolTAA1519_Enabler	Periodic	Surge	FFGMission_Enabler
PolicyScheduleRecord	OpSustainmentPoolTAA1519	Periodic	PreSurge	RCOpSus
PolicyScheduleRecord	OpSustainmentPoolTAA1519	Periodic	Surge	NearMaxUtilization
PolicyScheduleRecord	ACExcursion	Periodic	PreSurge	AC13
PolicyScheduleRecord	ACExcursion	Periodic	Surge	MaxUtilization
PolicyScheduleRecord	ACExcursion	Periodic	PostSurge	AC12Loose
PolicyScheduleRecord	RCExcursion	Periodic	PreSurge	RC15
PolicyScheduleRecord	RCExcursion	Periodic	Surge	NearMaxUtilization_Enabler
PolicyScheduleRecord	RCExcursion	Periodic	PostSurge	RC14Loose
PolicyScheduleRecord	ACEnablerExcursion	Periodic	PreSurge	AC13_Enabler
PolicyScheduleRecord	ACEnablerExcursion	Periodic	Surge	MaxUtilization_Enabler
PolicyScheduleRecord	ACEnablerExcursion	Periodic	PostSurge	AC12Loose_Enabler
PolicyScheduleRecord	RCEnablerExcursion	Periodic	PreSurge	RC15_Enabler
PolicyScheduleRecord	RCEnablerExcursion	Periodic	Surge	NearMaxUtilization_Enabler
PolicyScheduleRecord	RCEnablerExcursion	Periodic	PostSurge	RC14Loose_Enabler
PolicyScheduleRecord	ACUnconstrained	Sequential	Sequential	MaxUtilization
PolicyScheduleRecord	RCUnconstrained	Sequential	Sequential	MaxUtilization, NearMaxUtilization
PolicyScheduleRecord	ACUnconstrained_Enabler	Sequential	Sequential	MaxUtilization_Enabler
PolicyScheduleRecord	RCUnconstrained_Enabler	Sequential	Sequential	MaxUtilization_Enabler, NearMaxUtilization_Enabler
PolicyScheduleRecord	ACMission->Rotational	Sequential	Sequential	FFGMission, FFGACRoto
PolicyScheduleRecord	RCMission->Rotational	Sequential	Sequential	FFGMission, FFGRCRoto
PolicyScheduleRecord	RCOpSus->Rotational	Periodic	Pre-Deployment	RCOpSus
PolicyScheduleRecord	RCOpSus->Rotational	Periodic	Post-Deployment	FFGRCRoto")

(def policy-defs 
"CompositeName	Composition
ACSchedule	{\"PreSurge\" \"AC13\" \"Surge\" \"AC11\" \"PostSurge\" \"AC12\"}
RCSchedule	{\"PreSurge\" \"RC15\" \"Surge\" \"RC12\" \"PostSurge\" \"RC14\"}
ACScheduleLoose	{\"PreSurge\" \"AC13Loose\" \"Surge\" \"AC11\" \"PostSurge\" \"AC12Loose\"} 
RCScheduleLoose	{\"PreSurge\" \"AC13Loose\" \"Surge\" \"AC11\" \"PostSurge\" \"AC12Loose\"} 
ACBinary	{\"PreSurge\" \"AC13\" \"Surge\" \"AC11\"}
RCScheduleMyles	{\"PreSurge\" \"RC15\" \"Surge\" \"AC11\" \"PostSurge\" \"RC14\"}
ScheduleBuster	{\"First\" \"AC13\" \"Second\" \"AC12\" \"Third\" \"AC11\" \"Fourth\" \"AC12Loose\" \"Fifth\" \"AC13Loose\"}
ACTAA	{\"PreSurge\" \"AC13\" \"Surge\" \"AC11\" \"PostSurge\" \"AC12Loose\"}
RCTAA	{\"PreSurge\" \"RC15\"  \"Surge\" \"RC12\" \"PostSurge\" \"RC14Loose\"}
ACEnablerTAA	{\"PreSurge\" \"AC13_Enabler\", \"Surge\" \"AC11\", \"PostSurge\" \"AC12Loose_Enabler\"}
RCEnablerTAA	{\"PreSurge\" \"RC15_Enabler\", \"Surge\" \"RC12\", \"PostSurge\" \"RC14Loose_Enabler\"}
RCTAA_Floating3Year	{\"PreSurge\" \"RC15\", \"Surge\" \"RC12\", \"PostSurge\" \"RC14Loose_3Year\"}
RCTAA_Floating3Year_Enabler	{\"PreSurge\" \"RC15_Enabler\", \"Surge\" \"RC12\", \"PostSurge\" \"RC14Loose_3Year_Enabler\"}
RCTAA_ReMob	{\"PreSurge\" \"RC15\", \"Surge\" \"RC12\", \"PostSurge\" \"RC14ReMob\"}
RCTAA_ReMob_Enabler	{\"PreSurge\" \"RC15_Enabler\", \"Surge\" \"RC12\", \"PostSurge\" \"RC14ReMob_Enabler\"}
ACTAA1519	{\"PreSurge\" \"FFGACRoto\", \"Surge\" \"MaxUtilization\"}
ACTAA1519_Enabler	{\"PreSurge\" \"FFGACRoto_Enabler\", \"Surge\" \"MaxUtilization_Enabler\"}
RCTAA1519	{\"PreSurge\" \"FFGRCRoto\", \"Surge\" \"NearMaxUtilization\"}
RCTAA1519_Enabler	{\"PreSurge\" \"FFGRCRoto_Enabler\", \"Surge\" \"NearMaxUtilization_Enabler\"}
MissionPoolTAA1519	{\"PreSurge\" \"FFGMission\", \"Surge\" \"MaxUtilization\"}
MissionPoolTAA1519_Enabler	{\"PreSurge\" \"FFGMission_Enabler\", \"Surge\" \"FFGMission_Enabler\"}
OpSustainmentPoolTAA1519	{\"PreSurge\" \"RCOpSus\", \"Surge\" \"NearMaxUtilization\"}
ACUnconstrained	[\"MaxUtilization\"]
RCUnconstrained	[\"MaxUtilization\", \"NearMaxUtilization\"]
ACUnconstrained_Enabler	[\"MaxUtilization_Enabler\"]
RCUnconstrained_Enabler	[\"MaxUtilization_Enabler\", \"NearMaxUtilization_Enabler\"]
ACMission->Rotational	[\"FFGMission\", \"FFGACRoto\"]
RCMission->Rotational	[\"FFGMission\", \"FFGRCRoto\"]")

(def supply-records 
"Type	Enabled	Quantity	SRC	Component	OITitle	Name	Behavior	CycleTime	Policy	Tags	SpawnTime	Location	Position	Original
SupplyRecord	FALSE	1	Ghost	Ghost	Anything	Auto	Ghost365_45	0	Ghost365_45	Auto	0	Auto	Auto	FALSE
SupplyRecord	TRUE	3	SRC1	NG	Generated_SRC1	Auto	Auto	0	RC14	Auto	0	Auto	Auto	FALSE
SupplyRecord	TRUE	3	SRC1	AC	Generated_SRC1	Auto	Auto	0	AC12	Auto	0	Auto	Auto	FALSE
SupplyRecord	TRUE	3	SRC2	NG	Generated_SRC2	Auto	Auto	0	RC15	Auto	0	Auto	Auto	FALSE
SupplyRecord	TRUE	3	SRC2	AC	Generated_SRC2	Auto	Auto	0	AC13	Auto	0	Auto	Auto	FALSE
SupplyRecord	TRUE	20	SRC3	NG	Generated_SRC3	Auto	Auto	0	RCOpSus	Auto	0	Auto	Auto	FALSE
SupplyRecord	TRUE	10	SRC3	AC	Generated_SRC3	Auto	Auto	0	FFGACRoto	Auto	0	Auto	Auto	FALSE
")

(def src-tag-records 
"Type	SRC	Tag
EnablerRecord	SRC1	Special
")

(def demand-records 
"Type	Enabled	Priority 	Quantity	DemandIndex	StartDay	Duration	Overlap	SRC	SourceFirst	DemandGroup	Vignette	Operation	Category
DemandRecord	TRUE	1	1	46	901	1080	45	SRC1	AC	Small		OP21	Foundation
DemandRecord	TRUE	1	1	46	541	90	45	SRC1	AC		A1	OP1	Foundation
DemandRecord	TRUE	1	1	46	721	180	45	SRC1	AC		A2	OP2	Foundation
DemandRecord	TRUE	1	1	46	1081	1440	45	SRC1	AC		A3	OP3	Foundation
DemandRecord	TRUE	1	1	46	1261	270	45	SRC1	AC		A4	OP4	Foundation
DemandRecord	TRUE	1	1	46	1261	1260	45	SRC1	AC		A5	OP5	Foundation
DemandRecord	TRUE	1	1	46	1441	1080	45	SRC1	AC		A6	OP6	Foundation
DemandRecord	TRUE	1	1	46	1711	810	45	SRC1	AC		A7	OP7	Foundation
DemandRecord	TRUE	1	1	46	451	16	45	SRC1	AC	XL		OP8	Surge
DemandRecord	TRUE	1	3	46	467	56	45	SRC1	AC	XL		OP9	Surge
DemandRecord	TRUE	1	4	46	523	40	45	SRC1	AC	XL		OP10	Surge
DemandRecord	TRUE	1	4	46	563	32	45	SRC1	AC	XL		OP11	Surge
DemandRecord	TRUE	1	4	46	595	368	45	SRC1	AC	XL		OP12	Surge
DemandRecord	TRUE	1	3	46	963	88	45	SRC1	AC	XL		OP13	Surge
DemandRecord	TRUE	1	1	46	1051	279	45	SRC1	AC	XL		OP14	Surge
DemandRecord	TRUE	1	1	46	481	73	45	SRC1	AC	Large		OP15	Surge
DemandRecord	TRUE	1	1	46	554	64	45	SRC1	AC	Large		OP16	Surge
DemandRecord	TRUE	1	1	46	618	48	45	SRC1	AC	Large		OP17	Surge
DemandRecord	TRUE	1	1	46	666	112	45	SRC1	AC	Large		OP18	Surge
DemandRecord	TRUE	1	1	46	811	90	45	SRC1	AC	Medium		OP19	Foundation
DemandRecord	TRUE	1	1	46	901	720	45	SRC1	AC	Medium		OP20	Foundation
DemandRecord	TRUE	1	3	46	91	90	45	SRC2	AC		A1	O1	Foundational
DemandRecord	TRUE	1	3	46	631	90	45	SRC2	AC		A2	O2	Foundational
DemandRecord	TRUE	1	1	46	2341	180	45	SRC2	AC		A3	O3	Foundational
DemandRecord	TRUE	1	2	46	451	16	45	SRC2	AC	Large	A4	O4	Surge
DemandRecord	TRUE	1	2	46	467	56	45	SRC2	AC	Large	A5	O5	Surge
DemandRecord	TRUE	1	2	46	523	40	45	SRC2	AC	Large	A6	O6	Surge
DemandRecord	TRUE	1	2	46	563	32	45	SRC2	AC	Large	A7	O7	Surge
DemandRecord	TRUE	1	2	46	595	368	45	SRC2	AC	Large	A8	O8	Surge
DemandRecord	TRUE	1	2	46	963	88	45	SRC2	AC	Large	A9	O9	Surge
DemandRecord	TRUE	1	2	46	1051	279	45	SRC2	AC	Large	A10	O10	Surge
DemandRecord	TRUE	1	2	46	481	73	45	SRC2	AC	Small	A11	O11	Surge
DemandRecord	TRUE	1	2	46	554	64	45	SRC2	AC	Small	A12	O12	Surge
DemandRecord	TRUE	1	2	46	618	48	45	SRC2	AC	Small	A13	O13	Surge
DemandRecord	TRUE	1	2	46	666	112	45	SRC2	AC	Small	A14	O14	Surge
DemandRecord	TRUE	2	2	46	1	90	45	SRC3	AC		R1	O1	Foundational
DemandRecord	TRUE	2	1	46	1	2520	45	SRC3	AC		R2	O2	Foundational
DemandRecord	TRUE	1	1	46	1	2520	45	SRC3	AC		R3	O3	Foundational
DemandRecord	TRUE	1	3	46	271	90	45	SRC3	AC		R4	O4	Foundational
DemandRecord	TRUE	1	1	46	361	2160	45	SRC3	AC		R5	O5	Foundational
DemandRecord	TRUE	1	2	46	451	270	45	SRC3	AC		R6	O6	Foundational
DemandRecord	TRUE	1	2	46	721	180	45	SRC3	AC		R7	O7	Foundational
DemandRecord	TRUE	1	1	46	721	180	45	SRC3	AC		R8	O8	Foundational
DemandRecord	TRUE	1	1	46	991	1530	45	SRC3	AC		R9	O9	Foundational
DemandRecord	TRUE	1	2	46	1081	1440	45	SRC3	AC		R10	O10	Foundational
DemandRecord	TRUE	1	1	46	1261	180	45	SRC3	AC		R11	O11	Foundational
DemandRecord	TRUE	1	11	46	1261	90	45	SRC3	AC		R12	O12	Foundational
DemandRecord	TRUE	1	3	46	1261	1260	45	SRC3	AC		R13	O13	Foundational
DemandRecord	TRUE	1	3	46	1441	1080	45	SRC3	AC		R14	O14	Foundational
DemandRecord	TRUE	1	1	46	1711	810	45	SRC3	AC		R15	O15	Foundational
DemandRecord	TRUE	1	2	46	2071	450	45	SRC3	AC		R16	O16	Foundational
DemandRecord	TRUE	1	17	46	451	16	45	SRC3	AC	Large	R17	O17	Surge
DemandRecord	TRUE	1	47	46	467	56	45	SRC3	AC	Large	R18	O18	Surge
DemandRecord	TRUE	1	62	46	523	40	45	SRC3	AC	Large	R19	O19	Surge
DemandRecord	TRUE	1	67	46	563	32	45	SRC3	AC	Large	R20	O20	Surge
DemandRecord	TRUE	1	88	46	595	368	45	SRC3	AC	Large	R21	O21	Surge
DemandRecord	TRUE	1	64	46	963	88	45	SRC3	AC	Large	R22	O22	Surge
DemandRecord	TRUE	1	36	46	1051	279	45	SRC3	AC	Large	R23	O23	Surge
DemandRecord	TRUE	1	1	46	1330	765	45	SRC3	AC	Large	R24	O24	Surge
DemandRecord	TRUE	1	2	46	1	540	45	SRC3	AC	V1	R25	O25	Foundational
DemandRecord	TRUE	1	2	46	541	900	45	SRC3	AC	V1	R26	O26	Foundational
DemandRecord	TRUE	1	3	46	451	270	45	SRC3	AC	V2	R27	O27	Foundational
DemandRecord	TRUE	1	10	46	451	270	45	SRC3	AC	V2	R28	O28	Foundational
DemandRecord	TRUE	1	28	46	481	73	45	SRC3	AC	Small	R29	O29	Surge
DemandRecord	TRUE	1	28	46	554	64	45	SRC3	AC	Small	R30	O30	Surge
DemandRecord	TRUE	1	33	46	618	48	45	SRC3	AC	Small	R31	O31	Surge
DemandRecord	TRUE	1	25	46	666	112	45	SRC3	AC	Small	R32	O32	Surge
DemandRecord	TRUE	1	1	46	778	270	45	SRC3	AC	Small	R33	O33	Surge
DemandRecord	TRUE	1	1	46	901	900	45	SRC3	AC	V3	R34	O34	Foundational
DemandRecord	TRUE	1	7	46	811	90	45	SRC3	AC	V4	R35	O35	Foundational
DemandRecord	TRUE	1	8	46	901	1080	45	SRC3	AC	V4	R36	O36	Foundational
")


(def srm-demand-records
  "Type	Enabled	Priority	Quantity	DemandIndex	StartDay	Duration	Overlap	SRC	SourceFirst	DemandGroup	Vignette	Operation	Category	Title 10_32	OITitle	Strength	Command	Location	DemandType	Theater	RIPTOA	BOG	Length	StartState	EndState
DemandRecord	TRUE	1	1	1	1	5000	0	Binder	Uniform	A	Al's Game	Al	Rotational	10	TrapperKeeper	1	Lifeguard	KO	A	Beach	0	FALSE	9999999	MD_DA_C1	R_C2
DemandRecord	TRUE	1	1	1	1	5000	0	Binder	Uniform	A	Bill's Game	Bill	Rotational	10	TrapperKeeper	1	Lifeguard	HI	A	Beach	0	FALSE	9999999	MD_DA_C1	R_C2
DemandRecord	TRUE	1	1	1	1	730	0	Binder	AC_First	B	Chuck's Game	Chuck	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	0	FALSE	730	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	240	0	Binder	AC_First	B	Chuck's Game	Dick	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	0	TRUE	240	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1700	30	Binder	AC_First	B	Ed's Game	Ed	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	30	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1640	30	Binder	AC_First	B	Frank's Game	Frank	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	30	TRUE	270	MA_NDA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	5000	0	Binder	AC_First	B	Gerry's Game	Gerry	Rotational	10	TrapperKeeper	1	Referee	CO	G	West	0	FALSE	9999999	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1370	0	Binder	RC_First	B	Hank's Game	Hank	Rotational	10	TrapperKeeper	1	Referee	North	G	North	0	FALSE	365	MA_NDA_C1	PB_C3
DemandRecord	TRUE	1	1	1	93	120	0	Rubber	Uniform	A	Paul's Game	1	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	244	150	0	Rubber	Uniform	A	Paul's Game	2	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	150	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	275	120	0	Rubber	Uniform	A	Paul's Game	3	Rotational	10	Rubber Cup	1	Goalie	AK	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	489	120	0	Plastic	Uniform	A	Paul's Game	4	Rotational	10	Plastic Cup	1	Goalie	HI	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	578	150	0	Rubber	Uniform	A	Paul's Game	5	Rotational	10	Rubber Cup	1	Goalie	AK	A	Beach	0	FALSE	150	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	640	120	0	Plastic	Uniform	A	Paul's Game	6	Rotational	10	Plastic Cup	1	Goalie	HI	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	823	120	0	Rubber	Uniform	A	Paul's Game	7	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	974	120	0	Rubber	Uniform	A	Paul's Game	8	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1005	120	0	Rubber	Uniform	A	Paul's Game	9	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	640	365	0	Rubber	Uniform	A	Charlie's Game	1	Rotational	10	Rubber Cup	1	Coach	WA	A	West	0	FALSE	365	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1370	365	0	Rubber	Uniform	A	Charlie's Game	2	Rotational	10	Rubber Cup	1	Coach	GER	A	West	0	FALSE	365	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	275	270	0	Plastic	Uniform	A	Charlie's Game	3	Rotational	10	Plastic Cup	1	Coach	ITAL	A	West	0	FALSE	270	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1005	365	0	Plastic	Uniform	A	Charlie's Game	4	Rotational	10	Plastic Cup	1	Coach	ITAL	A	West	0	FALSE	365	MD_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1583	0	Rubber	Uniform	B	Jim	1	Rotational	10	Rubber Cup	1	Referee	North	A	North	0	FALSE	365	MA_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1611	30	Steel	Uniform	B	Ted	1	Rotational	10	Steel Cup	1	Referee	Beach	A	Beach	30	FALSE	365	MA_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	1552	30	Steel	Uniform	B	Bob	1	Rotational	10	Steel Cup	1	Referee	Middle	A	Middle	30	FALSE	270	MA_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1552	0	Steel	Uniform	B	Joe	1	Rotational	10	Steel Cup	1	Referee	Continental	A	Continental	0	FALSE	365	MA_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	730	0	Plastic	Uniform	B	Maurice	1	Rotational	10	Plastic Cup	1	Referee	NAF	A	Dry	0	FALSE	365	MA_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	1552	0	Plastic/Steel	Uniform	B	Jeremy	1	Rotational	10	Plastic/Steel Cup	1	Referee	Continental	A	Continental	0	FALSE	365	MP_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	32	300	0	Rubber	Uniform	B	Jack	1	Rotational	10	Rubber Cup	1	Referee	GER	G	West	0	FALSE	300	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1784	30	Plastic	AC_First	B	Jill	1	Rotational	10	Plastic Cup	1	Referee	Middle	G	Middle	30	TRUE	270	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1642	120	Plastic	AC_First	B	Jean	1	Rotational	10	Plastic Cup	1	Referee	Middle	G	Middle	120	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1700	30	Rubber	AC_First	B	Phyllis	1	Rotational	10	Rubber Cup	1	Referee	Middle	G	Middle	30	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1460	0	Plastic	RC_First	B	Michael	1	Rotational	32	Plastic Cup	1	Referee	Beach	G	Beach	0	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1460	0	Plastic	RC_First	B	Dwight	1	Rotational	32	Plastic Cup	1	Referee	Special	G	Special	0	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	365	0	Plastic	RC_First	B	Stanley	1	Rotational	32	Plastic Cup	1	Referee	Middle	G	Middle	0	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	731	365	0	Plastic	Uniform	B	Meredith	1	Rotational	32	Plastic Cup	1	Referee	West	G	West	0	TRUE	365	MA_NDA_C1	PT_C4
  "
  )

(def srm-supply-records
  "Type	Enabled	Quantity	SRC	Component	OITitle	Name	Behavior	Cycletime	Policy	Tags	Spawntime	Location	Position	Original	Strength	Remarks	Command	Loc	StartState	Duration
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	KO	MD_DA_C1	99999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	HI	MD_DA_C1	99999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	MA_NDA_C1	510
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	Ready_C2	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	Ready_C2	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CA	MA_NDA_C1	99999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	MA_NDA_C1	150
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NC	PT_C4	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	MA_NDA_C1	210
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	PA	PB_C3	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	VA	Ready_C2	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	MN	PB_C3	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	Ready_C2	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	ID	Ready_C2	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IN	MA_NDA_C1	60
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CA	PB_C3	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	PB_C3	
SupplyRecord	TRUE	2	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	WA	Ready_C2	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	AK	PB_C3	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Coach	GER	MA_NDA_C1	120
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CO	MA_NDA_C1	270
SupplyRecord	TRUE	1	Rubber	NG	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	PA	PB_C3	
SupplyRecord	TRUE	1	Rubber	NG	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	WA	PB_C3	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	HI	PT_C4	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	HI	Ready_C2	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CO	MA_NDA_C1	210
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	MA_DA_C1	150
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	MA_NDA_C1	30
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	PT_C4	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	LA	MA_NDA_C1	60
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	PT_C4	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	MA_NDA_C1	270
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	PB_C3	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Coach	ITAL	Ready_C2	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Referee	NC	Ready_C2	240
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Referee	NC	Ready_C2	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Referee	NC	Ready_C2	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	AR	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	Ready_C2	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	OH	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	OK	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IN	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	HI	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CA	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IL	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NJ	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	WI	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	LA	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	OR	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	VT	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	FL	MA_NDA_C1	60
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IA	Ready_C2	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	PA	MA_NDA_C1	240
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	VA	MA_NDA_C1	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	PB_C3	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	MA_DA_C1	150
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	PT_C4	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	Ready_C2	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	MA_DA_C1	90
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CO	PB_C3	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	MA_DA_C1	240
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	ID	Ready_C2	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IA	PB_C3	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	MS	PB_C3	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TN	PB_C3	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NC	PT_C4	
")


(def period-records 
"Type	Name	FromDay	ToDay
PeriodRecord	Initialization	0	0
PeriodRecord	PreSurge	1	450
PeriodRecord	Surge	451	991
PeriodRecord	PostSurge	992	999999999
")

(def relation-records 
"Type	Relation	Donor	Recepient	Cost	Enabled
RelationRecord	sub	Ghost	Ghostable	4	TRUE
RelationRecord	sub	SRC3	SRC1	1	TRUE
RelationRecord	sub	SRC3	SRC2	1	FALSE
RelationRecord	sub	SRC1	SRC2	1	FALSE
RelationRecord	sub	SRC1	SRC3	1	TRUE
RelationRecord	sub	SRC2	SRC1	1	FALSE
RelationRecord	sub	SRC2	SRC3	1	FALSE
")

(def suitability-records 
"Suitability	Definition
fencedto	builtin
maxdwell	builtin
uniform	[fenced  followon maxdwell] 
mindwell	(flip maxdwell)
ac_first	[fenced followon (where-compo \"AC\") maxdwell]
rc_first	[fenced followon (where-compo \"RC\") maxdwell]
ng_first	[fenced followon (where-compo \"NG\") maxdwell]
ar_first	[fenced followon (where-compo \"AR\") maxdwell]
not_ac	(except-compo \"AC\")
title32	[(where-compo \"NG\") mindwell]")

(def parameters 
"ParameterName	Value
LastDay	0
LastDayDefault	2550
Interval Length	365
Number of Intervals	0
ApplyAdjustments	TRUE
StartDate	8/6/2014 14:09
DefaultACPolicy	ACEnablerExcursion
DefaultRCPolicy	RCEnablerExcursion
DefaultNGPolicy	RCEnablerExcursion
DefaultGhostPolicy	Ghost365_30
ProjectPath	Default
SpecialACPolicy	ACExcursion
SpecialRCPolicy	RCExcursion
SpecialNGPolicy	RCExcursion
SpecialGhostPolicy	Ghost365_45
DefaultSupplyPriority	Uniform
DefaultRecoveryTime	0
TAA1519MaxUtilizationHack	FALSE
DefaultPromotionPolicy	Auto 
DefaultDemotionPolicy	Auto 
")


;;A project is just a map of tables....
(def raw-sample-project 
  {
   ;:PolicyTemplates policy-templates 
   :PolicyRecords   policy-records
   :CompositePolicyRecords composite-policy-records
   :PolicyDefs      policy-defs
   :SupplyRecords   supply-records
   :SRCTagRecords   src-tag-records
   :DemandRecords   demand-records
   :PeriodRecords   period-records 
   :RelationRecords relation-records
   :Parameters      parameters
   :SuitabilityRecords suitability-records})

(def non-tables #{:PolicyDefs})

(def sample-tables 
  (reduce-kv (fn [acc name data]
               (assoc acc name 
                      (s/read-schema name data)))
             {:PolicyTemplates policy-templates} raw-sample-project))



(defn get-sample-records [name]
  (if (non-tables name) 
    (get sample-tables name)
    (tbl/record-seq (get sample-tables name))))


                        
  
