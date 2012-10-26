(ns marathon.port.data.simstate)

'simstate
'Data required to constitute a simulation environment.
Option Explicit

Public supplystore As TimeStep_StoreSupply
Public demandstore As TimeStep_StoreDemand
Public policystore As TimeStep_StorePolicy
Public outputStore As TimeStep_StoreOutput
Public parameters As TimeStep_Parameters
Public behaviorManager As TimeStep_ManagerOfBehavior
Public fillstore As TimeStep_StoreFill
Public context As timestep_SimContext



