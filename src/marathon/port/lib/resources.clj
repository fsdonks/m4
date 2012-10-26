(ns marathon.port.resources)

'MarathonResources
'11 July 2012 -> Significant change in functionality and organization.
'Shifting toward a loose, data-centric model, rather than the towering hierarchy I had before.

'Generally speaking, data needs to be shared across domains in some situations, and the object model
'was encapsulating too much.  As an alternative, I figure we can just clump references together and
'use generic dictionaries to facilitate data sharing.

'The flaw with the previous design, was that TimeStep_Engine served as the universal backplane for everything.
'This isn't bad, as long as it doesn't create specific coupling that effs design changes.
'Also, VBA's garbage collector is effing terrible.  As a result, doing multiple replications or setting up
'multiple runs, created problems.  Specifically, memory leakage.  Plus, it's hard to test systems in isolation
'when there are implicit dependencies across the abstractions.

'So, in contrast, we'll loosely organize the data.  It doesnt' mean there has to be a central repository,
'although there could certainly be one.  It does mean that there a generic notion of "data access", and that
'our systems act more like pure functions; i.e. they define a set of known inputs and map the result
'to a set of known outputs.  We just pass around a dictionary or other datastore that should have references
'to everything the functions need.  Then, all we have to do is unpack the needed data from the store.
'I'll probably some proxy for the TimeStep_Engine bit, specifically the glue code.  We either need a light
'object, or a module, to glue the systems together and to propogate data across one or more stores.

'This is core business logic for marathon.  Specifically, for managing resources, and porting most of the
'logic that was in TimeStep_Engine.
'This is currently just a loose wrapper around the dictionary library.
'We're using dictionaries to bundle the resources for the simulation...
'That lets us do things like merge and the like.
'It also lets the data specification be a bit more flexible.
'We can even be generic in our type signatures, if we want to be lazy...

'I'm currently porting the class-specific junk from the existing object hierarchy into independent functional
'modules.

'Coupling between classes has prompted a looser approach to resource sharing and definition.
'We don't need a class to store data...
'Just use dictionaries.
'However, we'd like there to be consistency.
'So, we define some methods for adding cannonical resources to a dictionary.

Option Explicit
Public Const demandstr As String = "DemandStore"
Public Const supplystr As String = "SupplyStore"
Public Const policystr As String = "PolicyStore"
Public Const outputstr As String = "OutputStore"
Public Const behaviormgr As String = "BehaviorManager"
Public Const simcontext As String = "SimContext"
Public Const parameters As String = "Parameters"
Public Const fillstr As String = "FillStore"

'Fetches a resource from a dictionary of resources, assumably canonical marathon-ish inputs.
'Most resources are just derived from the old object model, to help separate data from functions.
Public Function getResource(res As Dictionary, resname As String) As Variant
assert res.exists(resname), True

If IsObject(res(resname)) Then
    Set getResource = res(resname)
Else
    getResource = res(resname)
End If

End Function

'Generic function that adds resources to a dictionary. Overwrites existing resources with v.
Public Function setResource(res As Dictionary, k As String, v) As Dictionary
If res.exists(k) Then res.Remove k
res.add k, v

Set setResource = res
        
End Function

'Functions for easily adding canonical resources to a resource set.
Public Function setSupply(res As Dictionary, supply As TimeStep_StoreSupply) As Dictionary
Set setSupply = setResource(res, supplystr, supply)
End Function

Public Function setDemand(res As Dictionary, demand As TimeStep_StoreDemand) As Dictionary
Set setDemand = setResource(res, demandstr, demand)
End Function

Public Function setPolicy(res As Dictionary, policy As TimeStep_StorePolicy) As Dictionary
Set setPolicy = setResource(res, policystr, policy)
End Function

Public Function setOutput(res As Dictionary, output As TimeStep_StoreOutput) As Dictionary
Set setOutput = setResource(res, outputstr, output)
End Function

Public Function setContext(res As Dictionary, context As timestep_SimContext) As Dictionary
Set setContext = setResource(res, simcontext, context)
End Function

'Gives us a set of resources with minimal stuff in it.  Probably should rename this schlock.
Public Function blankResources() As Dictionary
Set blankResources = _
    newdict(demandstr, New TimeStep_StoreDemand, _
            supplystr, New TimeStep_StoreSupply, _
            policystr, New TimeStep_StorePolicy, _
            outputstr, New TimeStep_StoreOutput, _
            behaviormgr, New TimeStep_ManagerOfBehavior, _
            simcontext, SimLib.makeContext(), _
            parameters, New TimeStep_Parameters, _
            fillstr, New TimeStep_StoreFill)
End Function

'Given a resources dictionary, produce a proxy object that references the canonical
'resources in the underlying dictionary.
Public Function reifyCoreData(res As Dictionary) As TimeStep_SimState
Set reifyCoreData = New TimeStep_SimState

With reifyCoreData
    Set .behaviorManager = getResource(res, behaviormgr)
    Set .context = getResource(res, simcontext)
    Set .demandstore = getResource(res, demandstr)
    Set .fillstore = getResource(res, fillstr)
    Set .outputStore = getResource(res, outputstr)
    Set .parameters = getResource(res, parameters)
    Set .supplystore = getResource(res, supplystr)
    Set .policystore = getResource(res, policystr)
End With
    
End Function

Public Function excelToCore()

End Function


