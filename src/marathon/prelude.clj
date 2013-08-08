;;A short description of Marathon, and its supporting infrastructure.
(ns marathon.prelude)

;;#What is Marathon?#
;;Marathon is a mechanism for analyzing the effects of Army supply, demand, and 
;;policy variations, where supply is a set of potentially deployable units, 
;;demand is a set of activities requiring a unit, and policy is a collection of
;;rules or constraints that determine a unit’s ability to fill a demand.  

;;As a design goal, Marathon seeks to validly simulate the physics of Army 
;;supply and demand, governed by policy, to analyze both the general behavior of
;;such systems and the specific effects relative to changes in supply, demand, 
;;or policy. Ultimately, Marathon is an analytic sandbox for evaluating courses 
;;of action relative to the Army Force Generation domain.  

;;#Army Force Generation#
;;Army Force Generation is a system for managing readiness, the ability for 
;;units to deploy to meet contingencies.  In general, force generation is the 
;;structured progression of increased unit readiness over time, resulting in the
;;periodic availability of trained, ready and cohesive units prepared for 
;;operational deployment in support of civil authorities and combatant commander
;;requirements.  The domain of Army Force Generation is enormous,   encompassing
;;the range of processes and resources necessary to man, equip, train, deploy, 
;;and sustain the Army’s supply of units.  

;;Out of necessity, Marathon focuses on a subset of the Force Generation
;;process, and generally holds many gross assumptions about the behavior of 
;;quite complex subsystems (such as training processes, manning, equipment, 
;;mobilization, etc.)  Even with the Force Generation domain scoped to the unit 
;;level of detail , and with complex subsystems like equipping and manning 
;;abstracted away, the variety of supply, demand, and policy options is still
;;staggering.  

;;#How Does Marathon Work?
;;Marathon typically simulates the force generation process through a 
;;coordinated set of supply, demand, and policy simulations.   The supply system
;;acts as a coordination point for polling unit availability, a dissemination 
;;channel for simulation supply events, and a general container of units.  
;;Thousands  of unique unit entities follow rotational policies that are either
;;global (shared)  or local (unique to the unit), and are directed by one or 
;;more supply systems to execute the “supply physics” dictated by the 
;;corresponding  policy.  Each unit’s simulated history can be traced, recorded,
;;and reacted to within the simulation ecosystem.  

;;#Supply#
;;Unit rotational policy generally consists of a directed sequence of states and
;;durations.   Units also have a behavior, which interprets policy to implement 
;;the desired supply-side and deployed actions.  Policies are entirely modular 
;;and variable, as are individual unit behaviors.  The decoupling of behavior 
;;and policy allows for both homogenous sets of units that appear to behave 
;;identically, as well as a diaspora of independent singletons that can apply 
;;similar behavior to different policies or interpret the same policies 
;;(via different behavior) to simulate radically different populations.  

;;The potential for unique entities allows Marathon to flexibly and modularly 
;;account for the legion of subtleties and corner-cases in the force generation 
;;problem domain.  

;;#Demand#
;;Demands are activated, and slated for filling, based on a - potentially 
;;sophisticated - user-defined priority function.  A fill system matches the 
;;highest priority demand to the most suitable supply as needed, and directs the
;;transition of units from the supply system to deployments or other states.  
;;The fill system also accounts for potentially complex unit substitution rules,
;;demand preferences, and almost any value function associated with the 
;;selection of units to fill demands.  

;;#Policy#
;;Finally, a policy system accounts for changes to policy (such as ARFORGEN 
;;suspension, variation in lifecycle length, and changes in deployment time) by 
;;enacting system-wide policy changes in response to either time or event.   
;;Policy changes automatically filter down to subscribing units, enabling a rich
;;and diverse simulation of the supply-policy-demand dynamics.
