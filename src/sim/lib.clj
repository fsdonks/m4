
; a port of the simlib module.
;2/27/2013 5:13:39 PM
;Provides a wrapper for a several functions that interact with a generic simulation
;context.  The simulation context is a TimeStep_SimContext object (Currently), which
;wraps several components vital to any discrete event simulation:
;  The update manager:
;    An object that tracks requests for updates (akin to wake events for threads or processes).
;  The scheduler:
;    An object that tracks and schedules time events (i.e. an agenda, or abstract clock).
;  The manager of events:
;    An object that propogates event information via an Observable/Observer design pattern.
;    Maintains event registrations, and allows clients to notify observers of certain events.

(ns sim.lib)


