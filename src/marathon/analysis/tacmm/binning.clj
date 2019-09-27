;;A namespace exploring the binning methodology
;;proposed for TACMM.
(ns marathon.analysis.tacmm.binning
  (:require [marathon.analysis :as a]
            [marathon.analysis.requirements :as r]))

;; I tend to work better from a more formal model, rather than the imperative
;; if-then rules. Here's my take in a declarative fashion (not rigorous, but
;; useful):

;; Given:

;; SRCS   = ... some set of SRCs...
;; Compos = {RA NG AR}
;; Supply(S,Compo) = an initial supply for SRC S, Component Compo
;; MOD    = {Tacmm1 Tacmm2 Tacmm3}
;; Proportion(SRC,Compo) = proportion of supply for SRC,Compo relative to total
;;                         supply Required (T,S,Compo) = TACMM Requirement for
;;                         Mod level T, SRC S, and Component Compo

;; For T in MOD, S in SRCs, Compo in Compos,

;; Find the assignment of supply to bins, Bin(T,S,Compo),

;; Constrained by:

;; Bin(T,S,Compo) <= Required(T,S,Compo)

;; Sum(T, Bin(T,S,Compo)) = Supply(S,Compo)
;; For all T in MOD, S in SRCs, Compo in Compos

;; Such that Tacmm bins are allocated maximally in priority order of Tacmm1,
;; Tacmm2, Tacmm3, forming the hierarchical objective function:

;; forall (S in SRC, Compo in Compos):

;; Max Bin(Tacmm1, S, Compo),
;;   Max Bin(Tacmm2, S, Compo),
;;      Max Bin(Tacmm3, S, Compo)

;; Or the dependent series of problems

;; Z1 = Sum(S,Compo)Bin(Tacmm1, S, Compo)
;; Z2 = Sum(S,Compo)Bin(Tacmm2, S, Compo)
;; Z3 = Sum(S,Compo)Bin(Tacmm3, S, Compo)

;; T1Max =  Max Z1
;; T2Max =  Max z2, such that Z1 = T1MAX
;; Max z3, such that z2 = T2MAX

;; Where Required(T,S,Compo) is a parameter defined by computing an M4 requirements
;; analysis, m4requirements(T,S), where the input supply is filtered by SRC S, and
;; the demand is filtered by both SRC S, and mod level T. The requirement is
;; computed per canonical requirements analysis growth assumptions, namely that we
;; have a fixed compo distribution (relative to Proportion(SRC,Compo)). This
;; is "old hat" and leverages stuff we already know how to do.

;; Caveats:

;; This method assumes that SRCs are independent, e.g. we are not binning TACMM
;; levels "between" SRCs. We are also not allowed to reinvest "mod dollars" from
;; lower-mod requirements to higher-mod requirements (there's no global resource
;; constraint, only the supply of SRCs and compo distribution). The task is
;; effectively a labeling problem, where each element of supply ultimately is given
;; a TACMM mod label (binned). The binned supply is representative of the best
;; possible TACMM levels, relative to expected requirements by TACMM level, such
;; that higher TACMM levels are binned first. This ensures minimum risk to mission.

;; Method 2(TBD):

;; The more refined excursion (aforementioned "Method 2") would include a global
;; resource allocation, where some objective would be served by modernizing
;; relatively "more important" units, where importance is currently
;; undefined (maybe proportional to STR). Say, it's "more important" to modernize 1
;; ABCT, than 100 dog teams, so maybe we'd be willing to accept lower-mod dog teams
;; to help buy another TACMM1 ABCT,etc.

;; Summary:

;; "Method 1," as restated above, is pretty simple and I should have a notional example done today.
;; The results will be a typical supply file format, of SRC, Compo, Mod, Quantity.
;; Perhaps also producing the table of Required(T,S,Compo) would be useful for
;; explaining the justification for the binning.


(comment

  (def the-path "~/repos/notional/base-testdata-v8.xlsx")
)
