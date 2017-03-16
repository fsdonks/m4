;;DISCLAIMER EXPLORATORY NS

;; For compatibility purposes, we need to allow legacy categories to hold.
;; For instance, Title32, NonBog, and Rotational are all categories that had
;; meaning in legacy marathon...We can either define categories as rules that
;; matter, i.e. built in, or allow some flexibility in the rule definition inline.
;; Categories tell us where to look for supply, they should map to a query that
;; allows for a fundamental categorization of supply.  After the supply is
;; categorized, we can sort it.

;; The mechanics of finding supply are not too bad, since we're typically
;; dealing with a very small problem domain.  What is vital, is to be able to
;; express the patterns we're interested in, particularly since we have so many
;; possible niche relationships.
;; Taking a page out of logic programming, semantic web stuff, is going to help us
;; here I think.  We already encode a lot of knowledge in graphs....so we can take
;; advantage of our existing libs.
(ns marathon.ces.categories)

;;Looking at the existing picture, we maintain a cache of relationships....
;;primarily maps of relations...
;;Like....u1 is a unit...
;;u1 is in src1-deployables
;;src1-deployables is in deployables
;;ergo u1 is deployable.

;;u1 is an src1-supply
;;src1-supply is viable for src1-demand
;;src1-supply is viable for src1-src3sub
;;src1-src3sub is viable for src3-demand
;;src1-src3sub costs 3

;;d1 is a demand...
;;d1 is unfilled
;;d1 is an src1-demand

;;d1 prefers ac
;;d1 requires src1

;;to determine if u1 can fill d1,
;;we could walk the graph...

;;Each fact corresponds to an edge in the
;;knowledgebase.  So, pattern matching on
;;the edges produces a seqeunce of sets of
;;variables that correspond to a combination
;;of feasible variables relative to the
;;constraints.
(def query
  '[[d1 capability SRCDemanded?]
    [u? type Supply]
    [u? part-of Deployable]
    [u? capability ?SRCSupplied]
    [or [SRCSupplied? = SRCDemanded?]
     [and [SRCSupplied? viable Sub?]
      [Sub? viable SRCDemanded?]]]
    [or [Sub? costs cost?]
     [cost? = 0]]
    [u? readiness r?]
    [r? in #{C2 C3}]])

;;we could even break the query up into
;;smaller queries to make life easier.
(defn deployable?
  [u] 
  [u part-of Deployable])

(defn substituable [l r]
  [or [l == r]
   [and [l viable sub?]
    [sub? viable r]]])

;;Intersting ideads here...
;;we can actually map our underlying db into
;;something akin to a model
;;for a logic engine to search....

;;The current setup more or less ignores category, unless we're
;;using nonbog or something like it.
;;We need to lock down exactly what category implies...
;;Currently it's also a single value...
;;can we compose categories ordinally?
;;one possibility is that we have
;;default categories built in - ala
;;rotational, non-bog, title32, etc.
;;If the category does not map to a known
;;cat, we throw an error...
;;For now, that's probably the best approach...

;;The legacy implementation uses categories to
;;determine behavior, not fill...

;;We now need to use categories to determine
;;fill and behavior....
;;The implication is, if the category is
;;SRM, then we use extra information in the
;;demand to determine things.  We can unify
;;this in a semantic categorical model later,
;;for now it's getting hardcoded.

;;The properties that SRM cares about are
;;C-level of readiness...
;;Since deployability is so context sensitive,
;;it doesn't make "much" sense to try to bin units
;;by deployable rating.  However, the new discriminator
;;is c-level....
;;We could take the time to cache units as they change
;;c-level...
;;In fact, we typically want to know what their
;;c-level is at any point in time, so it might
;;be a good idea to do this for metrics if nothing
;;else...

;;For fill purposes, category only matters (for now)
;;as a restriction if the demand has SRM as its category.
;;We can change it later, but it'll help us
;;implement behavior now.
