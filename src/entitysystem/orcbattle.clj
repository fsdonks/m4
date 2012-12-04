;from barski's book....tailored for an entity system using component based 
;architecture....

(ns entitysystem.orcbattle 
  (:use [entitysystem.store]))

(defrecord gamestate [entities monsternum])

(def new-game (->gamestate emptystore 12)) 

(defcomponent coords
  "A simple set of 2D coordinates."
  [xy] xy)

(defcomponent basicstats 
  "Stats that all active entities share."
  [{:keys [health agility strength] :as stats}] 
  stats)
(defcomponent offense
  "Entities that are capable of offense."
  [x] x)
(defcomponent race 
  "Entities that have a race."
  [race] race)
(defcomponent timer
  "A timer with an initial time."
  [init] init)
(defcomponent deathcry
  "Entities that evoke a cry when they perish."
  [description] description)
(defcomponent visage
  "A text description of the entity."
  [description] description) 

;(defcomponent events [] (initial-schedule))
;(defcomponent events [] )


;in conrad's version, structs provide accessors automatically. 
;so you get monster-health for instance.
;since we're using components....we just get the health component from 
;the entity.
(def nonzero-int (comp inc rand-int))
(defn rand-int-between
  "Returns a random integer between x1 and x2.."
  [x1 x2]  
  (+ (rand-int (- (inc x2) x1)) x1))  

(defn wimpy-stat [] (rand-int-between 1 10))
(defn strong-stat [] (rand-int-between 10 20))
(defn super-stat [] (rand-int-between 20 30))

(defn random-stats
  "Rolls up a set of random statistics for health, agility, and strength.
   Stats can be overriden using key arguments."
  [& {:keys [health agility strength]}]
    {:health (default health (wimpy-stat)) 
     :agility (default agility (wimpy-stat))
     :strength (default strength (wimpy-stat))})

(defn brawler-stats []
  (random-stats :health   (strong-stat) 
                :strength (super-stat)))

(defn rogue-stats []
  (random-stats :health   (wimpy-stat) 
                :strength (wimpy-stat)
                :agility  (super-stat)))

(defn boss-stats []
  (random-stats :health (super-stat)
                :strength (strong-stat)
                :agility (strong-stat)))

(defentity player [id]
  "A simple template for human players."
  [basicstats {:health 30 :agility 30 :strength 30}
   offense 10
   visage (str "The remnant of a lost age, standing alone against the evil that"
               " plagues this land...")
   coords {:x 0 :y 0}  
   :playertag (keyword (str "player" id))])

(defentity monster [id & {:keys [name race stats vis]}]
  [basicstats (default stats (random-stats))
   :race      (default race :generic)
   :monster   true
   visage     (default vis 
               (str "The " name " defies description!"))
   :enemy     true])

(defn simple-monster [race & [stats &rest]]
  (monster nil :race race
               :stats stats))

(defentity orc
  "Orcs are simple monsters...vicious in nature.
   Creates a random orc."
  [id & {:keys [orcstats] :or {orcstats (brawler-stats)}}]
  [(simple-monster :orc orcstats)]
  [:damage-modifier (inc (rand-int 8))
   visage "A wicked orc!"])

(defentity rogue
  "Rogues are agile enemies with weak attacks.  They have 
   the ability to snare opponents in a net, paralyzing them 
   for a round.  Creates a random rogue."
  [id & {:keys [net-probability roguestats] 
         :or   {net-probability 0.1 
                roguestats (rogue-stats)}}]
  [(simple-monster :rogue roguestats)]
  [:effects {:paralyze (default net-probability  0.1)} 
   visage "An evil rogue!"])
  
(defentity hydra
  "Hydras are multi-headed beasts that can regenerate health.
   Creates a random hydra."
  [id] 
  [(simple-monster :hydra)]
  [visage "A Malicous hydra!"
   :effects {:regeneration 1}])

(defentity slime-mold
  "Slime-molds are weak monsters that slow down prey, feasting 
   after starvation or suffocation takes hold.
   Creates a random slime-mold."
  [id]
  [(simple-monster :slime-mold)]
  [visage "A slime mold!"
   :effects {:drain :agility}])


