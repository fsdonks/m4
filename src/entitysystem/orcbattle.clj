;from barski's book....tailored for an entity system using component based 
;architecture....

(ns entitysystem.orcbattle 
  (:use [entitysystem.store]))

(defrecord gamestate [entities monsternum])

(def new-game (->gamestate emptystore 12)) 

(defcomponent coords [xy] xy)
(defcomponent basicstats [{:keys [health agility strength] :as stats}] stats)
(defcomponent offense [n] n)
(defcomponent actor [race] race)
(defcomponent timer [init] init)
;(defcomponent events [] (initial-schedule))
;(defcomponent events [] )
(defcomponent deathcry [description] description)
(defcomponent visage [description] description) 

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

(defspec player 
  [basicstats {:health 30 :agility 30 :strength 30}
   offense 10
   visage (str "The remnant of a lost age, standing alone against the evil that"
               " plagues this land...")
   coords {:x 0 :y 0}  
   :playertag :player1])


(defspec monster [id & {:keys [name race stats vis]}]
  [basicstats (default stats (random-stats))
   :race      (default race :generic)
   :monster   true
   visage     (default vis 
               (str "The " name " defies description!"))])

(defn simple-monster [race & [name]]
  (monster nil :name (default name race) 
               :race race))
               
(defspec orc
  "Orcs are simple monsters...vicious in nature."
  [id]
  [(monster id :stats (brawler-stats))]
  [:damage-modifier (inc (rand-int 8))
   visage "A wicked orc!"])

(defspec hydra [id] 
  [(simple-monster :hydra)]
  [:visage "A Malicous hydra!"])

(defspec slime-mold [id] 
  [(simple-monster :slime-mold)]
  [visage "A slime mold!"])

