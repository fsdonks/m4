;from barski's book....tailored for an entity system using component based 
;architecture....

(ns entitysystem.orcbattle 
  (:use [entitysystem.core
         DEVS.schedule]))

(defrecord gamestate [entities monsternum])

(def new-game (->gamestate emptybase 12)) 

(defcomponent coords [xy] xy)
(defcomponent basicstats [{:keys [health agility strength] :as stats}] stats)
(defcomponent offense [n] n)
(defcomponent actor [race] race)
(defcomponent timer [init] init)
(defcomponent events [] (initial-schedule))
(defcomponent deathcry [description] description)
(defcomponent visage [description] description) 

(defspec player 
  [basicstats {:health 30 :agility 30 :strength 30}
   offense 10
   visage (str "The remnant of a lost age, standing alone against the evil that"
               " plagues this land...")
   coords {:x 0 :y 0}]
  (->component  :playertag :player1))

(defspec monster 
  [basicstats {:health (inc (rand-int 10)) 
               :agility (inc (rand-int 10))
               :strength (inc (rand-int 10))}

(defn evil-orc [] 
  ((spec-monster)
    (conj-component 

(defn dead? [stats] (<= 0 (:health stats))) 
(defn active? [stats] (>= 0 (:agility stats))) 
(defn damage [strength] 
;this is naive....
(defn health-system [state0]
  (reduce (fn [s e] (get-component :basicstats (:entities state))


(defspec flying-pig 
  [nick "pot bellied terror" 
   aged 100 
   hitpoints 2000
   locomotion flyingmotion] (->component :temperament :angry!))

(def red-pig (conj-component (spec-flying-pig :FastRedPig) (->agility 2.5)))

