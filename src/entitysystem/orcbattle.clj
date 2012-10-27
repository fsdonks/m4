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
(defmacro ->sentence [wordlist]
  `(apply str (interleave (map str ~wordlist) (repeat \space))))

(defspec player 
  [basicstats {:health 30 :agility 30 :strength 30}
   offense 10
   visage (str "The remnant of a lost age, standing alone against the evil that"
               " plagues this land...")
   coords {:x 0 :y 0}  
   :playertag :player1])

(defspec monster 
  [basicstats {:health (inc (rand-int 10)) 
               :agility (inc (rand-int 10))
               :strength (inc (rand-int 10))}
   :monster :generic
   visage "A monster never before seen by the likes of man!"])

(defspec monster [name race]
  [basicstats {:health 10
               :agility 10
               :strength 10}
   :race    race
   :monster name])


(defmacro defmonster 
  "Monsters will always have a race and a monster component."
  ([name race description specs components]
    `(defspec ~name 
       [build-monster ~@specs]
       [:monster ~name
        :race ~race
        visage ~description
        ~@components]))
  ([name race description]
    `(defmonster ~name ~race ~description nil nil)))
(defspec orc
  [build-monster]
  [:damage-modifier (inc (rand-int 8))
   :monster :orc 
   :race :orc 
   visage "A wicked orc!"])

(defspec hydra 
  [build-monster]
  [:visage "A Malicous hydra!"])

(defspec slime-mold 
  [build-monster]
  [visage "A slime mold!"])
  
;this is naive....
(defn health-system [state0]
  (reduce (fn [s e] (get-component :basicstats (:entities state))


(defspec flying-pig 
  [nick "pot bellied terror" 
   aged 100 
   hitpoints 2000
   locomotion flyingmotion] (->component :temperament :angry!))

(def red-pig (conj-component (spec-flying-pig :FastRedPig) (->agility 2.5)))

