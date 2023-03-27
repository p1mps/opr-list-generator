(ns p1mps.fight
  (:require [p1mps.opr-list-generator :as generator]
            [cheshire.core :as json]))


(def units
  (generator/parse-data (json/parse-string  (slurp "Human Defense Force.json") true)))

(def infantry-squad
  (get units 2))

(def conscripts
  (get units 1))

(def weapon-team
  (get units 3))


(defn roll []
  (rand-int 7))

(defn hit? [unit]
  (>= (roll) (:quality unit)))

(defn wound? [unit]
  (< (roll) (:defense unit)))


(defn fight [attacker defender]
  (reduce + (for [e (:equipment attacker)]
              (if (and (hit? attacker) (wound? defender))
                1
                0))))


(fight infantry-squad infantry-squad)
