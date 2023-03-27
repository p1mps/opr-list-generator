(ns p1mps.units
  (:require [p1mps.opr-list-generator :as generator]
            [cheshire.core :as json]))

(def units
  (generator/parse-data (json/parse-string  (slurp "Human Defense Force.json") true)))

(def commander
  (get units 0))

(def storm-leader
  (get units 1))

(def conscripts
  (get units 2))

(def infantry-squad
  (get units 3))

(def weapon-teams
  (get units 4))

(def veterans
  (get units 5))

(def storm-troopers
  (get units 6))

(def special-weapons
  (get units 7))

(def snipers
  (get units 8))

(def ogres
  (get units 9))

(def cavalry
  (get units 10))

(def truck
  (get units 11))

(def apc
  (get units 12))

(def attack-vehicle
  (get units 13))

(def support-vehicle
  (get units 14))

(def tank
  (get units 15))



(generator/set-equipment tank)
