(ns p1mps.opr-list-generator
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure.pprint :as pprint]
            [clojure.set :as clojure.set]))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))

(defn print-upgrades [unit]
  (doseq [u (:upgrades unit)]
    (doseq [o (:options u)]
      (doseq [g (:gains o)]
        (println (:name g))
        (println "Replace:" (or (:select u) (:affects u)) (:replaceWhat u))))))

(defn print-equipment [u]
  (doseq [e (:equipment u)]
    (println (or (:name e) (:label e)))))

(defn print-units [units ]
  (doseq [u units]
    (println "=====================")
    (println "Name:" (:name u))
    (println "Quality:" (:quality u))
    (println "Defense:" (:defense u))
    (println "Size:" (:size u))
    (println "Equipment:")
    (print-equipment u)
    ;;(println "Upgrades:")
    ;;(print-upgrades u)
    ))


(defn parse-data [data]
  (let [units (:units data)
        upgrades (:upgradePackages data)]
    (reduce (fn [result unit]
              (let [upgrades-by-id (group-by :uid upgrades)
                    unit-upgrades (:upgrades unit)
                    upgrades (->> (mapcat (fn [u] (get upgrades-by-id u)) unit-upgrades)
                                  (mapcat :sections)
                                  (filter #(= (:type %) "replace")))
                    equipments (->> (:equipment unit)
                                    (mapcat #(take (:size unit) (repeat %))))]
                (conj result (-> (select-keys unit [:name :quality :defense :size])
                                 (assoc :equipment equipments)
                                 (assoc :upgrades upgrades)))))
            []
            units)))

(defn combine [unit]
  (let [equipment (group-by :name (:equipment unit))
        new-equipment (mapcat (fn [[_ values]]
                             (take (* (:size unit) 2) values))
                           equipment)]

    (assoc unit :equipment new-equipment)))

;; (or (:select u) (:affects u)) (:replaceWhat u)
(defn upgrade-equipment [unit]
  (let [upgrades (group-by :replaceWhat (:upgrades unit))
        equipment (:equipment unit)
        new-equipment (reduce (fn [result e]
                                (let [upgrade (get upgrades (:name e))
                                      gain (->> (mapcat :options upgrade)
                                                (mapcat :gains)
                                                (first))
                                      already-upgraded (count (filter #(= % (:name gain))
                                                                      (map :name result)))
                                      count-max-upgraded (reduce + (map :select upgrade))]

                                  (if (and upgrade (< already-upgraded count-max-upgraded))
                                    (conj result gain)

                                    (conj result e))))
                              []
                              equipment)]
    (assoc unit :equipment new-equipment)))


(defn set-equipment [unit]
  (let [upgrades (group-by :replaceWhat (:upgrades unit))
        rand-replace-whats (random-sample 0.5 (keys upgrades))
        rand-upgrades (reduce (fn [result upgrade]
                                (let [chosen-option (rand-nth (:options upgrade))]
                                  (conj result (assoc upgrade :options [chosen-option]))))
                              []
                              (mapcat #(get upgrades %) rand-replace-whats))
        new-unit (assoc unit :upgrades rand-upgrades)]
    (println "Replace with:")
    (print-upgrades new-unit)
    ;;(print-units [unit])
    (println "New equipment:")
    (print-equipment (upgrade-equipment new-unit))))



(def units
  (parse-data  (json/parse-string  (slurp "Human Defense Force.json") true)))


(def infantry-squad
  (get units 2))

(print-equipment
 (combine infantry-squad))



(set-equipment infantry-squad)
(println "=======================")
;;(print-units units)
