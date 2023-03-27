(ns p1mps.opr-list-generator
  (:gen-class)
  (:require [cheshire.core :as json]
            [clojure.pprint :as pprint]
            [clojure.string :as string]
            [clojure.set :as clojure.set]))


(defn print-upgrades [unit]
  (doseq [u (:upgrades unit)]
    (doseq [o (:options u)]
      (doseq [g (:gains o)]
        (println (:name g))
        (println "Replace:" (or (:select u) (:affects u)) (:replaceWhat u))))))

(defn cost [unit]
  (let [unit-cost (:cost unit)
        eq-cost (reduce + (remove nil? (map :cost (:equipment unit))))]
    (+ unit-cost eq-cost)))

(defn print-equipment [u]
  ;; (doseq [e (:equipment u)]
  ;;   (println (or (:name e) (:label e))))
  (let [e (group-by :label (:equipment u))]
    (doseq [[weapon values] e]
      (if-not weapon
        (print (count values) (:name (first (first (get (group-by :name (:equipment u)) weapon)))) " ")
        (print (count values) weapon " ")))
    (println)))



(defn print-unit [unit]
  (println "=====================")
  (println (assoc (select-keys unit [:name :quality :defense :size])
                  :cost (cost unit)))
  (print-equipment unit))


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
                (conj result (-> (select-keys unit [:name :quality :defense :size :cost])
                                 (assoc :equipment equipments)
                                 (assoc :upgrades upgrades)))))
            []
            units)))

(defn combine [unit]
  (let [equipment (group-by :name (:equipment unit))
        new-equipment (mapcat (fn [[_ values]]
                                (take (* (:size unit) 2) values))
                              equipment)]
    (-> (update unit :size (partial * 2))
        (assoc :equipment new-equipment))))

(defn upgrade-by-label [upgrades-by-label label]
  (or (get upgrades-by-label
           (str "Replace any " (string/join "" (drop-last label))))
      (get upgrades-by-label
           (str "Replace any " label))
      (get upgrades-by-label
           (str "Replace " label))))


;; (or (:select u) (:affects u)) (:replaceWhat u)
(defn upgrade-equipment [unit]
  (let [upgrades (group-by :replaceWhat (:upgrades unit))
        upgrades-by-label (group-by :label (:upgrades unit))

        equipment (:equipment unit)
        new-equipment (reduce (fn [result e]
                                (let [upgrade (get upgrades (:name e))
                                      upgrade (if-not upgrade
                                                (upgrade-by-label upgrades-by-label (:label e))
                                                upgrade)
                                      cost-gain (reduce + (->> (mapcat :options upgrade)
                                                               (map :cost)))
                                      gain (->> (mapcat :options upgrade)
                                                (mapcat :gains)
                                                (rand-nth))
                                      gain (assoc gain :cost cost-gain)
                                      already-upgraded (count (filter #(= % (:name gain))
                                                                      (map :name result)))
                                      affects (first (map :affects upgrade))
                                      count-max-upgraded (if (and affects (not= affects "any"))
                                                           (reduce + (map :select upgrade))
                                                           (:size unit))]

                                  (cond
                                    (and upgrade already-upgraded count-max-upgraded  (< already-upgraded count-max-upgraded)) (conj result gain)
                                    (= affects "any") (conj result (take (:size unit) (repeat gain)))
                                    :else (conj result e))))
                              []
                              equipment)]
    (assoc unit :equipment new-equipment)))


(defn set-equipment [unit]
  (let [upgrades (group-by :replaceWhat (:upgrades unit))
        rand-replace-whats (keys upgrades)
        rand-upgrades (reduce (fn [result upgrade]
                                (let [chosen-option (rand-nth (:options upgrade))]
                                  (conj result (assoc upgrade :options [chosen-option]))))
                              []
                              (mapcat #(get upgrades %) rand-replace-whats))
        new-unit (assoc unit :upgrades rand-upgrades)
        new-unit (upgrade-equipment new-unit)]
    new-unit))



(def units
  (parse-data (json/parse-string  (slurp "Human Defense Force.json") true)))


(->> (json/parse-string  (slurp "Human Defense Force.json") true)
     :upgradePackages
     (map :hint))


(->> (json/parse-string  (slurp "Human Defense Force.json") true)
     :units
     (map :name))

(def infantry-squad
  (get units 2))

(def conscripts
  (get units 1))

(def weapon-team
  (get units 3))

(def combined-infantry-squad
  (combine infantry-squad))

(defn list-cost [l]
  (reduce + (map :cost l)))

(defn make-valid [l]
  (loop [l (take 10 (shuffle l))
         cost (list-cost l)]
    (if (<= cost 2000)
      l
      (let [new-list (drop 1 (shuffle l))]
        (recur
         new-list
         (list-cost new-list))))))


(defn generate-list [units]
  (let [l (loop [cost 0
                 result []]
            (if (>= cost 2000)
              result
              (let [units (take (+ 1 (rand-int 3)) (repeat (rand-nth units)))
                    units (map set-equipment units)
                    new-cost (reduce + (map :cost units))]
                (recur
                 (+ cost new-cost)
                 (concat result units)))))]
    (make-valid l)))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [i 0
         result []]
    (if (> i 10000)
      result
      (do
        (println "\n\nNew list:\n")
        (let [l (generate-list units)
              cost (list-cost l)
              result []]
          (doseq [unit l]
            (print-unit unit))
          (println "Cost:" cost)
          (if (> cost 1500)
            (recur i (conj result l))
            (recur (inc i) result)))))))

;;(print-units units)
