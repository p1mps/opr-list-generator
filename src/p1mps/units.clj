(ns p1mps.units
  (:require [p1mps.opr-list-generator :as generator]
            [cheshire.core :as json]
            [clojure.set :as clojure.set]))

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


;; (defn choose-upgrades [unit]
;;   (let [options (->> (get (group-by :replaceWhat (:upgrades unit)) nil)
;;                      (mapcat :options))
;;         _ (println (->> (get (group-by :replaceWhat (:upgrades unit)) nil)
;;                         ))
;;         gains-with-cost (->> options
;;                              (mapcat (fn [o] (->> (:gains o)
;;                                                   (map (fn [g] (assoc g :cost (:cost o))))))))]
;;     gains-with-cost))

(defn find-eq [equipment name-eq]
  (let [e (first (filter #(or (= (:name %) name-eq)
                              (= (clojure.string/join "" (drop-last (:name %))) name-eq)) equipment))]
    (.indexOf equipment e)))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (if (not= pos -1)
    (into (subvec coll 0 pos) (subvec coll (inc pos)))
    coll))

(defn remove-equipment [equipment to-replace select]
  (loop [select select
         equipment (vec equipment)]
    (if (= select 0)
      equipment
      (recur (dec select)
             (loop [[eq-to-replace & rest-eq]  to-replace
                    equipment equipment]
               (if-not eq-to-replace
                 equipment
                 (recur
                  rest-eq
                  (vec-remove (find-eq equipment eq-to-replace) equipment))))))))

(defn print-equipment [unit]
  (let [eq (group-by :name (:equipment unit))]
    (doseq [[k v] eq]
      (println "Equipment " k (count v)))
    (println "Chosen upgrades:")
    (doseq [u (:chosen-upgrade unit)]
      (println (:name u)))))


(defn label->n [label & unit-size]
  (try
    (condp = label
      "one" 1
      "two" 2
      "any" (+ 1 (rand-int unit-size)))
    (catch Exception _
      1)))


(defn find-select [label unit-size]
  (let [label-split (clojure.string/split label  #" ")]
    (cond
      (= (get label-split 0) "Replace")
      (let [how-many (get label-split 1)]
        (if (not= how-many "up")
          (label->n how-many unit-size)
          (+ 1 (rand-int (label->n (get label-split 3))))))
      (= (get label-split 0) "Upgrade")
      (let [how-many (get label-split 2)]
        (label->n how-many unit-size)))))


(defn choose-upgrades [unit upgrades]
  (reduce (fn [result [equipments-to-replace with-what]]
            (if equipments-to-replace
              (let [with-what (rand-nth with-what)
                    select (find-select (:label with-what) (:size unit))
                               ;;(reduce + (map #(get % :select (get % :affects)) with-what))
                    new-equipment (remove-equipment result equipments-to-replace select)
                    upgrades (->> (:options with-what)
                                  (rand-nth)
                                  :gains)
                    upgrades-content (mapcat :content upgrades)]
                (concat new-equipment
                        (flatten (concat (repeat select upgrades)
                                         (repeat select upgrades-content)))))
              result))
          (:equipment unit)
          upgrades))


(defn take-random [coll]
  (reduce (fn [result e]
            (if (not= (rand-int 2) 0)
              (conj result e)
              result))
          []
          coll)

  )



(defn set-equipment [unit]
  (-> (assoc unit
             :equipment
             (choose-upgrades unit (group-by :replaceWhat (random-sample 0.5 (:upgrades unit))))
             :chosen-upgrade
             (->> (take-random (->> (mapcat :options (get (group-by :replaceWhat (:upgrades unit)) nil))
                                    (mapcat :gains)))))))








(def unit
  {:name "unit"
   :equipment [{:name "A"}
               {:name "A"}
               {:name "B"}
               {:name "B"}]
   :upgrades [{:options
               [{:gains
                 [{:name "Pistol",
                   :type "ArmyBookWeapon"}
                  {:name "CCW",
                   :count 1}],
                 :cost 0}],
               :select 2
               :replaceWhat ["A"]}]})


(def infantry-squad2
  (assoc infantry-squad
         :equipment
         [{:name "CCWs"}
          {:name "Rifles"}
          {:name "CCWs"}
          {:name "Rifles"}]
         :upgrades [{:label "Replace one Rifle and CCW"
                     :options
                     [{:gains
                       [{:name "Pistol",
                         :type "ArmyBookWeapon"}
                        {:name "CCW",
                         :type "ArmyBookWeapon"
                         :count 1}],
                       :cost 0}],
                     :select 2
                     :replaceWhat ["Rifles" "CCWs"]}]))


(do
  (println "======================")
  (print-equipment
   infantry-squad2)
  (println "======================")
  (print-equipment
   (-> (set-equipment infantry-squad2)
       (set-equipment))))


(do
  (println "======================")
  (print-equipment
   infantry-squad)
  (println "======================")
  (print-equipment
   (set-equipment infantry-squad)))


(do
  (println "======================")
  (print-equipment
   tank)
  (println "======================")
  (print-equipment
   (set-equipment tank)))


(get (group-by :replaceWhat (:upgrades tank)) nil)
