(ns rcv
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(defn read-data [file]
  (let [lines (-> file
                  slurp
                  (s/split #"\r?\n"))]
    (map #(s/split % #",") lines)))

(defn to-drop [totals]
  (let [min-votes (apply min (map second totals))]
    (->> totals
         (filter #(= (second %) min-votes))
         (map first)
         (into #{}))))

(defn drop-candidates [votes to-drop]
  (->> votes
       (map #(remove to-drop %))
       (remove empty?)))

(defn derived-data [{:keys [votes] :as data}]
  (let [remaining-candidates (into #{} (apply concat votes))
        totals (merge (zipmap remaining-candidates (repeat 0)) (frequencies (map first votes)))]
    (assoc data
      :remaining-candidates remaining-candidates
      :totals totals
      :to-drop (to-drop totals))))

(defn next-round [{:keys [votes round to-drop]}]
  (let [new-votes (drop-candidates votes to-drop)]
    (derived-data {:votes new-votes :round (inc round)})))

(defn threshold-met? [{:keys [votes totals]}]
  (<= (inc (/ (count votes) 2)) (apply max (vals totals))))

(defn all-rounds [votes]
  (let [data (derived-data {:votes votes :round 1})
        [head [result]] (split-with (complement threshold-met?)
                                (iterate next-round data))]
    (conj (vec head) result)))

(defn main []
  (let [votes (read-data (first *command-line-args*))]
    (pprint (map #(dissoc % :votes) (all-rounds votes)))))

(main)
