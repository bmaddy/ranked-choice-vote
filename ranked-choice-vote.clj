(ns rcv
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(defn read-data [filename]
  "Reads a csv file into a vector of vectors."
  (let [lines (-> filename
                  slurp
                  (s/split #"\r?\n"))]
    (map #(s/split % #",") lines)))

(defn to-drop [totals]
  "Given a map of the number of votes for each candidate, it returns the candidates with the lowest number of votes."
  (let [min-votes (apply min (map second totals))]
    (->> totals
         (filter #(= (second %) min-votes))
         (map first)
         (into #{}))))

(defn drop-candidates [votes to-drop]
  "Removes all votes (in any rank) of candidates in 'to-drop'"
  (->> votes
       (map #(remove to-drop %))
       (remove empty?)))

(defn derived-data [{:keys [votes] :as data}]
  "Calculate derived data from the votes and merge them into the map."
  (let [remaining-candidates (into #{} (apply concat votes))
        totals (merge (zipmap remaining-candidates (repeat 0)) (frequencies (map first votes)))]
    (assoc data
      :remaining-candidates remaining-candidates
      :totals totals
      :to-drop (to-drop totals))))

(defn next-round [{:keys [votes round to-drop]}]
  "Given a map representing a round of counting, return the next round's map."
  (let [new-votes (drop-candidates votes to-drop)]
    (derived-data {:votes new-votes :round (inc round)})))

(defn threshold-met? [{:keys [votes totals]}]
  "Returns true if one of the candidates has at least 50% + 1 of the votes."
  (<= (inc (/ (count votes) 2)) (apply max (vals totals))))

(defn all-rounds [votes]
  "Given an initial vector of votes, returns all rounds in order."
  (let [data (derived-data {:votes votes :round 1})
        [head [result]] (split-with (complement threshold-met?)
                                (iterate next-round data))]
    (conj (vec head) result)))

(defn main []
  (let [votes (read-data (first *command-line-args*))]
    (pprint (map #(dissoc % :votes) (all-rounds votes)))))

(main)
