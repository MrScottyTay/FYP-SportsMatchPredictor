(ns dataset-creator.core
  (:require [clojure.data.csv]
            [clj-time.core :as time]))

(def ^:const input-data-path "")
(def ^:const non-player-stats-columns [:date :time :home-team :away-team :home-score :away-score :player :team])

;; _____________________________________________________________________________________________________________________
;; Helpers

;helper function from "https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Clojure"
(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

;; Converts a string into a more keywordable string (replaces " " with "-" etc.)
(defn get-keywordable-name [input]
  (clojure.string/lower-case (clojure.string/replace (clojure.string/replace input #"[^a-zA-Z\d\s]" "") " " "-")))

;; Converts a string date "29/01/2019" into a datetime object
(defn string->date-time [input]
  (let [[day month year] (map (fn [x] (Integer/parseInt (re-find #"\A-?\d+" x))) (clojure.string/split input #"/"))]
    (time/date-time year month day)))

;; _____________________________________________________________________________________________________________________
;; Read .csv file

(defn read-csv [file-name]
  (let [[head & lines](with-open [reader (clojure.java.io/reader (str input-data-path file-name))]
                        (doall (clojure.data.csv/read-csv reader)))]
    (map (fn [x] (zipmap (map keyword head) x)) lines)))

(declare csv-str->num-partitioned)

(defn csv-str->num
  ([csv-data]
   (if (< 100 (count csv-data))
     (csv-str->num-partitioned csv-data)
     (csv-str->num csv-data (keys (first csv-data)) [])))
  ([rows-waiting columns rows-processed]
   (if (empty? rows-waiting) rows-processed
     (csv-str->num (rest rows-waiting) columns (conj rows-processed (csv-str->num (first rows-waiting) columns)))))
  ([row columns]
   (if (empty? columns) row
     (let [column (first columns)
           column-data (column row)]
       (if (numeric? column-data)
         (csv-str->num (assoc row column (parse-int column-data)) (rest columns))
         (csv-str->num row (rest columns)))))))

(defn csv-str->num-partitioned [csv-data]
  (flatten (map csv-str->num (partition 100 100 nil csv-data))))

(def csv-data (csv-str->num (read-csv "nba-17-18-stats.csv")))

(def data-keys (keys (first csv-data)))

;; _____________________________________________________________________________________________________________________
;; Ordering Data

(defn group-data-by-player [data]
  (into {} (map (fn [x] (assoc {} (keyword (get-keywordable-name (first x))) (second x))) (group-by :player data))))

(def player-grouped-data (group-data-by-player csv-data))

(defn sort-by-date [data]
  (sort-by (fn [x] (string->date-time (:date x))) data))
(defn sort-all-players-by-date
  ([data]
   (sort-all-players-by-date (keys data)))
  ([data players]
   (if (empty? players) data
     (let [player (first players)]
       (sort-all-players-by-date (assoc data player (sort-by-date (player data))) (rest players))))))

;; _____________________________________________________________________________________________________________________
;; Dataset 01 - Averaged prior stats

(declare prior-averages)
(declare get-past-average)

; Prior Averages initiator, needs data to be grouped by player first
(defn dataset-prior-averages
  ([data]
   (dataset-prior-averages data (keys data)))
  ([data players]
   (if (empty? players) data
     (let [player (first players)]
       (dataset-prior-averages (assoc data player (prior-averages (player data))) (rest players))))))

; Gets the prior averages for each match for a single player, is used when iterating through the player grouped data
(defn prior-averages
  ([input]
   (prior-averages (first input) (rest input) [] []))
  ([current future past data]
   (if (empty? future) (conj data (prior-averages current past)) ; return data after final average calculation
     ; iterate through the "future" add the current to the past and get the prior averages for the current match
     (prior-averages (first future) (rest future) (conj past current) (conj data (prior-averages current past)))))
  ([current past]
   (prior-averages current past (remove (fn [x] (.contains non-player-stats-columns x)) (keys current))))
  ([current past columns]
   (if (empty? columns) current
     (let [column (first columns)]
       (if (empty? past) (prior-averages (assoc current column 0) past (rest columns))
                         (prior-averages (assoc current column (get-past-average past column)) past (rest columns)))))))

; calculates the averages of one stat for all previous matches
(defn get-past-average [past column]
  (float ((partial (fn [x] (/ (reduce + x) (count x)))) (map column past))))

(def prior-averaged-data (dataset-prior-averages player-grouped-data))

;; _____________________________________________________________________________________________________________________
;; 