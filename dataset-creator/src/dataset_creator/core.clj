(ns dataset-creator.core
  (:require [clojure.data.csv]
            [clj-time.core :as time]))

(def ^:const input-data-path "")
(def ^:const non-player-stats-columns [:date :time :home-team :away-team :home-score :away-score :player :team :result])

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
  (clojure.string/lower-case (clojure.string/replace (clojure.string/replace (str input) #"[^a-zA-Z\d\s]" "") " " "-")))

;; Converts a string date "29/01/2019" into a datetime object
(defn string->date-time [input]
  (let [[day month year] (map (fn [x] (Integer/parseInt (re-find #"\A-?\d+" x))) (clojure.string/split input #"/"))]
    (time/date-time year month day)))

(defn column-average [data column]
  (float ((partial (fn [x] (/ (reduce + x) (count x))) (map column data)))))

;; _____________________________________________________________________________________________________________________
;; Read .csv file

(defn read-csv [file-name]
  (let [[head & lines](with-open [reader (clojure.java.io/reader (str input-data-path file-name))]
                        (doall (clojure.data.csv/read-csv reader)))]
    (map (fn [x] (zipmap (map keyword head) x)) lines)))

(declare csv-str->type-partitioned)

(defn csv-str->type
  ([csv-data]
   (if (< 100 (count csv-data))
     (csv-str->type-partitioned csv-data)
     (csv-str->type csv-data (keys (first csv-data)) [])))
  ([rows-waiting columns rows-processed]
   (if (empty? rows-waiting) rows-processed
     (csv-str->type (rest rows-waiting) columns (conj rows-processed (csv-str->type (first rows-waiting) columns)))))
  ([row columns]
   (if (empty? columns) row
     (let [column (first columns)
           column-data (column row)]
       (cond
         (numeric? column-data)
         (csv-str->type (assoc row column (parse-int column-data)) (rest columns))

         (= column-data "TRUE")
         (csv-str->type (assoc row column true) (rest columns))

         (= column-data "FALSE")
         (csv-str->type (assoc row column false) (rest columns))

         :else
         (csv-str->type row (rest columns)))))))

(defn csv-str->type-partitioned [csv-data]
  (flatten (map csv-str->type (partition 100 100 nil csv-data))))

(def csv-data (csv-str->type (read-csv "nba-17-18-stats.csv")))

(def data-keys (keys (first csv-data)))

;; _____________________________________________________________________________________________________________________
;; Write .csv file

(def default-columns ^:const
  [:date :time :home-team :home-score :away-score :away-team
   :player :team :pts :ast :reb :min :fg :fga :3p :3pa :ft :fta :or :dr :pf :st :to :bs :result])

(defn write-csv [path data columns]
  (let [headers (map name columns)
        rows (mapv #(mapv % columns) data)]
    (with-open [file (clojure.java.io/writer path)]
      (clojure.data.csv/write-csv file (cons headers rows)))))

;; _____________________________________________________________________________________________________________________
;; Ordering Data

(defn group-data-by-x [data column]
  (into {} (map (fn [x] (assoc {} (keyword (get-keywordable-name (first x))) (second x))) (group-by column data))))

(def player-grouped-data (group-data-by-x csv-data :player))

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
;; Match 0 - Total Averaged stats for previous season

; Gets a players total averages for a total season
(defn player-total-averages
  ([input] ; Input data should already be grouped by players
   (let [players (keys input)
         first-player (first players)]
     (player-total-averages first-player (rest players) (keys first-player) input)))
  ([current-player rest-players columns data]; Iterates through the players
   (let [player-data (current-player data)
         output-data (assoc {} :player (:player (first current-player)))]
     (if (empty? rest-players)
       (assoc data current-player (player-total-averages player-data columns output-data))
       (player-total-averages (first rest-players) (rest rest-players) columns
                              (assoc data current-player (player-total-averages player-data columns output-data))))))
  ([player-data columns output-data] ; Calculates the total averages for a player
   (if (empty? columns) output-data
     (player-total-averages
       player-data (rest columns)
       (float ((partial (fn [x] (/ (reduce + x) (count x)))) (map (first columns) player-data)))))))

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
;; Dataset Modifier 01 - Weighted by minutes played


;; _____________________________________________________________________________________________________________________
;; Aggregate teams

(defn non-bias-average-aggregation
  ([data]
   (let [columns (keys (first data))]
         (non-bias-average-aggregation data (first columns) (rest columns) {})))
  ([data current-column columns-left output]
   (let [output (conj output (column-average data current-column))]
     (if (empty? columns-left) output
       (non-bias-average-aggregation data (first columns-left) (rest columns-left) output)))))

(declare aggregate-teams-)
(def aggr-function-default non-bias-average-aggregation)

(defn aggregate-teams
  ([data]
   (aggregate-teams data aggr-function-default))
  ([data aggr-function]
    (let [teams (keys data)]
      (aggregate-teams (first teams) (rest teams) data aggr-function)))
  ([current-team teams data aggr-function]
    (let [team-data (group-data-by-x (current-team data) :date)
          data (assoc data current-team (aggregate-teams- team-data aggr-function))]
      (if (empty? teams) data
        (aggregate-teams (first teams) (rest teams) data aggr-function)))))

(defn aggregate-teams-
  ([team-data aggr-function] (aggregate-teams- (first team-data) (rest team-data) () aggr-function))
  ([current-match matches data aggr-function]
   (let [data (conj data (aggr-function (second current-match)))]
     (if (empty? matches) data
       (aggregate-teams- (first matches) (rest matches) data aggr-function)))))

(def aggregate-teams-data (aggregate-teams (group-data-by-x prior-averaged-data :team)))


;; _____________________________________________________________________________________________________________________
;; Dataset to csv-able data - basic, still separated by players

(defn create-dataset-player-csv-format [player-grouped-data]
  (sort-by (juxt (fn [x] (string->date-time (:date x))) :min) (flatten (map (fn [x] (second x)) player-grouped-data))))

(def prior-averaged-csv-data (create-dataset-player-csv-format prior-averaged-data))