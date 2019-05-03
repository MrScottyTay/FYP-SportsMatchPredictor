(ns sportseer_client.dataset_creator
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

;; Converts a string into a more keywordable string (replaces " " with "-" etc.)
(defn get-keywordable-name [input]
  (clojure.string/lower-case (clojure.string/replace (clojure.string/replace input #"[^a-zA-Z\d\s]" "") " " "-")))

;; Converts a string date "29/01/2019" into a datetime object
(defn string->date-time [input]
  (let [[day month year] (map (fn [x] (Integer/parseInt (re-find #"\A-?\d+" x))) (clojure.string/split input #"/"))]
    (time/date-time year month day)))

;; Removes non player stat keywords (:player, :team, :result etc.)
(defn remove-non-stat-keys [input] (remove (fn [x] (.contains non-player-stats-columns x)) input))

(defn averages [data] (float ((partial (fn [x] (/ (reduce + x) (count x)))) data)))

;flattens the data structure after conj-ing
(defn flatten-conj [a b] (flatten (conj a b)))

;; _____________________________________________________________________________________________________________________
;; Ordering Data

(defn group-data-by-x [data group-value]
  (into {} (map (fn [x] (assoc {} (keyword (get-keywordable-name (first x))) (second x))) (group-by group-value data))))

(defn sort-by-date [data] (sort-by (fn [x] (string->date-time (:date x))) data))

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
         (numeric? column-data) (csv-str->type (assoc row column (read-string column-data)) (rest columns))
         (= column-data "") (csv-str->type (assoc row column 0) (rest columns))
         (= column-data "TRUE") (csv-str->type (assoc row column true) (rest columns))
         (= column-data "FALSE") (csv-str->type (assoc row column false) (rest columns))
         :else (csv-str->type row (rest columns)))))))

(defn csv-str->type-partitioned [csv-data] (flatten (map csv-str->type (partition 100 100 nil csv-data))))

(defn remove-all-star-matches [data]
  (let [team-grouped-data (group-data-by-x data :team)]
    (flatten (vals (select-keys team-grouped-data (remove (fn [x] (.contains [:0] x)) (keys team-grouped-data)))))))

(defn import-csv [file-name] (remove-all-star-matches (csv-str->type (read-csv file-name))))

;; _____________________________________________________________________________________________________________________
;; Combine .csv files

(defn multi-import-csv
  ([file-names] (multi-import-csv (first file-names) (rest file-names) []))
  ([current-file-name file-names data]
   (let [csv-data (import-csv current-file-name)]
     (if (empty? file-names)
       (flatten-conj csv-data data)
       (multi-import-csv (first file-names) (rest file-names) (flatten-conj csv-data data))))))


;; _____________________________________________________________________________________________________________________
;; Write .csv file

(def default-columns ^:const
  [:date :time :home-team :home-score :away-score :away-team
   :player :team :pts :ast :reb :min :fg :fga :3p :3pa :ft :fta :or :dr :pf :st :to :bs :result])

(def aggregated-dataset-columns ^:const
  [:date :time :home-team :home-score :away-score :away-team
   :pts :ast :reb :min :fg :fga :3p :3pa :ft :fta :or :dr :pf :st :to :bs :result])

(defn write-csv [path data columns]
  (let [headers (map name columns)
        rows (mapv #(mapv % columns) data)]
    (with-open [file (clojure.java.io/writer path)]
      (clojure.data.csv/write-csv file (cons headers rows)))))

;; _____________________________________________________________________________________________________________________
;; Match 0 - Total Averaged stats for previous season

(declare total-averages)

; Gets a players total averages for a total season
(defn dataset-total-averages
  ([data] (dataset-total-averages data (keys data)))
  ([data players]
   (if (empty? players) data
     (let [player (first players)]
       (dataset-total-averages (assoc data player (total-averages player (player data))) (rest players))))))

(defn total-averages
  ([player player-data] (let [columns (remove-non-stat-keys (keys (first player-data)))]
                          (total-averages player-data columns (assoc {} :player player))))
  ([player-data columns output-data]
   (if (empty? columns) output-data
     (let [column (first columns)]
       (total-averages player-data (rest columns) (assoc output-data column (averages (map column player-data))))))))

;; _____________________________________________________________________________________________________________________
;; Averaged prior stats for a player

(declare prior-averages)
(declare get-past-average)

; Prior Averages initiator, needs data to be grouped by player first
(defn dataset-prior-averages
  ([data] (dataset-prior-averages data (keys data) nil))
  ([data match-0] (dataset-prior-averages data (keys data) match-0))
  ([data players match-0]
   (if (empty? players) (flatten (vals data))
     (let [player (first players)]
       (dataset-prior-averages
         (assoc data player (prior-averages (player data) (player match-0))) (rest players) match-0)))))

; Gets the prior averages for each match for a single player, is used when iterating through the player grouped data
(declare prior-averages-)
(defn prior-averages
  ([player-data match-0] (prior-averages (first player-data) (rest player-data) match-0 [] []))
  ([current future match-0 past data]
   (cond
     (and match-0 (empty? past) (empty? future)) (conj data (prior-averages- current past))

     (and match-0 (empty? past))
     (prior-averages
       (first future) (rest future) match-0 (conj past current) (conj data (prior-averages- current match-0)))

     (empty? future)
     (conj data (prior-averages- current past))

     :else (prior-averages
             (first future) (rest future) match-0 (conj past current) (conj data (prior-averages- current past))))))
(defn prior-averages-
  ([current past]
   (prior-averages- current past (remove-non-stat-keys (keys current))))
  ([current past columns]
   (if (empty? columns) current
     (let [column (first columns)]
       (cond
         (empty? past) (prior-averages- (assoc current column 0) past (rest columns))
         (map? past) (prior-averages- (assoc current column (column past)) past (rest columns))
         :else (prior-averages- (assoc current column (averages (map column past))) past (rest columns)))))))

;; _____________________________________________________________________________________________________________________
;; Averaged prior stats for a player - revision 2 (remove first match)

(declare prior-averages-rf)

(defn dataset-prior-averages-rf
  ([data] (dataset-prior-averages-rf data (keys data)))
  ([data players]
   (if (empty? players) (flatten (vals data))
     (let [player (first players)
           player-data (player data)]
       (if (> (count player-data) 1)
         (dataset-prior-averages-rf (assoc data player (prior-averages-rf player-data)) (rest players))
         (dataset-prior-averages-rf data (rest players)))))))

(defn prior-averages-rf
  ([player-data] (prior-averages-rf (first player-data) (rest player-data) [] []))
  ([current future past data]
   (cond (empty? future) (conj data (prior-averages-rf current past))
         (empty? past) (prior-averages-rf (first future) (rest future) (conj past current) data)
         :else (prior-averages-rf
                 (first future) (rest future) (conj past current) (conj data (prior-averages-rf current past)))))
  ([current past] (prior-averages-rf current past (remove-non-stat-keys (keys current))))
  ([current past columns]
   (if (empty? columns) current
     (let [column (first columns)]
       (prior-averages-rf (assoc current column (averages (map column past))) past (rest columns))))))

(def average-algorithms ^:const [#_{:name "Total Averages" :function dataset-total-averages}
                                 {:name "Prior Averages" :function dataset-prior-averages-rf}])

(defn get-average-algorithm
  ([average-name] (get-average-algorithm average-name average-algorithms))
  ([average-name algorithm-list]
   (if (empty? algorithm-list) false
     (let [current-algorithm (first algorithm-list)]
       (if (= average-name (:name current-algorithm))
         (:function current-algorithm)
         (get-average-algorithm average-name (rest algorithm-list)))))))

;; _____________________________________________________________________________________________________________________
;; Aggregate players into teams

(def ^:const team-stat-keys [:date :time :home-team :away-team :home-score :away-score :team :result])
(def ^:const zero-high-keys
  (into [] (concat team-stat-keys [:pts :ast :reb :min :fg :fga :3p :3pa :ft :fta :or :dr :pf :st :to :bs])))

;; unbiased aggregation using averages across the team (no weighting)
(defn aggr-average
  ([data] (let [first-data (first data)]
            (aggr-average data (remove-non-stat-keys (keys first-data)) (select-keys first-data team-stat-keys))))
  ([data columns output]
   (if (empty? columns) output
     (let [column (first columns)]
       (aggr-average data (rest columns) (assoc output column (averages (map column data))))))))


(defn get-percentage [low high] (* (float (/ 100 high)) low))
(defn get-weight-value [player-data highest-minutes]
  (assoc player-data :weight-value (/ (get-percentage (:min player-data) highest-minutes) 100)))
(defn weighted-stat [player-data column] (* (column player-data) (:weight-value player-data)))
(defn weighted-average [data column]
  (let [weighted-stats (map (fn [x] (weighted-stat x column)) data)]
    (do (/ (reduce + weighted-stats) (count weighted-stats)))))

;; biased aggregation that takes into account the average minutes played per player
(defn aggr-minutes
  ([data]
   (let [ordered-data (reverse (sort-by :min data))
         first-data (first ordered-data)
         first-min (:min first-data)]
     (if (zero? first-min)
       (select-keys first-data zero-high-keys)
       (aggr-minutes (map (fn [x] (get-weight-value x first-min)) ordered-data)
                     (remove-non-stat-keys (keys first-data)) (select-keys first-data team-stat-keys)))))
  ([data columns output]
   (if (empty? columns) output
     (let [column (first columns)]
       (aggr-minutes data (rest columns) (assoc output column (weighted-average data column)))))))

(def aggregation-algorithms ^:const [{:name "Weighted by Minutes" :function aggr-minutes}
                                     {:name "Unbiased" :function aggr-average}])

(defn get-aggregation-algorithm
  ([aggr-name] (get-aggregation-algorithm aggr-name aggregation-algorithms))
  ([aggr-name algorithms-list]
   (if (empty? algorithms-list) false
     (let [current-algorithm (first algorithms-list)]
       (if (= aggr-name (:name current-algorithm))
         (:function current-algorithm)
         (get-aggregation-algorithm aggr-name (rest algorithms-list)))))))


(declare aggregate-players-)
(declare aggregate-players--)
(defn aggregate-players
  ([data] (aggregate-players data aggr-average)) ; if no aggregation function is supplied use un-biased averages
  ([data aggr-fn]
   (let [team-sorted-data (group-data-by-x data :team)]
     (aggregate-players team-sorted-data (keys team-sorted-data) aggr-fn)))
  ([data teams aggr-fn]
   (if (empty? teams) (into [] (flatten (map vals (vals data))))
     (let [current-team (first teams)
           date-sorted-data (group-data-by-x (current-team data) (fn [x] (str (:date x) (:time x))))]
       (aggregate-players
         (assoc data current-team (aggregate-players- date-sorted-data (keys date-sorted-data) aggr-fn))
         (rest teams) aggr-fn)))))

(defn aggregate-players- [data datetimes aggr-fn]
  (if (empty? datetimes) data
    (let [current-datetime (first datetimes)]
      (aggregate-players- (assoc data current-datetime (aggr-fn (current-datetime data))) (rest datetimes) aggr-fn))))

;; _____________________________________________________________________________________________________________________
;; Combine Match Data into a single row

(defn determine-home-away [teams]
  (let [first-team (first teams)]
    (if (= (:home-team first-team) (:team first-team))
      (assoc {} :home-team first-team :away-team (second teams))
      (assoc {} :home-team (second teams) :away-team first-team))))

(declare combine-match-data-)
(declare combine-match-data--)
(defn combine-match-data
  ([data] (let [date-sorted-data (group-data-by-x data :date)]
            (combine-match-data date-sorted-data (keys date-sorted-data))))
  ([date-sorted-data dates]
   (if (empty? dates) (flatten (map vals (vals date-sorted-data)))
     (let [current-date (first dates)]
       (combine-match-data (assoc date-sorted-data current-date (combine-match-data- (current-date date-sorted-data)))
                           (rest dates))))))

(defn combine-match-data-
  ([data] (let [home-grouped-data (group-data-by-x data :home-team)]
            (combine-match-data- home-grouped-data (keys home-grouped-data))))
  ([data home-teams]
   (if (empty? home-teams) data
     (let [current-home-team (first home-teams)
           current-home-team-data (current-home-team data)]
       (if (= (count current-home-team-data) 2) ;; check because for some reason an oppossing team was missing
         (combine-match-data- (assoc data current-home-team (combine-match-data-- current-home-team-data)) (rest home-teams))
         (combine-match-data- data (rest home-teams)))))))


(defn combine-match-data--
  ([data] (let [teams (determine-home-away data)
                home-team (:home-team teams)]
            (combine-match-data-- home-team (:away-team teams) (remove-non-stat-keys (keys home-team)))))
  ([home-team away-team columns]
   (if (empty? columns) home-team
     (let [column (first columns)]
       (combine-match-data-- (assoc home-team column (- (column home-team) (column away-team))) away-team (rest columns))))))