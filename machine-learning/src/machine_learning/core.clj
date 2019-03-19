(ns machine-learning.core
  (:require [clojure.data.csv]))

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

;; Removes non player stat keywords (:player, :team, :result etc.)
(defn remove-non-stat-keys [input]
  (remove (fn [x] (.contains non-player-stats-columns x)) input))

;; _____________________________________________________________________________________________________________________
;; Read CSV Files

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
                            (csv-str->type (assoc row column (read-string column-data)) (rest columns))

                            (= column-data "")
                            (csv-str->type (assoc row column 0) (rest columns))

                            (= column-data "true")
                            (csv-str->type (assoc row column 1) (rest columns))

                            (= column-data "false")
                            (csv-str->type (assoc row column 0) (rest columns))

                            :else
                            (csv-str->type row (rest columns)))))))

(defn csv-str->type-partitioned [csv-data]
  (flatten (map csv-str->type (partition 100 100 nil csv-data))))

(def csv-data (csv-str->type (read-csv "nba-17-18_prior-averages-m0_mins-aggr.csv")))

;; _____________________________________________________________________________________________________________________
;; Dataset Initialiser

(defn initialise-dataset
  ([data] (initialise-dataset data []))
  ([data output]
   (if (empty? data) output
     (let [current-data (first data)
           stats (into [] (vals (select-keys current-data (remove-non-stat-keys (keys current-data)))))
           label (vector (:result current-data))]
       (initialise-dataset (rest data) (conj output (assoc {} :data stats :label label))) ))))

(def dataset (initialise-dataset csv-data))