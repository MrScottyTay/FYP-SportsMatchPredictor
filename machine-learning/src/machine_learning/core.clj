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

;helper function from "https://stackoverflow.com/questions/3249334/test-whether-a-list-contains-a-specific-value-in-clojure"
;by j-g-faustus
(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

;; Removes non player stat keywords (:player, :team, :result etc.)
(defn remove-non-stat-keys [input]
  (remove (fn [x] (.contains non-player-stats-columns x)) input))

(defn get-percentage-of [high low]
  (* (/ high 100) low))

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

(defn get-training-testing-indices
  ([total testing-size]
   (let [testing-indices (get-training-testing-indices total testing-size [])]
     (vector (into [](remove (partial in? testing-indices) (range total))) testing-indices)))
  ([total training-size indices]
   (if (= (count indices) training-size) indices
     (let [number (rand-int total)]
       (if (in? indices number) (get-training-testing-indices total training-size indices)
         (get-training-testing-indices total training-size (conj indices number)))))))

;; gets the lines of data by the indices supplied
(defn get-data-by-indices
  ([data indices] (get-data-by-indices data indices []))
  ([data indices output]
   (if (empty? indices) output
     (get-data-by-indices data (rest indices) (conj output (nth data (first indices)))))))

;; splits dataset up into training and testing sets, percentage given is the size of the testing set
(defn split-dataset [data percentage]
   (let [dataset-size (count data)
         [training-indices testing-indices] (get-training-testing-indices
                                              dataset-size (get-percentage-of dataset-size percentage))]
     (assoc {} :training-set (get-data-by-indices data training-indices)
               :testing-set (get-data-by-indices data testing-indices))))

(def dataset (split-dataset (initialise-dataset csv-data) 20))

