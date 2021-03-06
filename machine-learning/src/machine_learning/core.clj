(ns machine-learning.core
  (:require [clojure.data.csv]
            [cortex.experiment.train :as train]
            [cortex.experiment.util :as experiment.util]
            [cortex.nn.execute :as execute]
            [cortex.nn.layers :as layers]
            [cortex.nn.network :as network]
            [cortex.util :as util]
            [cortex.optimize.adam :as adam]
            [clojure.string :as string]))

(def ^:const input-data-path "")
(def ^:const non-player-stats-columns [:date :time :home-team :away-team :home-score :away-score :player :team :result])
(def ^:const default-data-columns ["pts" "ast" "reb" "min" "fg" "fga" "3p" "3pa" "ft" "fta" "or" "dr" "pf" "st" "to"])
(def ^:const default-label-column "result")
(def ^:const default-network-params
  {:optimizer   (adam/adam)
   :batch-size  100
   :epoch-count 10
   :epoch-size  200000})
(def ^:const default-network-description
  [(layers/input 16 1 1 :id :data)
   (layers/linear->relu 32)
   (layers/dropout 0.9)
   (layers/linear->relu 20)
   (layers/linear 2)
   (layers/softmax :id :label)])

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

(defn contains-keys? [coll keys]
  (cond (empty? keys) true
        (contains? coll (first keys)) (contains-keys? coll (rest keys))
        :else false))

;; _____________________________________________________________________________________________________________________
;; Read CSV Files

(defn date-or-time? [input]
  (or (string/includes? input "/") (string/includes? input ":")))

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
         (and (not (date-or-time? column-data)) (number? (read-string column-data)))
         (csv-str->type (assoc row column (read-string column-data)) (rest columns))

         (= column-data "") (csv-str->type (assoc row column 0) (rest columns))
         (= column-data "true") (csv-str->type (assoc row column 1) (rest columns))
         (= column-data "false") (csv-str->type (assoc row column 0) (rest columns))
         :else (csv-str->type row (rest columns)))))))

(defn csv-str->type-partitioned [csv-data]
  (flatten (map csv-str->type (partition 100 100 nil csv-data))))

(defn import-csv [csv-file-name] (csv-str->type (read-csv csv-file-name)))

#_(def csv-data (csv-str->type (read-csv "nba-13-18_prior-averages_mins-aggr.csv")))
#_(def csv-data2 (csv-str->type (read-csv "nba-17-18_prior-averages-m0_unbiased-aggr.csv")))

;; _____________________________________________________________________________________________________________________
;; Dataset Initialiser

(defn initialise-dataset
  ([data data-columns label-column]
   (initialise-dataset data (map #(keyword %) data-columns) (keyword label-column ) []))
  ([data data-columns label-column output]
   (if (empty? data) output
     (let [current-data (first data)
           stats (into [] (vals (select-keys current-data data-columns)))
           label (util/idx->one-hot (label-column current-data) 2)]
       (initialise-dataset (rest data) data-columns label-column (conj output (assoc {} :data stats :label label)))))))

;; -------------------------
;; Split Dataset (Random)

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
(defn split-dataset-random [data percentage]
   (let [dataset-size (count data)
         [training-indices
          testing-indices] (get-training-testing-indices dataset-size (get-percentage-of dataset-size percentage))]
     (assoc {} :training-set (get-data-by-indices data training-indices)
               :testing-set (get-data-by-indices data testing-indices))))

;; -------------------------
;; Split Dataset (Sectioned)

(defn split-dataset-sectioned [data percentage]
  (let [section-size (+ (int (/ (count data) 10)) 1)
        sections (partition section-size section-size nil data)
        testing-take-count (get-percentage-of section-size percentage)]
    (assoc {} :training-set (flatten (map #(take (dec (- (count %) testing-take-count)) %) sections))
              :testing-set (flatten (map #(take-last testing-take-count %) sections)))))

;; -------------------------

(defn split-csv
  ([data parts-count]
   (let [data-size (count data)
         partition-size (+ (int (/ data-size parts-count)) 1)] ; 1 is added because int always rounds down on fractions
     (partition partition-size partition-size nil data))))

(defn partitioned-initialise-dataset
  ([csv-parts data-columns label-column]
   (flatten (partitioned-initialise-dataset csv-parts data-columns label-column [])))
  ([csv-parts data-columns label-column output]
   (if (empty? csv-parts) output
     (partitioned-initialise-dataset (rest csv-parts) data-columns label-column
                                     (conj output (initialise-dataset (first csv-parts) data-columns label-column))))))

(defn create-dataset
  ([csv-data data-columns label-column training-percentage]
   (create-dataset csv-data data-columns label-column training-percentage split-dataset-sectioned))
  ([csv-data data-columns label-column training-percentage split-algorithm]
   (split-algorithm (partitioned-initialise-dataset (split-csv csv-data 10) data-columns label-column)
                    training-percentage)))

#_(def dataset (create-dataset csv-data 20))

;; _____________________________________________________________________________________________________________________
;; Neural Network Declaration and Training

(defn insert-default-params
  ([params]
   (if (or (empty? params) (nil? params))
     default-network-params
     (insert-default-params params (keys default-network-params))))
  ([params param-keys]
   (if (empty? param-keys)
     params
     (let [current-key (first param-keys)]
       (if (nil? (current-key params))
         (insert-default-params (assoc params current-key (current-key default-network-params)) (rest param-keys))
         (insert-default-params params (rest param-keys)))))))

(defn train [dataset description params]
  (let [network (network/linear-network description)
        train-original (:training-set dataset)
        test-dataset (:testing-set dataset)
        train-dataset (experiment.util/infinite-class-balanced-dataset
                        train-original :class-key :label :epoch-size (:epoch-size params))]
    (train/train-n network train-dataset test-dataset
                   :batch-size (:batch-size params)
                   :epoch-count (:epoch-count params)
                   :optimizer (:optimizer params))))

(defn create-model [model-name csv-file-name data-columns label-column training-percentage description params]
  (let [params (insert-default-params params)
        csv-data (import-csv csv-file-name)
        data-columns (if (nil? data-columns) default-data-columns data-columns)
        label-column (if (nil? label-column) default-label-column label-column)
        training-percentage (if (nil? training-percentage) 20 training-percentage)
        dataset (create-dataset csv-data data-columns label-column training-percentage)
        model (train dataset description params)]
    (assoc {} :name model-name :data-columns data-columns :label-column label-column :model model)))

(defn best-result [[false-score true-score]]
  (> true-score false-score))

(defn make-prediction [model stats]
  (best-result (execute/run model (vector stats))))

(defn create-predictions [csv-file-name model]
  (let [csv-data (import-csv csv-file-name)
        data-columns (:data-columns model)]
    (if (not (contains-keys? (first csv-data) data-columns)) false
      (map #(let [stats (select-keys % data-columns)]
              (assoc {} :data % :stats stats :prediction (make-prediction (:model model) stats))) csv-data))))

#_(def model (train dataset network-description network-params))

(defn test-model
  ([model testing-dataset] (test-model (execute/run model testing-dataset) testing-dataset 0 []))
  ([results testing-dataset current-index test-results]
   (if (= current-index (count testing-dataset)) test-results
     (let [current-result (:label (nth results current-index))
           current-result-best (best-result current-result)
           current-testing (best-result (:label (nth results current-index)))]
       (test-model results testing-dataset (inc current-index)
         (conj test-results (assoc {} :prediction (assoc {} :scores current-result :best current-result-best)
                                      :actual current-testing :correct (= current-result-best current-testing))))))))