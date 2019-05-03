(ns sportseer_client.scraper
  (:require [net.cgrand.enlive-html :as html]
            [etaoin.api :as web]
            [clojure.data.csv :as csv]))

(def match-listing-ids ^:const (assoc {} :loading-overlay "fs_overlay" :more-results "tournament-page-results-more"))

(def nba-match-urls ^:const [{:name "NBA Season '13-'14" :url "https://www.scoreboard.com/uk/nba-2013-2014/results/"}
                             {:name "NBA Season '14-'15" :url "https://www.scoreboard.com/uk/nba-2014-2015/results/"}
                             {:name "NBA Season '15-'16" :url "https://www.scoreboard.com/uk/nba-2015-2016/results/"}
                             {:name "NBA Season '16-'17" :url "https://www.scoreboard.com/uk/nba-2016-2017/results/"}
                             {:name "NBA Season '17-'18" :url "https://www.scoreboard.com/uk/nba-2017-2018/results/"}])

(defn get-nba-match-url
  ([name] (get-nba-match-url name nba-match-urls))
  ([name url-list]
   (if (empty? url-list) false
     (let [first-url (first url-list)]
       (if (= name (:name first-url)) (:url first-url)
         (get-nba-match-url name (rest url-list)))))))

(def nba-match-prefix-url "https://www.scoreboard.com/uk/match/")
(def nba-match-player-stats-suffix-url "/#player-statistics;0")
(def match-stats-ids ^:const (assoc {} :loading-overlay "preload-all"
                                       :player-table "tab-player-statistics-0-statistic"))

(def team-abbrevs ^:const
  (assoc {} :atl "Atlanta Hawks"         :bro "Brooklyn Nets"          :bos "Boston Celtics"
            :cha "Charlotte Hornets"     :chi "Chicago Bulls"          :cle "Cleveland Cavaliers"
            :dal "Dallas Mavericks"      :den "Denver Nuggets"         :det "Detroit Pistons"
            :gsw "Golden State Warriors" :hou "Houston Rockets"        :ind "Indiana Pacers"
            :lac "Los Angeles Clippers"  :lal "Los Angeles Lakers"     :mem "Memphis Grizzlies"
            :mia "Miami Heat"            :mil "Milwaukee Bucks"        :min "Minnesota Timberwolves"
            :nop "New Orleans Pelicans"  :nyk "New York Knicks"        :okl "Oklahoma City Thunder"
            :okc "Oklahoma City Thunder" :orl "Orlando Magic"          :phi "Philadelpia 76ers"
            :pho "Phoenix Suns"          :por "Portland Trail Blazers" :sac "Sacramento Kings"
            :sas "San Antonio Spurs"     :tor "Toronto Raptors"        :uta "Utah Jazz"
            :was "Washington Wizards"))

;; _____________________________________________________________________________________________________________________
;; Helper functions

;helper function from "https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Clojure"
(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))

(defn parse-int [s] (Integer. (re-find  #"\d+" s)))

(defn parse-minsec [minsec]
  (let [split-minsec (clojure.string/split minsec #":")]
    (float (+ (parse-int (first split-minsec)) (/ (parse-int(second split-minsec)) 60)))))

(defn reformat-stat [stat]
  (cond
    (and (string? stat) (clojure.string/includes? stat ":")) (parse-minsec stat) ; time -reformat minutes to decimal
    (and (string? stat) (= stat "-")) 0 ; turn "-" to 0
    (and (string? stat) (numeric? stat)) (parse-int stat) ; turn string to number
    :else stat)) ; no need to change stat

(defn keywordable-name [input]
  (clojure.string/lower-case
    (clojure.string/replace (clojure.string/replace input #"[^a-zA-Z\d\s]" "") " " "-")))

(defn player-team-parse [player-team-abbrev] ((keyword (keywordable-name player-team-abbrev)) team-abbrevs))

(defn get-result [player-team home-team home-score away-score]
  (let [home-win? (> home-score away-score)
        player-home? (= player-team home-team)]
    (cond
      (and home-win? player-home?) true
      (and home-win? (not player-home?)) false
      (and (not home-win?) player-home?) false
      (and (not home-win?) (not player-home?)) true)))

;; _____________________________________________________________________________________________________________________
;; Collecting URLs for match data

(declare load-all-match-listings-)
(defn load-all-match-listings [url ids]
  (let [driver (web/chrome)] ; set up driver
    (do (web/go driver url) ; go to specified web-page
        (let [source (load-all-match-listings- driver ids)] ; recursively click to get all of the data
          (do (web/close-window driver) (web/stop-driver driver) source))))) ; close driver before returning html
(defn load-all-match-listings- [driver ids]
  (let [more-results-id (:more-results ids)]
    (do
      ; wait for data to load before continuing
      (web/wait-invisible driver {:id (:loading-overlay ids)}) (web/scroll-bottom driver)
      ; if show more button still exists, then click it and recur, if not, then return the pages html
      (if (and (web/exists? driver {:id more-results-id}) (web/visible? driver {:id more-results-id}))
        (do (web/click-visible driver {:id more-results-id}) (load-all-match-listings- driver ids)) ; click and recur
        (web/get-source driver))))) ; get html


(defn get-match-ids [html-data]
  (map (comp last (fn [x] (clojure.string/split x #"_")))
       (filter identity (map (comp :id :attrs) (html/select (html/html-snippet html-data) [:tr])))))
(defn build-match-urls [prefix-url suffix-url match-ids] (map (fn [x] (str prefix-url x suffix-url)) match-ids))

;; _____________________________________________________________________________________________________________________
;; Collecting match stats html

(defn get-match-stats-html
  ([match-url ids] ; init without a driver, create a new one each time
   (get-match-stats-html match-url ids (web/chrome) false))
  ([match-url ids driver] ; init with a driver, recycle the same one
   (get-match-stats-html match-url ids driver true))
  ([match-url ids driver recycle-driver?]
   (let [player-table-id (:player-table ids)]
     (do (web/go driver match-url)
         ; wait for data to load before continuing
         (web/wait-invisible driver {:id (:loading-overlay ids)})
         ; if player stats table exists, then return html after closing window, otherwise return nil
         (if (and (web/exists? driver {:id player-table-id}) (web/visible? driver {:id player-table-id}))
           (let [source (web/get-source driver)] ; getting html data
             ; close and stop driver if needed
             (do (if (not recycle-driver?) (do (web/close-window driver) (web/stop-driver driver)) ())
                 source)) ; return html
           nil))))) ; return nil

;; _____________________________________________________________________________________________________________________
;; Parsing match stats stats table

(declare parse-stats-table)
(declare expand-player-stats)

(defn get-match-stats [match-stats-data]
  (let [html-snippet (html/html-snippet match-stats-data)
        stats-table (second (html/select (html/select html-snippet [:div#tab-player-statistics-0-statistic]) [:table]))
        player-stats (expand-player-stats (parse-stats-table stats-table))
        date-time (clojure.string/split (first (:content (last (html/select html-snippet [:td#utime])))) #" ")
        date (clojure.string/replace (first date-time) #"[.]" "/")
        time (second date-time)
        score (map (comp first :content) (html/select html-snippet [:span.scoreboard]))
        home-score (reformat-stat (first score))
        away-score (reformat-stat (second score))
        teams (flatten (map (comp #(map (comp first :content) %) :content) (html/select html-snippet [:span.tname])))
        home-team (second teams)
        away-team (nth teams 2)]
    (map (fn [x] (assoc x :date date :time time :home-score home-score :away-score away-score
                          :home-team home-team :away-team away-team
                          :result (get-result (:team x) home-team home-score away-score)))
         player-stats)))

;; _____________________________________________________________________________________________________________________
;;  Parsing player stats table

(declare parse-heads)
(declare parse-rows)
(defn parse-stats-table
  ([stats-table-raw] (parse-stats-table (parse-heads stats-table-raw) (parse-rows stats-table-raw))) ; get the text content from the table
  ([heads rows] (map (fn [x] (parse-stats-table heads x {})) rows)) ; map the iterative function to create a map from the data
  ([heads row results] ; iteratively creates a map with the heads as the keys for the data
   (cond
     (empty? row) results ;base case

     (= (first heads) "+/-") ; remove +/- column, shouldn't be needed and its keyword equivalent messes up the data
     (parse-stats-table (rest heads) (rest row) results)

     :else ; iterate whilst adding the current column to the map
     (parse-stats-table (rest heads) (rest row) (assoc results (keyword (keywordable-name (first heads))) (first row))))))

;; Gets the text content for the heads of the table
(defn parse-heads [raw-table] (map (comp first :content)
                                   (html/select (html/select raw-table [:th]) [:span.txt])))
;; Gets the text content for the rows of the table
(defn parse-rows [raw-table] (map (comp (fn [x] (map (comp last :content) x)) :content)
                                  (html/select (html/select raw-table [:tbody]) [:tr])))

;; _____________________________________________________________________________________________________________________
;; Expanding player stats columns

(declare expand-player-stats-row)

(defn expand-player-stats [stats] (map expand-player-stats-row stats))
(defn expand-player-stats-row [stats-row]
  (let [fgs (clojure.string/split (:fg stats-row) #"-|_")
        threeps (clojure.string/split (:3p stats-row) #"-|_")
        fts (clojure.string/split (:ft stats-row) #"-|_")]
    (assoc {} :pf (reformat-stat (:pf stats-row))    :fg (reformat-stat (first fgs))
              :fga (reformat-stat (second fgs))      :3p (reformat-stat (first threeps))
              :3pa (reformat-stat (second threeps))  :ft (reformat-stat (first fts))
              :fta (reformat-stat (second fts))      :team (player-team-parse (:team stats-row))
              :player (:player stats-row)            :min (reformat-stat (:min stats-row))
              :reb (reformat-stat (:reb stats-row))  :or (reformat-stat (:or stats-row))
              :pts (reformat-stat (:pts stats-row))  :ast (reformat-stat (:ast stats-row))
              :st (reformat-stat (:st stats-row))    :dr (reformat-stat (:st stats-row))
              :bs (reformat-stat (:bs stats-row))    :to (reformat-stat (:to stats-row)))))

;; _____________________________________________________________________________________________________________________
;; Synchronously scrape match results

(declare partitioned-scrape)
(defn scrape-all-match-results [match-listings-url, match-listing-ids,
                                match-stats-url-prefix, match-stats-url-suffix, match-stats-ids]
  (let [match-urls (build-match-urls match-stats-url-prefix match-stats-url-suffix
                                     (get-match-ids (load-all-match-listings match-listings-url match-listing-ids)))]
    (flatten (map get-match-stats (filter identity (partitioned-scrape match-urls match-stats-ids))))))

; disposes of the driver every 100 pages, and creates a new one
(defn partitioned-scrape
  ([match-urls match-stats-ids] (partitioned-scrape match-urls match-stats-ids (web/chrome) 0 '()))
  ([match-urls match-stats-ids driver scrape-count data]
   (cond
     (empty? match-urls) data

     (>= scrape-count 100)
     (do (web/close-window driver) (web/stop-driver driver)
           (partitioned-scrape match-urls match-stats-ids (web/chrome) 0 data))

     :else
     (partitioned-scrape (rest match-urls) match-stats-ids driver (inc scrape-count)
                           (conj data (get-match-stats-html (first match-urls) match-stats-ids driver))))))

;; _____________________________________________________________________________________________________________________
;; Write to .csv

(defn write-csv [path data]
  (let [columns [:date :time :home-team :home-score :away-score :away-team
                 :player :team :pts :ast :reb :min :fg :fga :3p :3pa :ft :fta :or :dr :pf :st :to :bs :result]
        headers (map name columns)
        rows (mapv #(mapv % columns) data)]
    (with-open [file (clojure.java.io/writer path)]
      (csv/write-csv file (cons headers rows)))))