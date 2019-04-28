(defproject sportseer-client "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.4"]
                 ; UI
                 [seesaw "1.4.5"]
                 ; Scraper
                 [etaoin "0.3.2"]
                 [enlive "1.1.6"]
                 ; Dataset Creator
                 [clj-time "0.15.0"]
                 ; Machine Learning
                 [thinktopic/experiment "0.9.22"]
                 [com.taoensso/nippy "2.14.0"]])

