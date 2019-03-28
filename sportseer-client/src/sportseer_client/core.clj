(ns sportseer-client.core
  (:require [seesaw.core])
  (:import (javax.imageio ImageIO)))

(use 'seesaw.core)

(def transparent (seesaw.color/color 0 0 0 0))

(native!)
(def f (frame :title "SPORTSEER - Sports Match Prediction Client" :resizable? false
              :icon (seesaw.icon/icon (ImageIO/read (clojure.java.io/file "sportseer-logo.png")))))
(config! f :size [800 :by 800])

(def seasons-list ["NBA Season 2010-2011", "NBA Season 2011-2012", "NBA Season 2012-2013",
                   "NBA Season 2013-2014", "NBA Season 2014-2015", "NBA Season 2015-2016",
                   "NBA Season 2016-2017", "NBA Season 2017-2019 (incomplete)"])

(def temp-seasons-list (listbox :model (-> seasons-list sort)))
(selection temp-seasons-list  {:multi? true})

(def dataset-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :text "Change Data Folder")
                        (button :id "dataset-start-button" :class :type :text "Create Dataset"
                                :size [415 :by 31])])))

(def dataset-menu
  (border-panel :north (styled-text :text (str "Select the scraped data to use when creating a dataset.\n"
                                           "Multiple can be selected by Shift and Control Clicking.")
                                    :wrap-lines? true
                                    :background transparent)
                :center (scrollable temp-seasons-list) ; lb is temporary
                :south dataset-menu-buttons
                :vgap 5 :hgap 5))



(defn click-and-key-listen [widget function]
  (listen widget :mouse-clicked function :key-pressed function))

(declare change-menu)
(def main-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "scrape-button" :class :type :text "Scraper" :group bg :mnemonic \s
                                :listen [:action (fn [e] (change-menu (label :text "Scrape Menu")))])
                        (button :id "dataset-button" :class :type :text "Datasets" :group bg :mnemonic \d
                                :listen [:action (fn [e] (change-menu dataset-menu))])
                        (button :id "train-button" :class :type :text "Models" :group bg :mnemonic \m
                                :listen [:action (fn [e] (change-menu (label :text "Training Menu")))])
                        (button :id "predict-button" :class :type :text "Predictions" :group bg :mnemonic \p
                                :listen [:action (fn [e] (change-menu (label :text "Prediction Menu")))])])))

(def main-menu (border-panel :north (separator) :center main-menu-buttons :south (separator)))
(def title-logo
  (label :icon (seesaw.icon/icon (ImageIO/read (clojure.java.io/file "sportseer-logo-text-half.png")))))

(defn change-menu [content]
  (config! f :content (border-panel :north title-logo :center main-menu :south content
                                    :vgap 10 :hgap 5 :border 15))
  (pack! f))

(change-menu (label :text "placeholder"))
(-> f pack! show!)