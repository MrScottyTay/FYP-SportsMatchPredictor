(ns sportseer-client.core
  (:require [seesaw.core])
  (:import (javax.imageio ImageIO)))

(use 'seesaw.core)

;; _____________________________________________________________________________________________________________________
;; Global variables

(def transparent (seesaw.color/color 0 0 0 0))
(def default-text-colour
  (let [colour (seesaw.color/default-color "Label.foreground")]
    (seesaw.color/color (.getRed colour) (.getGreen colour) (.getBlue colour))))
(def sportseer-orange (seesaw.color/color 255 127 42 255))

;; _____________________________________________________________________________________________________________________
;; Initial Frame Set Up

(native!)
(def f (frame :title "SPORTSEER - Sports Match Prediction Client" :resizable? false
              :icon (seesaw.icon/icon (ImageIO/read (clojure.java.io/file "sportseer-logo.png")))))

;; _____________________________________________________________________________________________________________________
;; Skeleton Temps

;; Dataset Menu Temp listbox
(def seasons-list ["NBA Season 2010-2011", "NBA Season 2011-2012", "NBA Season 2012-2013",
                   "NBA Season 2013-2014", "NBA Season 2014-2015", "NBA Season 2015-2016",
                   "NBA Season 2016-2017", "NBA Season 2017-2019 (incomplete)"])
(def temp-seasons-list (listbox :model (-> seasons-list sort)))
(selection temp-seasons-list {:multi? true})

(def datasets-list ["NBA 10-17 [match-0 minutes-weighted]"
                    "NBA 10-17 [minutes-weighted]"
                    "NBA 16-17 [match-0 minutes-weighted]"])
(def temp-datasets-list (listbox :model (-> datasets-list sort)))
(selection temp-datasets-list {:multi? true})

;; _____________________________________________________________________________________________________________________
;; Dataset Menu

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

;; _____________________________________________________________________________________________________________________
;; Training Menu

(def training-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :text "Create New Model")
                        (button :text "Train Existing Model")])))

(def training-menu
  (border-panel :id "training-menu"
                :center training-menu-buttons))

;; _____________________________________________________________________________________________________________________
;; Main Menu Functionality

(defn reset-main-menu-buttons [e]
  ;this is optimal - but it dont work :@
  #_(map #(config! % :foreground default-text-colour) (select e [:<javax.swing.JButton>>]))
  (config! (select f [:#scrape-button]) :foreground default-text-colour)
  (config! (select f [:#dataset-button]) :foreground default-text-colour)
  (config! (select f [:#train-button]) :foreground default-text-colour)
  (config! (select f [:#predict-button]) :foreground default-text-colour))

(defn set-selected-colour [e]
  (config! e :foreground sportseer-orange))

(declare change-menu)
(def main-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "scrape-button" :class :type :group bg :mnemonic \s
                                :text "Data Scraper"
                                :listen [:action #(do (change-menu (label :text "Scrape Menu"))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "dataset-button" :class :type :group bg :mnemonic \d
                                :text "Datasets"
                                :listen [:action #(do (change-menu dataset-menu)
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "train-button" :class :type :group bg :mnemonic \m
                                :text "Models"
                                :listen [:action #(do (change-menu training-menu)
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "predict-button" :class :type :group bg :mnemonic \p
                                :text "Predictions"
                                :listen [:action #(do (change-menu (label :text "Prediction Menu"))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])])))

(def main-menu (border-panel :north (separator) :center main-menu-buttons :south (separator)))
(def title-logo
  (label :icon (seesaw.icon/icon (ImageIO/read (clojure.java.io/file "sportseer-logo-text-half.png")))))

(defn max-width-pack! [f]
  (do (pack! f)
      (config! f :size [584 :by (.height (.getSize f))])
      f))

(defn change-menu [content]
  (config! f :content (border-panel :north title-logo :center main-menu :south content
                                    :vgap 10 :hgap 5 :border 15))
  (-> f max-width-pack!))

(change-menu (styled-text :text (str "This is a client for creating and managing "
                                     "machine learning models"
                                     "designed to predict sports match outcomes.\n\n"
                                     "Created by Scott Taylor\n"
                                     "For my BSc Computer Science Final Year Project at Teesside University")
                          :wrap-lines? true :background transparent))
(-> f max-width-pack! show!)