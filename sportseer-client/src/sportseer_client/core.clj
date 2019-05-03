(ns sportseer_client.core
  (:gen-class)
  (:require [seesaw.core]
            [sportseer_client.scraper :as scr]
            [sportseer_client.dataset_creator :as dat]
            [sportseer_client.machine_learning :as mac]
            [clojure.java.io :as io])
  (:import (javax.imageio ImageIO)))

(use 'seesaw.core)
(native!) ; turns the UIs style into the native OS's

; paths for data locations
(defn create-path [dir-list] (str (.normalize (java.nio.file.Paths/get "" (into-array String dir-list)))))
(def client-ui-path (create-path ["client-ui"]))
(def data-path (create-path ["data"]))
(def scraped-data-path (create-path [data-path "scraped-data"]))
(def dataset-path (create-path [data-path "datasets"]))
(def model-path  (create-path [data-path "models"]))
(def prediction-request-path (create-path [data-path "prediction-request"]))

;; _____________________________________________________________________________________________________________________
;; Global variables

(def transparent ^:const (seesaw.color/color 0 0 0 0))
(def default-text-colour ^:const
  (let [colour (seesaw.color/default-color "Label.foreground")]
    (seesaw.color/color (.getRed colour) (.getGreen colour) (.getBlue colour))))
(def sportseer-orange ^:const (seesaw.color/color 255 127 42 255))

;; _____________________________________________________________________________________________________________________
;; Helper Functions

(declare change-menu)

(defn set-selected-colour [e] (config! e :foreground sportseer-orange))

(def title-logo
  (label :icon (seesaw.icon/icon (ImageIO/read (io/file (create-path [client-ui-path "sportseer-logo-text-half.png"]))))))

; resizes the frame to the set max width (which matches that of the logo image).
; this stops styled-text elements from expanding the window width
(defn max-width-pack! [f] (do (pack! f) (config! f :size [584 :by (.height (.getSize f))]) f))

; Allows the ability to resize a single axis of a UI element without affecting the axis
(defn resize! [obj axis value]
  (let [opp-axis-value (if (= axis :width) (.height (.getSize obj)) (.width (.getSize obj)))]
    (config! obj :size (if (= axis :width) [value :by opp-axis-value] [opp-axis-value :by value]))))

;; _____________________________________________________________________________________________________________________
;; Initial Frame Set Up

(def f (frame :title "SPORTSEER - Sports Match Prediction Client" :resizable? true :on-close :exit
              :icon (seesaw.icon/icon (ImageIO/read(io/file (create-path [client-ui-path "sportseer-logo.png"]))))))

;; _____________________________________________________________________________________________________________________
;; Loading Screen

; creates and indeterminate loading screen that doesn't show progress but instead letting the user know something is loading
; sadly does not appear in this version unless the program crashes while it is 'loading'.
(defn loading-screen [loading-text]
  (do (config!
        f :content (border-panel
                     :north title-logo
                     :center (styled-text :text loading-text
                                          :wrap-lines? true :enabled? false
                                          :background transparent :foreground default-text-colour)
                     :south (progress-bar :indeterminate? true
                                          :foreground sportseer-orange) ;can't find the color attribute for the bar
                     :vgap 5 :hgap 5 :border 15))
      (-> f max-width-pack!)))

;; _____________________________________________________________________________________________________________________
;; Scraper Menu

(defn start-scrape [e combo] ; e is used so this can be called within an anonymous function for an action listener
  (let [selected-name (selection combo)
        url (scr/get-nba-match-url selected-name)]
    (scr/write-csv (create-path [scraped-data-path (str selected-name " Player Statistics.csv")])
      (scr/scrape-all-match-results url scr/match-listing-ids scr/nba-match-prefix-url
        scr/nba-match-player-stats-suffix-url scr/match-stats-ids))))

(defn scraper-menu []
  (let [combo (combobox :model (map :name scr/nba-match-urls))]
    (border-panel
      :south (flow-panel :items[combo (button :id "start-scrape-button" :class :type :mnemonic \t
                                              :text "Start Scraping" :listen [:action #(start-scrape % combo)])]))))

;; _____________________________________________________________________________________________________________________
;; Dataset Menu

(declare dataset-menu)

; uses the required functions from dataset_creator to create the user-specified dataset
(defn create-dataset [e average-combo aggr-combo dataset-name]
  (let [selected-data (map str (selection (select f [:#scraped-data-selector]) {:multi? true}))]
    (do (loading-screen "Manipulating scraped data into the desired dataset.\nThis may take a while...")
      (if (empty? selected-data)
        (change-menu dataset-menu)
        (let [csv-data (dat/multi-import-csv selected-data)
              average-algorithm (dat/get-average-algorithm (selection average-combo))
              aggr-algorithm (dat/get-aggregation-algorithm (selection aggr-combo))
              averaged-data (average-algorithm (dat/group-data-by-x csv-data :player))
              aggregated-data (dat/aggregate-players averaged-data aggr-algorithm)
              combined-data (dat/combine-match-data aggregated-data)]
          (do (dat/write-csv (create-path [dataset-path (text dataset-name)]) combined-data dat/aggregated-dataset-columns)
              (change-menu (dataset-menu))))))))

; These are the buttons on the bottom half of the create dataset menu, underneath the data selector.
(defn dataset-buttons []
  (let [average-combo (combobox :model (map :name dat/average-algorithms))
        aggr-combo (combobox :model (map :name dat/aggregation-algorithms))
        dataset-name (text :text "dataset.csv" :size [458 :by 31])
        bg (button-group)]
    (border-panel
      :north (flow-panel :items [(label :text "Averaging Algorithm") average-combo
                                 (label :text "Aggregation Algorithm") aggr-combo])
      :center (flow-panel :items [(label :text "Dataset Name") dataset-name])
      :south (flow-panel :items [(button :id "change-data-folder-button" :class :type :group bg :mnemonic \f
                                         :text "Change Data Folder" :enabled? false)
                                 (button :id "dataset-start-button" :class :type :group bg :mnemonic \c
                                         :text "Create Dataset" :size [415 :by 31]
                                         :listen [:action #(create-dataset % average-combo aggr-combo dataset-name)])]))))

; puts together all of the UI elements that make up the dataset menu
(defn dataset-menu []
  (border-panel
    :north (styled-text :text (str "Select the scraped data to use when creating a dataset.\n"
                               "Multiple can be selected by Shift and Control Clicking.")
                        :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour)
    :center (scrollable (listbox :id "scraped-data-selector" :model (drop 1 (-> scraped-data-path io/file file-seq sort))))
    :south (dataset-buttons) :vgap 5 :hgap 5))

;; _____________________________________________________________________________________________________________________
;; Training Menu

;; ---------------------------------------------------------------------------------------------------------------------
;; New Model Configuration

(declare get-layer-nodes)

; creates the network description as specified in the network description designer
(defn build-network-description
  ([data-column-count] (build-network-description data-column-count (get-layer-nodes) []))
  ([data-column-count layer-nodes network-description]
   (build-network-description layer-nodes (conj network-description (mac/get-input-layer data-column-count))))
  ([layer-nodes network-description]
   (if (empty? layer-nodes) (conj network-description (mac/get-output-layer))
     (let [current-layer (first layer-nodes)]
       (build-network-description
         (rest layer-nodes) (conj network-description ((mac/get-layer (:name current-layer))(:value current-layer))))))))

; uses the required functions from machine_learning to create a model
(defn create-model [csv-data-file-name optimizer-selector
                    batch-size-selector epoch-count-selector epoch-size-selector model-name]
  (let [data-columns (selection (select f [:#data-columns-selector]) {:multi? true})
        label-column (selection (select f [:#label-column-selector]) {:multi? false})
        optimizer (mac/get-optimizer (selection optimizer-selector))
        batch-size (selection batch-size-selector)
        epoch-count (selection epoch-count-selector)
        epoch-size (selection epoch-size-selector)
        model-name (text model-name)
        network-description (build-network-description (count data-columns))]
    (mac/save-model (mac/create-model model-name csv-data-file-name data-columns label-column 20 network-description
                                      {:optimizer optimizer :batch-size batch-size
                                       :epoch-count epoch-count :epoch-size epoch-size}))))

; the default network description that is shown in the designer when the menu is first accessed.
(def starting-network-description
  [(assoc (nth mac/layers-list 1) :value 32) (assoc (nth mac/layers-list 2) :value 0.9)
   (assoc (nth mac/layers-list 1) :value 20) (assoc (nth mac/layers-list 3) :value 2)])

; gets the data representation of the UI elements that make up the user specified layers in the network description
(defn get-layer-nodes
  ([] (get-layer-nodes 0 []))
  ([idx layer-nodes]
   (let [layer-node (select f [(keyword (str "#layer-" idx))])]
     (if (nil? layer-node)
       (get-layer-nodes layer-nodes)
       (get-layer-nodes (inc idx) (conj layer-nodes layer-node)))))
  ([layer-nodes]
   (map #(assoc {} :name (selection (select % [:#layer-selector])) :value (selection (select % [:#value-selector])))
        layer-nodes)))

; removes a layer from the list of layer nodes
(defn delete-layer
  ([idx] (delete-layer (get-layer-nodes) idx))
  ([layer-nodes idx] (delete-layer layer-nodes idx 0 []))
  ([layer-nodes target-idx current-idx output]
   (cond
     (empty? layer-nodes) output
     (= current-idx target-idx) (delete-layer (rest layer-nodes) target-idx (inc current-idx) output)
     :else (delete-layer (rest layer-nodes) target-idx (inc current-idx) (conj output (first layer-nodes))))))

; swaps two layers in the list of layer nodes
(defn swap-layers
  ([idx direction] (swap-layers (get-layer-nodes) idx direction))
  ([layer-nodes idx direction]
   (if (= direction :up)
     (swap-layers layer-nodes 0 (assoc {} :idx (dec idx) :node (nth layer-nodes idx))
                  (assoc {} :idx idx :node (nth layer-nodes (dec idx))) [])
     (swap-layers layer-nodes 0 (assoc {} :idx idx :node (nth layer-nodes (inc idx)))
                  (assoc {} :idx (inc idx) :node (nth layer-nodes idx)) [])))
  ([layer-nodes idx up-node down-node output]
   (cond
     (empty? layer-nodes) output
     (= idx (:idx up-node)) (swap-layers (rest layer-nodes) (inc idx) up-node down-node (conj output (:node up-node)))
     (= idx (:idx down-node)) (swap-layers (rest layer-nodes) (inc idx) up-node down-node (conj output (:node down-node)))
     :else (swap-layers (rest layer-nodes) (inc idx) up-node down-node (conj output (first layer-nodes))))))

(declare update-description-editor)

; creates the ui element for a layer node from the data representation
(defn create-layer-node [name value idx]
  (let [layer-selector (combobox :id "layer-selector" :model (map :name mac/layers-list))
        value-spinner (spinner :id "value-selector" :model (spinner-model (float value) :by 1.0))
        up-button (button :text "Up" :listen [:action (fn [x] (update-description-editor (swap-layers idx :up)))])
        down-button (button :text "Dn" :listen [:action (fn [x] (update-description-editor (swap-layers idx :down)))])
        delete-button (button :text "X" :listen [:action (fn [x] (update-description-editor (delete-layer idx)))])]
    (do (selection! layer-selector name)
        (horizontal-panel :id (str "layer-" idx) :items [layer-selector value-spinner up-button down-button delete-button]))))

; adds a default layer to the end of the list of layer nodes
(defn add-layer
  ([] (add-layer (get-layer-nodes)))
  ([layer-nodes] (reverse (conj (reverse layer-nodes) (assoc {} :name "Linear" :value 10)))))

; converts the list of node layers into the ui elements and builds up the editor using them
; this is recalled whenever the layer list is manipulated and the old editor gets replaced by this new one
(defn description-editor
  ([layers-list] (description-editor layers-list 0 []))
  ([layers-list idx layer-nodes]
   (if (empty? layers-list)
     (border-panel :north (label :text "Network Description") :center (vertical-panel :items layer-nodes)
                   :south (button :text "Add Layer" :listen [:action (fn [x] (update-description-editor (add-layer)))]))
     (let [current-layer (first layers-list)]
       (description-editor (rest layers-list) (inc idx)
                           (conj layer-nodes (create-layer-node (:name current-layer) (:value current-layer) idx)))))))

; replaces the current description editor with the new one after manipulation
(defn update-description-editor [layer-nodes]
  (do (config! (select f [:#description-editor]) :items [(description-editor layer-nodes)]) (max-width-pack! f)))

(declare create-new-model-menu)
(declare training-menu)

; brings together the ui elements that make up the model configuration menu
(defn change-to-model-configuration-menu [dataset]
  (let [csv-data (mac/import-csv (str dataset))
        columns (map name (keys (first csv-data)))
        dataset-column-selector (scrollable (listbox :id "data-columns-selector" :model columns))
        label-column-selector (scrollable (listbox :id "label-column-selector" :model columns))
        optimizer-selector (combobox :model (map :name mac/optimizers))
        batch-size-selector (spinner :model 100)
        epoch-count-selector (spinner :model 10)
        epoch-size-selector (spinner :model 200000)
        model-name (text :text "Model" :size [468 :by 31])
        menu (border-panel
               :north (border-panel
                        :north (label :text "Model Configuration") :south (separator)
                        :center (horizontal-panel
                                  :items [(label :text "Dataset File: ")
                                          (styled-text :text (str dataset) :wrap-lines? true :enabled? false
                                                       :background transparent :foreground default-text-colour)]))
               :center (border-panel
                         :west (vertical-panel :items [(label :text "Dataset Columns") dataset-column-selector])
                         :east (vertical-panel :items [(label :text "Label Column") label-column-selector])
                         :south (vertical-panel :items [(description-editor starting-network-description)]
                                                :id "description-editor"))
               :south (border-panel
                            :north (horizontal-panel
                                     :items [(vertical-panel :items [(label :text "Optimiser") optimizer-selector])
                                             (vertical-panel :items [(label :text "Batch Size") batch-size-selector])
                                             (vertical-panel :items [(label :text "Epoch Count") epoch-count-selector])
                                             (vertical-panel :items [(label :text "Epoch Size") epoch-size-selector])])
                            :center (flow-panel :items [(label :text "Model Name") model-name])
                            :south (button :class :type :mnemonic \c :text "Create Model and Start Training"
                                           :listen [:action (fn [x] (create-model (str dataset) optimizer-selector
                                                                              batch-size-selector epoch-count-selector
                                                                              epoch-size-selector model-name))])))]
    (do (change-menu (training-menu menu))
        (resize! dataset-column-selector :width 270) (resize! label-column-selector :width 270))))



;; ---------------------------------------------------------------------------------------------------------------------
;; New Model Sub Menu

; buttons on the bottom when choosing datasets to be used in the a new model
(def new-model-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "change-dataset-folder-button" :class :type :group bg :mnemonic \f
                                :text "Change Dataset Folder" :enabled? false)
                        (button :id "new-continue-button" :class :type :group bg :mnemonic \c
                                :text "Continue" :size [400 :by 31]
                                :listen [:action (fn [x] (change-to-model-configuration-menu
                                                           (selection (select f [:#dataset-selector]))))])])))

; puts together the buttons and the dataset selector into a menu
(defn create-new-model-menu []
  (border-panel
    :north (styled-text :text "Select the Dataset you want to use on the new model."
                        :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour)
    :center (scrollable (listbox :id "dataset-selector" :model (drop 1 (-> dataset-path io/file file-seq sort))))
    :south new-model-buttons))

;; ---------------------------------------------------------------------------------------------------------------------
;; Existing Model Sub Menu - Disabled

; gets the model names within the data structure after deserialising all of the models within the model folder,
; these names get shown alongside the file name (is also used in the prediction menu model selector)
(defn get-model-names
  ([model-file-names] (get-model-names model-file-names []))
  ([model-file-names output]
   (if (empty? model-file-names) output
     (let [current-file (first model-file-names)]
       (get-model-names (rest model-file-names) (conj output (str (:name (mac/load-model current-file)) " [" current-file "]")))))))
(defn model-name->file-name [model-name] (clojure.string/join "" (drop-last (second (clojure.string/split model-name #"\[")))))

; buttons used in the existing model menu
(def existing-model-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "change-existing-model-folder-button" :class :type :group bg :mnemonic \f
                                :text "Change Model Folder" :enabled? false)
                        (button :id "train-start-button" :class :type :group bg :mnemonic \t
                                :text "Start Training" :size [400 :by 31])])))

; brings together the elements needed to make the existing model menu
(defn existing-model-menu []
  (border-panel :north (styled-text :text "Select the Model you want to train." :wrap-lines? true :enabled? false
                                    :background transparent :foreground default-text-colour)
                :center (scrollable
                          (listbox :model (get-model-names (map #(str %) (drop 1 (-> model-path io/file file-seq sort))))))
                :south existing-model-buttons))

;; ---------------------------------------------------------------------------------------------------------------------
;; Main Training Menu

; resets the text colours of the menu when switching between sub menus so its easier to tell which menu is currently selected
(defn reset-training-menu-buttons [e]
  (config! (select f [:#new-model-button]) :foreground default-text-colour)
  (config! (select f [:#train-model-button]) :foreground default-text-colour))

(declare training-menu)
; buttons used to switch between sub menus for the training menu
(def training-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "new-model-button" :class :type :group bg :mnemonic \n :text "Create New Model"
                                :listen [:action #(do (change-menu (training-menu (create-new-model-menu)))
                                                    (reset-training-menu-buttons %) (set-selected-colour %))])
                        (button :id "train-model-button" :class :type :group bg :mnemonic \t
                                :text "Train Existing Model" :enabled? false
                                :listen [:action #(do (change-menu (training-menu (existing-model-menu)))
                                                      (reset-training-menu-buttons %) (set-selected-colour %))])])))

; text shown when the training menu is first opening
(def training-menu-default
  (styled-text :text "Choose whether to create a new model or to train an existing model."
               :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour))

; used to replace the starting text with the sub menu when a button is pressed
(defn training-menu
  ([] (training-menu training-menu-default))
  ([sub-menu] (border-panel :id "training-menu" :north training-menu-buttons :center (separator) :south sub-menu)))

;; _____________________________________________________________________________________________________________________
;; Prediction Menu

;; ---------------------------------------------------------------------------------------------------------------------
;; Results Page

; creates two panel elements and sizes them to visualise the confidence in a result
; the higher percentage gets coloured orange, the other black
(defn confidence-bar [home-win? confidence]
  (if home-win?
    (flow-panel
      :items [(horizontal-panel :size [(- 272 (* confidence 2.72)) :by 10] :background (seesaw.color/color :black))
              (horizontal-panel :size [(+ 272 (* confidence 2.72)) :by 10] :background sportseer-orange)])
    (flow-panel
      :items [(horizontal-panel :size [(+ 272 (* confidence 2.72)) :by 10] :background sportseer-orange)
              (horizontal-panel :size [(- 272 (* confidence 2.72)) :by 10] :background (seesaw.color/color :black))])))

; an element node that shows the home and away win percentages above a confidence bar
(defn results-node [results]
  (let [home-result (vertical-panel :items [(label :text "Home Win %") (label :text (* (:home-win results) 100))])
        away-result (vertical-panel :items [(label :text "Away Win %") (label :text (* (:away-win results) 100))])
        confidence-bar (confidence-bar (:home-win? results) (:confidence results))]
    (border-panel :west away-result :east home-result :south confidence-bar)))

; puts together a list of results-nodes to show one or more results from predictions requested
(defn results-page [results]
  (if (= 1 (count results))
    (results-node (first results))
    (flatten (vector (results-node (first results)) (results-page (rest results))))))


;; ---------------------------------------------------------------------------------------------------------------------
;; Manual Prediction Menu

; makes a data representation of the manual inputs made by the user
; this is used to then manipulate the list, or to use the list for predictions
(defn get-manual-selection-nodes
  ([columns] (assoc {} :home (get-manual-selection-nodes columns "home" 0 [])
                       :away (get-manual-selection-nodes columns "away" 0 [])))
  ([columns side idx output]
   (let [result (get-manual-selection-nodes columns (str "#selector-" side "-" idx "-") {})]
     (if result (get-manual-selection-nodes columns side (inc idx) (conj output result)) output)))
  ([columns id-prefix output]
   (if (empty? columns) output
      (let [current-column (first columns)
            ui-node (select f [(keyword (str id-prefix current-column))])]
        (if (not ui-node) nil
          (get-manual-selection-nodes (rest columns) id-prefix (assoc output (keyword current-column) (selection ui-node))))))))

; merges the manual selections in a similar way as done in dataset_creator, but is more lightweight
(defn merge-manual-selections [home-data away-data columns]
  (let [column (keyword (first columns))]
    (if (= 1 (count columns))
      (assoc {} column (- (column home-data) (column away-data)))
      (merge (assoc {} column (- (column home-data) (column away-data)))
             (merge-manual-selections home-data away-data (rest columns))))))

; prepares the manual selections to be used in a prediction request
(defn prepare-manual-selections
  ([columns]
   (let [selections (get-manual-selection-nodes columns)
         home-data (first (prepare-manual-selections (:home selections) columns))
         away-data (first (prepare-manual-selections (:away selections) columns))
         columns (drop-last columns)]
     (merge-manual-selections home-data away-data columns)))
  ([selections columns]
   (if (= 1 (count selections)) (map #(dissoc % :AVG) selections)
     (let [selections (dat/aggr-minutes (map #(dissoc (assoc % :min (:AVG %)) :AVG) selections))]
       (if (contains? columns "min") (map #(dissoc % :min) selections) selections)))))

; creates a player node to then be added to the list
(defn create-player-node [columns]
  (if (= 1 (count columns)) (assoc {} (keyword (first columns)) 0.0)
    (merge (assoc {} (keyword (first columns)) 0.0) (create-player-node (rest columns)))))

(declare manual-selector)
; adds a player to the specified side (home/away)
(defn add-player [side columns]
  (let [nodes (get-manual-selection-nodes columns)]
    (assoc nodes side (conj (side nodes) (create-player-node columns)))))

; the column of spinners used for manual selections
; it is done in columns instead of rows so only one header is needed and to make sure they're all aligned
(defn manual-selector-column
  ([values column side]
   (border-panel :north (label column) :center (vertical-panel :items (manual-selector-column values column side 0 []))))
  ([values column side idx output]
   (if (empty? values) output
     (let [spinner (spinner :id (str "selector-" side "-" idx "-" column)
                            :model (spinner-model (float (first values)) :by 1.0))]
       (do (config! spinner :size [60 :by 25])
           (manual-selector-column (rest values) column side (inc idx) (conj output spinner)))))))

; creates and puts together all of the columns needed for the manual selection list for a given side
(defn manual-selector-columns [nodes side]
  (let [nodes (side nodes)
        side (name side)
        manual-selectors (map (fn [column] (manual-selector-column (map column nodes) (name column) side))
                              (keys (first nodes)))]
    (if (= 1 (count nodes))
      manual-selectors
      (cons (border-panel
              :north (label :text "#")
              :center (vertical-panel :items (into [] (map #(label :text (str %) :size [10 :by 25]) (range (count nodes))))))
            manual-selectors))))

; resizes the selector panel, if the selector panel only has 5 or less rows it will size it perfectly to fit them rows
; beyond that it has a scroll bar
(defn resize-selector-panel [panel rows]
  (if (<= rows 5) (resize! panel :height (+ 32 (* 25 rows))) (resize! panel :height 162)))

; creates the ui elements needed for the manual selector, both the home and away sections
(defn manual-selector [nodes columns refresh-menu-fn]
  (let [away-selectors (scrollable (horizontal-panel :items (manual-selector-columns nodes :away)))
        home-selectors (scrollable (horizontal-panel :items (manual-selector-columns nodes :home)))]
    (do (resize-selector-panel away-selectors (count (:away nodes)))
        (resize-selector-panel home-selectors (count (:home nodes)))
      (border-panel
        :north (border-panel :north (label "Away Team / Challenger(s)") :center away-selectors
                             :south (button :text "Add Player/Challenger"
                                            :listen [:action (fn [x] (refresh-menu-fn (add-player :away columns)))]))
        :south (border-panel :north (label "Home Team / Defender(s)") :center home-selectors
                             :south (button :text "Add Player/Defender"
                                            :listen [:action (fn [x] (refresh-menu-fn (add-player :home columns)))]))))))

(declare prediction-menu)

; puts together the manual selector, and buttons with the functionality to make a prediction request
(defn manual-prediction-menu
  ([model-name]
   (let [model-file-name (model-name->file-name model-name)
         model (mac/load-model model-file-name)
         columns (conj (into [] (:data-columns model)) "AVG")]
     (manual-prediction-menu model columns {:away [(create-player-node columns)] :home [(create-player-node columns)]})))
  ([model columns selector-nodes]
   (let [refresh-menu-fn (fn [nodes] (change-menu (prediction-menu (manual-prediction-menu model columns nodes))))]
     (border-panel
       :north (border-panel
                :north (styled-text :text (str "The 'AVG' Column is only used when multiple rows are used for a team.\n"
                                               "Currently, only the Minutes Aggregation Algorithm is available, so enter"
                                               "the player's 'minutes played' statistic for a more accurate prediction.")
                                    :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour)
                :south (separator))
       :center (manual-selector selector-nodes columns refresh-menu-fn)
       :south (button :text "Get Prediction"
                      :listen [:action (fn [x] (let [selections (vector (prepare-manual-selections columns))]
                                                 (change-menu (prediction-menu
                                                                (results-page
                                                                  (mac/create-predictions selections model))))))])
       :vgap 5 :hgap 5))))

;; ---------------------------------------------------------------------------------------------------------------------
;; Bulk Prediction Menu

; the menu used to select a csv file to use to make predictions
(defn bulk-prediction-menu [model-name]
  (let [model-file-name (model-name->file-name model-name)
        model (mac/load-model model-file-name)]
    (border-panel
      :center (scrollable (listbox :id "stats-selector" :model (drop 1 (-> prediction-request-path io/file file-seq sort))))
      :south (button :text "Get Prediction(s)"
                     :listen [:action (fn [x]
                                        (let [csv-data (dat/import-csv (selection (select f [:#stats-selector])))
                                              aggr-fn (if (some #(= :min %) (keys (first csv-data)))
                                                        dat/aggr-minutes dat/aggr-average)
                                              combined-data (into [] (dat/combine-match-data
                                                                       (dat/aggregate-players csv-data aggr-fn)))]
                                          (change-menu
                                            (prediction-menu (results-page (mac/create-predictions combined-data model))))))]))))


;; ---------------------------------------------------------------------------------------------------------------------
;; Model Selection Menu

; the model selection menu used for both the bulk menu and the manual selection menu
(defn prediction-model-select-menu [sub-menu-fn]
  (border-panel :north (styled-text :text "Select the Model you want to use for predictions."
                                    :wrap-lines? true :enabled? false
                                    :background transparent :foreground default-text-colour)
                :center (scrollable
                          (listbox :id "model-selector"
                                   :model (get-model-names (map #(str %) (drop 1 (-> model-path io/file file-seq sort))))))
                :south (button :text "Select Model"
                               :listen [:action
                                        (fn [x] (change-menu
                                                  (prediction-menu (sub-menu-fn (selection (select f [:#model-selector]))))))])))

;; ---------------------------------------------------------------------------------------------------------------------
;; Main Prediction Menu

; resets the colours for the main menu for the prediction section
(defn reset-prediction-menu-buttons []
  (config! (select f [:#manual-prediction-button]) :foreground default-text-colour)
  (config! (select f [:#bulk-prediction-button]) :foreground default-text-colour))

; the main menu for the prediction section
(def prediction-menu-buttons
  (let [bg (button-group)]
    (flow-panel
      :items [(button :id "manual-prediction-button" :class :type :group bg :mnemonic \n :text "Manual Input"
                      :listen [:action #(do (change-menu (prediction-menu (prediction-model-select-menu manual-prediction-menu)))
                                          (reset-prediction-menu-buttons) (set-selected-colour %))])
              (button :id "bulk-prediction-button" :class :type :group bg :mnemonic \b :text "Bulk Input"
                      :listen [:action #(do (change-menu (prediction-menu (prediction-model-select-menu bulk-prediction-menu)))
                                          (reset-prediction-menu-buttons) (set-selected-colour %))])])))

; the text that is shown when the prediction menu is first accessed
(def prediction-menu-default
  (styled-text :text "Choose whether to input prediction requests manually or bulk import them from a .csv file."
               :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour))

; puts together the menu buttons and the sub menu specified
(defn prediction-menu
  ([] (prediction-menu prediction-menu-default))
  ([sub-menu] (border-panel :id "prediction-menu" :north prediction-menu-buttons :center (separator) :south sub-menu)))

;; _____________________________________________________________________________________________________________________
;; Main Menu Functionality

; resets the main menu button colours
(defn reset-main-menu-buttons [e]
  (config! (select f [:#scrape-button]) :foreground default-text-colour)
  (config! (select f [:#dataset-button]) :foreground default-text-colour)
  (config! (select f [:#train-button]) :foreground default-text-colour)
  (config! (select f [:#predict-button]) :foreground default-text-colour))

; the buttons used for navigating between sections of the program
(def main-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "scrape-button" :class :type :group bg :mnemonic \s :text "Data Scraper"
                                :listen [:action #(do (change-menu (scraper-menu))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "dataset-button" :class :type :group bg :mnemonic \d :text "Datasets"
                                :listen [:action #(do (change-menu (dataset-menu))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "train-button" :class :type :group bg :mnemonic \m :text "Models"
                                :listen [:action #(do (change-menu (training-menu))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "predict-button" :class :type :group bg :mnemonic \p :text "Predictions"
                                :listen [:action #(do (change-menu (prediction-menu))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])])))

; the elements that make up the area for the main menu buttons
(def main-menu (border-panel :north (separator) :center main-menu-buttons :south (separator)))

; changes the content of the frame whilst keeping the main menu
(defn change-menu [content]
  (do (config! f :content (border-panel :north title-logo :center main-menu :south content :vgap 5 :hgap 5 :border 15))
      (-> f max-width-pack!)))

; main entry point for the program, changes the menu to the startup screen and shows the frame
(defn -main [& args]
  (do (change-menu (styled-text :text (str "This is a client for creating and managing machine learning models"
                                           "designed to predict sports match outcomes.\n\nCreated by Scott Taylor\n"
                                           "For my BSc Computer Science Final Year Project at Teesside University\n\n"
                                           "Scraped data comes from scoreboard.com")
                                :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour))
      (show! f)))