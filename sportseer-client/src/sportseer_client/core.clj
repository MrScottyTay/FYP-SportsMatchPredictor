(ns sportseer-client.core
  (:require [seesaw.core]
            [sportseer-client.scraper :as scr]
            [sportseer-client.dataset-creator :as dat]
            [sportseer-client.machine-learning :as mac])
  (:import (javax.imageio ImageIO)
           (javafx.scene.text Font)))

(use 'seesaw.core)
(native!)

(defn create-path [dir-list]
  (str (.normalize (java.nio.file.Paths/get "" (into-array String dir-list)))))

(def client-ui-path (create-path ["client-ui"]))
(def data-path (create-path ["data"]))
(def scraped-data-path (create-path [data-path "scraped-data"]))
(def dataset-path (create-path [data-path "datasets"]))
(def model-path  (create-path [data-path "models"]))
(def prediction-request-path (create-path [data-path "prediction-request"]))
#_(def resources-path (create-path ["resources"]))

#_(def font-awesome
    (Font/loadFont (.toExternalForm (clojure.java.io/resource "resources/font-awesome.otf")) 20.0))

;; _____________________________________________________________________________________________________________________
;; Global variables

(def transparent ^:const (seesaw.color/color 0 0 0 0))
(def default-text-colour ^:const
  (let [colour (seesaw.color/default-color "Label.foreground")]
    (seesaw.color/color (.getRed colour) (.getGreen colour) (.getBlue colour))))
(def sportseer-orange ^:const (seesaw.color/color 255 127 42 255))

;; _____________________________________________________________________________________________________________________
;; Helper Functions

(defn set-selected-colour [e]
  (config! e :foreground sportseer-orange))

(declare change-menu)

(def title-logo
  (label :icon (seesaw.icon/icon
                 (ImageIO/read (clojure.java.io/file (create-path [client-ui-path "sportseer-logo-text-half.png"]))))))

(defn max-width-pack! [f]
  (do (pack! f)
      (config! f :size [584 :by (.height (.getSize f))])
      f))

(defn resize! [obj axis value]
  (let [opp-axis-value (if (= axis :width) (.height (.getSize obj)) (.width (.getSize obj)))]
    (config! obj :size (if (= axis :width) [value :by opp-axis-value]
                                           [opp-axis-value :by value]))))

(defn reset-buttons [frame button-ids]
 (map #(config! (select frame [(keyword (str "#" %))]) :foreground default-text-colour) button-ids))

(defn separate
  ([elements] (separate elements []))
  ([elements output]
   (cond
     (empty? elements) output
     (empty? output) (separate (rest elements) (conj output (first elements)))
     :else (separate (rest elements) (concat output (vector (separator) (first elements)))))))

;; _____________________________________________________________________________________________________________________
;; Initial Frame Set Up

(def f (frame :title "SPORTSEER - Sports Match Prediction Client" :resizable? false
              :icon (seesaw.icon/icon
                      (ImageIO/read(clojure.java.io/file (create-path [client-ui-path "sportseer-logo.png"]))))))
;; _____________________________________________________________________________________________________________________
;; Skeleton Temps

#_(selection temp-seasons-list {:multi? true})

(def datasets-list ["NBA 10-17 [match-0 minutes-weighted]"
                    "NBA 10-17 [minutes-weighted]"
                    "NBA 16-17 [match-0 minutes-weighted]"])
(def temp-datasets-list (listbox :model (-> datasets-list sort)))
(selection temp-datasets-list {:multi? false})

(def models-list ["NBA Model 1", "NBA Model 2", "NBA Model 3"])
(def temp-models-list (listbox :model (-> models-list sort)))
(selection temp-models-list {:multi? false})

;; _____________________________________________________________________________________________________________________
;; Loading Screen

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

(defn start-scrape [e combo]
  (let [selected-name (selection combo)
        url (scr/get-nba-match-url selected-name)]
    (scr/write-csv (create-path [scraped-data-path (str selected-name " Player Statistics.csv")])
      (scr/scrape-all-match-results url scr/match-listing-ids scr/nba-match-prefix-url
        scr/nba-match-player-stats-suffix-url scr/match-stats-ids))))

(def scraper-menu
  (let [combo (combobox :model (map :name scr/nba-match-urls))]
    (border-panel #_:north #_(styled-text :text (str "Select the season you want to scrape.\nScraping is done at"
                                                     "'scoreboard.com'and can take around 3 hours to complete. Please do"
                                                     "not touch chrome during the scrape, as it can hang in result.\n"
                                                     "Chromedriver must be inserted into the path on linux or the '.exe'"
                                                     "must be placed in the 'Utilities' Folder on Windows.")
                                          :wrap-lines? true :enabled? false?
                                          :background transparent :foreground default-text-colour)
                  :south (flow-panel
                           :items[combo
                                  (button :id "start-scrape-button" :class :type :mnemonic \t
                                          :text "Start Scraping" :listen [:action #(start-scrape % combo)])]))))


#_(def scraper-menu
    (styled-text :text "The Scraper has not yet been integrated into the GUI client."
                 :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour))

;; _____________________________________________________________________________________________________________________
;; Dataset Menu

(declare dataset-menu)

(defn create-dataset [e average-combo aggr-combo dataset-name]
  (let [selected-data (map str (selection (select f [:#scraped-data-selector]) {:multi? true}))]
    ; I think a new thread needs to be started for this loading screen to appear, it hangs before it can change
    (do (loading-screen "Manipulating scraped data into the desired dataset.\nThis may take a while...")
      (if (empty? selected-data)
        (change-menu dataset-menu)
        (let [csv-data (dat/multi-import-csv selected-data)
              average-algorithm (dat/get-average-algorithm (selection average-combo))
              aggr-algorithm (dat/get-aggregation-algorithm (selection aggr-combo))
              averaged-data (average-algorithm (dat/group-data-by-x csv-data :player))
              aggregated-data (dat/aggregate-players averaged-data aggr-algorithm)
              combined-data (dat/combine-match-data aggregated-data)]
          (do (dat/write-csv (create-path [dataset-path (text dataset-name)])
                             combined-data dat/aggregated-dataset-columns)
              (change-menu dataset-menu)))))))

(def dataset-buttons
  (let [average-combo (combobox :model (map :name dat/average-algorithms))
        aggr-combo (combobox :model (map :name dat/aggregation-algorithms))
        dataset-name (text :text "dataset.csv"
                           :size [458 :by 31])
        bg (button-group)]
    (border-panel :north (flow-panel :items [(label :text "Averaging Algorithm") average-combo
                                             (label :text "Aggregation Algorithm") aggr-combo])
                  :center (flow-panel :items [(label :text "Dataset Name") dataset-name])
                  :south (flow-panel :items [(button :id "change-data-folder-button" :class :type :group bg :mnemonic \f
                                                     :text "Change Data Folder" :enabled? false)
                                             (button :id "dataset-start-button" :class :type :group bg :mnemonic \c
                                                     :text "Create Dataset"
                                                     :size [415 :by 31]
                                                     :listen [:action
                                                              #(create-dataset
                                                                 % average-combo aggr-combo dataset-name)])]))))

(def dataset-menu
  (border-panel :north (styled-text :text (str "Select the scraped data to use when creating a dataset.\n"
                                           "Multiple can be selected by Shift and Control Clicking.")
                                    :wrap-lines? true :enabled? false
                                    :background transparent :foreground default-text-colour)
                :center (scrollable (listbox :id "scraped-data-selector"
                                             :model (drop 1 (-> scraped-data-path clojure.java.io/file file-seq sort))))
                :south dataset-buttons
                :vgap 5 :hgap 5))

;; _____________________________________________________________________________________________________________________
;; Training Menu

;; ---------------------------------------------------------------------------------------------------------------------
;; New Model Configuration

(declare get-layer-nodes)

(defn build-network-description
  ([data-column-count]
   (build-network-description data-column-count (get-layer-nodes) []))
  ([data-column-count layer-nodes network-description]
   (build-network-description layer-nodes (conj network-description (mac/get-input-layer data-column-count))))
  ([layer-nodes network-description]
   (if (empty? layer-nodes) (conj network-description (mac/get-output-layer))
     (let [current-layer (first layer-nodes)]
       (build-network-description
         (rest layer-nodes) (conj network-description ((mac/get-layer (:name current-layer))(:value current-layer))))))))

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
                                      {:optimizer optimizer
                                       :batch-size batch-size
                                       :epoch-count epoch-count
                                       :epoch-size epoch-size}))))

(def starting-network-description
  [(assoc (nth mac/layers-list 1) :value 32)
   (assoc (nth mac/layers-list 2) :value 0.9)
   (assoc (nth mac/layers-list 1) :value 20)
   (assoc (nth mac/layers-list 3) :value 2)])

(defn get-layer-nodes
  ([] (get-layer-nodes 0 []))
  ([idx layer-nodes]
   (let [layer-node (select f [(keyword (str "#layer-" idx))])]
     (if (nil? layer-node)
       (get-layer-nodes layer-nodes)
       (get-layer-nodes (inc idx) (conj layer-nodes layer-node)))))
  ([layer-nodes]
   (map #(assoc {} :name (selection (select % [:#layer-selector]))
                   :value (selection (select % [:#value-selector])))
        layer-nodes)))

(defn delete-layer
  ([idx] (delete-layer (get-layer-nodes) idx))
  ([layer-nodes idx] (delete-layer layer-nodes idx 0 []))
  ([layer-nodes target-idx current-idx output]
   (cond
     (empty? layer-nodes) output
     (= current-idx target-idx) (delete-layer (rest layer-nodes) target-idx (inc current-idx) output)
     :else (delete-layer (rest layer-nodes) target-idx (inc current-idx) (conj output (first layer-nodes))))))

(defn swap-layers
  ([idx direction] (swap-layers (get-layer-nodes) idx direction))
  ([layer-nodes idx direction]
   (if (= direction :up)
     (swap-layers layer-nodes 0
                  (assoc {} :idx (dec idx) :node (nth layer-nodes idx))
                  (assoc {} :idx idx :node (nth layer-nodes (dec idx))) [])
     (swap-layers layer-nodes 0
                  (assoc {} :idx idx :node (nth layer-nodes (inc idx)))
                  (assoc {} :idx (inc idx) :node (nth layer-nodes idx)) [])))
  ([layer-nodes idx up-node down-node output]
   (cond
     (empty? layer-nodes) output
     (= idx (:idx up-node)) (swap-layers (rest layer-nodes) (inc idx) up-node down-node (conj output (:node up-node)))
     (= idx (:idx down-node)) (swap-layers (rest layer-nodes) (inc idx) up-node down-node (conj output (:node down-node)))
     :else (swap-layers (rest layer-nodes) (inc idx) up-node down-node (conj output (first layer-nodes))))))

(declare update-description-editor)

(defn create-layer-node [name value idx]
  (let [layer-selector (combobox :id "layer-selector" :model (map :name mac/layers-list))
        value-spinner (spinner :id "value-selector" :model value)
        up-button (button :text "Up" :listen [:action (fn [x] (update-description-editor (swap-layers idx :up)))])
        down-button (button :text "Dn" :listen [:action (fn [x] (update-description-editor (swap-layers idx :down)))])
        delete-button (button :text "X" :listen [:action (fn [x] (update-description-editor (delete-layer idx)))])]
    (do (selection! layer-selector name)
        (horizontal-panel :id (str "layer-" idx) :items [layer-selector value-spinner up-button down-button delete-button]))))

(defn add-layer
  ([] (add-layer (get-layer-nodes)))
  ([layer-nodes] (reverse (conj (reverse layer-nodes) (assoc {} :name "Linear" :value 10)))))

(defn description-editor
  ([layers-list] (description-editor layers-list [] 0))
  ([layers-list layer-nodes idx]
   (if (empty? layers-list)
     (border-panel :north (label :text "Network Description")
                   :center (vertical-panel :items layer-nodes)
                   :south (button :text "Add Layer" :listen [:action (fn [x] (update-description-editor (add-layer)))]))
     (let [current-layer (first layers-list)]
       (description-editor (rest layers-list)
                           (conj layer-nodes (create-layer-node (:name current-layer) (:value current-layer) idx))
                           (inc idx))))))

(defn update-description-editor [layer-nodes]
  (do (config! (select f [:#description-editor]) :items [(description-editor layer-nodes)])
      (max-width-pack! f)))

(declare create-new-model-menu)
(declare training-menu)

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
               :north (border-panel :north (label :text "Model Configuration")
                                    :center (horizontal-panel :items [(label :text "Dataset File: ")
                                                                      (styled-text :text (str dataset)
                                                                                   :wrap-lines? true :enabled? false
                                                                                   :background transparent
                                                                                   :foreground default-text-colour)])
                                    :south (separator))
               :center (border-panel :west (vertical-panel :items [(label :text "Dataset Columns")
                                                                   dataset-column-selector])
                                     :center (vertical-panel :items [(label :text "Label Column")
                                                                     label-column-selector])
                                     :south (vertical-panel :items [(description-editor starting-network-description)]
                                                          :id "description-editor"))
               :south (border-panel
                        :north (horizontal-panel :items [(vertical-panel :items [(label :text "Optimiser")
                                                                                 optimizer-selector])
                                                         (vertical-panel :items [(label :text "Batch Size")
                                                                                 batch-size-selector])
                                                         (vertical-panel :items [(label :text "Epoch Count")
                                                                                 epoch-count-selector])
                                                         (vertical-panel :items [(label :text "Epoch Size")
                                                                                 epoch-size-selector])])
                        :center (flow-panel :items [(label :text "Model Name") model-name])
                        :south (button :class :type :mnemonic \c :text "Create Model and Start Training"
                                       :listen [:action (fn [x] (create-model (str dataset) optimizer-selector
                                                                              batch-size-selector epoch-count-selector
                                                                              epoch-size-selector model-name))])))]
    (do (change-menu (training-menu menu))
        #_(resize-width! dataset-column-selector 150)
        #_(resize-width! label-column-selector 150))))



;; ---------------------------------------------------------------------------------------------------------------------
;; New Model Sub Menu

(def new-model-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "change-dataset-folder-button" :class :type :group bg :mnemonic \f
                                :text "Change Dataset Folder" :enabled? false)
                        (button :id "new-continue-button" :class :type :group bg :mnemonic \c
                                :text "Continue"
                                :size [400 :by 31]
                                :listen [:action
                                         (fn [x] (change-to-model-configuration-menu
                                                   (selection (select f [:#dataset-selector]))))])])))

(def create-new-model-menu
  (border-panel :north (styled-text :text "Select the Dataset you want to use on the new model."
                                    :wrap-lines? true :enabled? false
                                    :background transparent :foreground default-text-colour)
                :center (scrollable (listbox :id "dataset-selector"
                                             :model (drop 1 (-> dataset-path clojure.java.io/file file-seq sort))))
                :south new-model-buttons))

;; ---------------------------------------------------------------------------------------------------------------------
;; Existing Model Sub Menu

(defn get-model-names
  ([model-file-names] (get-model-names model-file-names []))
  ([model-file-names output]
   (if (empty? model-file-names)
     output
     (let [current-file (first model-file-names)]
       (get-model-names (rest model-file-names) (conj output (str (:name (mac/load-model current-file))
                                                                  " [" current-file "]")))))))
(defn model-name->file-name [model-name]
  (clojure.string/join "" (drop-last (second (clojure.string/split model-name #"\[")))))

(def existing-model-buttons
  (let [bg (button-group)]

    (flow-panel :items [(button :id "change-existing-model-folder-button" :class :type :group bg :mnemonic \f
                                :text "Change Model Folder" :enabled? false)
                        (button :id "train-start-button" :class :type :group bg :mnemonic \t
                                :text "Start Training"
                                :size [400 :by 31])])))

(def existing-model-menu
  (border-panel :north (styled-text :text "Select the Model you want to train."
                                    :wrap-lines? true :enabled? false
                                    :background transparent :foreground default-text-colour)
                :center (scrollable (listbox :model
                                             (get-model-names
                                               (map #(str %)
                                                    (drop 1 (-> model-path clojure.java.io/file file-seq sort))))))
                :south existing-model-buttons))

;; ---------------------------------------------------------------------------------------------------------------------
;; Main Training Menu

(defn reset-training-menu-buttons [e]
  (config! (select f [:#new-model-button]) :foreground default-text-colour)
  (config! (select f [:#train-model-button]) :foreground default-text-colour))

(declare training-menu)
(def training-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "new-model-button" :class :type :group bg :mnemonic \n
                                :text "Create New Model"
                                :listen [:action #(do (change-menu (training-menu create-new-model-menu))
                                                    (reset-training-menu-buttons %) (set-selected-colour %))])
                        (button :id "train-model-button" :class :type :group bg :mnemonic \t
                                :text "Train Existing Model"
                                :listen [:action #(do (change-menu (training-menu existing-model-menu))
                                                      (reset-training-menu-buttons %) (set-selected-colour %))])])))

(def training-menu-default
  (styled-text :text "Choose whether to create a new model or to train an existing model."
               :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour))

(defn training-menu
  ([] (training-menu training-menu-default))
  ([sub-menu] (border-panel :id "training-menu"
                            :north training-menu-buttons
                            :center (separator)
                            :south sub-menu)))


;; _____________________________________________________________________________________________________________________
;; Prediction Menu

;; ---------------------------------------------------------------------------------------------------------------------
;; Results Page

(defn confidence-bar [home-win? confidence]
  (if home-win?
    (flow-panel :items [(horizontal-panel :size [(- 272 (* confidence 2.72)) :by 10]
                                          :background (seesaw.color/color :black))
                        (horizontal-panel :size [(+ 272 (* confidence 2.72)) :by 10]
                                          :background sportseer-orange)])
    (flow-panel :items [(horizontal-panel :size [(+ 272 (* confidence 2.72)) :by 10]
                                          :background sportseer-orange)
                        (horizontal-panel :size [(- 272 (* confidence 2.72)) :by 10]
                                          :background (seesaw.color/color :black))])))

;; HERE! should check what gets returned from prediction

(defn results-node [results]
  (do (print results)
      (let [home-result (vertical-panel :items [(label :text "Home Win %") (label :text (* (:home-win results) 100))])
            away-result (vertical-panel :items [(label :text "Away Win %") (label :text (* (:away-win results) 100))])
            confidence-bar (confidence-bar (:home-win? results) (:confidence results))]
        (border-panel :west away-result :east home-result :south confidence-bar))))

(defn results-page [results]
  (if (= 1 (count results))
    (results-node (first results))
    (flatten (vector (results-node (first results)) (results-page (rest results))))))


;; ---------------------------------------------------------------------------------------------------------------------
;; Manual Prediction Menu

(defn get-manual-selection-nodes
  ([columns] (assoc {} :home (get-manual-selection-nodes columns "home" 0 [])
                       :away (get-manual-selection-nodes columns "away" 0 [])))
  ([columns side idx output]
   (let [result (get-manual-selection-nodes columns (str "#selector-" side "-" idx "-") {})]
     (if result
       (get-manual-selection-nodes columns side (inc idx) (conj output result))
       output)))
  ([columns id-prefix output]
   (if (empty? columns)
      output
      (let [current-column (first columns)
            ui-node (select f [(keyword (str id-prefix current-column))])]
        (if ui-node
          (get-manual-selection-nodes (rest columns) id-prefix
                                      (assoc output (keyword current-column) (selection ui-node)))
          nil)))))

(defn merge-manual-selections [home-data away-data columns]
  (let [column (keyword (first columns))]
    (if (= 1 (count columns))
      (assoc {} column (- (column home-data) (column away-data)))
      (merge (assoc {} column (- (column home-data) (column away-data)))
             (merge-manual-selections home-data away-data (rest columns))))))

(defn prepare-manual-selections
  ([columns]
   (let [selections (get-manual-selection-nodes columns)
         columns (drop-last columns)
         home-data (first (prepare-manual-selections (:home selections) columns))
         away-data (first (prepare-manual-selections (:away selections) columns))
         merged-data (merge-manual-selections home-data away-data columns)]
     (map #((keyword %) merged-data) columns)))
  ([selections columns]
   (if (= 1 (count selections))
     (map #(dissoc % :AVG) selections)
     (let [selections (dat/aggr-minutes (map #(dissoc (assoc % :min (:AVG %)) :AVG) selections))]
       (if (contains? columns "min") (map #(dissoc % :min) selections) selections)))))

(defn create-player-node [columns]
  (if (= 1 (count columns))
    (assoc {} (keyword (first columns)) 0.0)
    (merge (assoc {} (keyword (first columns)) 0.0) (create-player-node (rest columns)))))

(declare manual-selector)
(defn add-player [side columns]
  (let [nodes (get-manual-selection-nodes columns)]
    (assoc nodes side (conj (side nodes) (create-player-node columns)))))

(defn manual-selector-column
  ([values column side]
   (border-panel :north (label column)
                 :center (vertical-panel :items (manual-selector-column values column side 0 []))))
  ([values column side idx output]
   (if (empty? values)
     output
     (let [spinner (spinner :id (str "selector-" side "-" idx "-" column)
                            :model (spinner-model (float (first values)) :by 1.0))]
       (do (config! spinner :size [60 :by 25])
           (manual-selector-column (rest values) column side (inc idx)
                                   (conj output spinner)))))))

(defn manual-selector-columns [nodes side]
  (let [nodes (side nodes)
        side (name side)
        manual-selectors (map (fn [column] (manual-selector-column (map column nodes) (name column) side))
                              (keys (first nodes)))]
    (if (= 1 (count nodes))
      manual-selectors
      (cons (border-panel :north (label :text "#")
                          :center (vertical-panel :items (into [] (map #(label :text (str %) :size [10 :by 25])
                                                                       (range (count nodes))))))
            manual-selectors))))

(defn resize-selector-panel [panel rows]
  (if (<= rows 5)
    (resize! panel :height (+ 32 (* 25 rows)))
    (resize! panel :height 162)))

(defn manual-selector [nodes columns refresh-menu-fn]
  (let [away-selectors (scrollable (horizontal-panel :items (manual-selector-columns nodes :away)))
        home-selectors (scrollable (horizontal-panel :items (manual-selector-columns nodes :home)))]
    (do (resize-selector-panel away-selectors (count (:away nodes)))
        (resize-selector-panel home-selectors (count (:home nodes)))
      (border-panel
        :north (border-panel
                 :north (label "Away Team / Challenger(s)")
                 :center away-selectors
                 :south (button :text "Add Player/Challenger"
                                :listen [:action (fn [x] (refresh-menu-fn (add-player :away columns)))]))
        :south (border-panel
                 :north (label "Home Team / Defender(s)")
                 :center home-selectors
                 :south (button :text "Add Player/Defender"
                                :listen [:action (fn [x] (refresh-menu-fn (add-player :home columns)))]))))))

(declare prediction-menu)

(defn manual-prediction-menu
  ([model-name]
   (let [model-file-name (model-name->file-name model-name)
         model (mac/load-model model-file-name)
         columns (into [] (conj (into [] (:data-columns model)) "AVG"))]
     (manual-prediction-menu model columns
                             {:away [(create-player-node columns)]
                              :home [(create-player-node columns)]})))
  ([model columns selector-nodes]
   (let [refresh-menu-fn (fn [nodes] (change-menu (prediction-menu (manual-prediction-menu model columns nodes))))]
     (border-panel :north (border-panel :north (styled-text :text (str "The 'AVG' Column is only used when multiple rows "
                                                                       "are used for a team.\n"
                                                                       "Currently, only the Minutes Aggregation Algorithm "
                                                                       "is available, so enter the player's 'minutes played' "
                                                                       "statistic for a more accurate prediction.")
                                                            :wrap-lines? true :enabled? false
                                                            :background transparent :foreground default-text-colour)
                                        :south (separator))
                   :center (manual-selector selector-nodes columns refresh-menu-fn)
                   :south (button :text "Get Prediction"
                                  :listen
                                  [:action (fn [x]
                                             (let [selections (prepare-manual-selections columns)]
                                               (change-menu (prediction-menu
                                                              (results-page
                                                                (mac/make-prediction (:model model) selections))))))])
                   :vgap 5 :hgap 5))))

;; ---------------------------------------------------------------------------------------------------------------------
;; Bulk Prediction Menu

(defn bulk-prediction-menu [model-name]
  (let [model-file-name (model-name->file-name model-name)
        model (mac/load-model model-file-name)]
    (border-panel :center (scrollable
                            (listbox :id "stats-selector"
                                     :model (drop 1 (-> prediction-request-path clojure.java.io/file file-seq sort))))
                  :south (button :text "Get Prediction(s)"
                                 :listen
                                 [:action
                                  (fn [x] (let [csv-data (dat/import-csv (selection (select f [:#stats-selector])))
                                                aggr-fn (if (some #(= :min %) (keys (first csv-data)))
                                                          dat/aggr-minutes dat/aggr-average)
                                                combined-data (into [] (dat/combine-match-data
                                                                         (dat/aggregate-players csv-data aggr-fn)))]
                                            (change-menu
                                              (prediction-menu (results-page (mac/create-predictions combined-data model))))))]))))


;; ---------------------------------------------------------------------------------------------------------------------
;; Model Selection Menu

(defn prediction-model-select-menu [sub-menu-fn]
  (border-panel :north (styled-text :text "Select the Model you want to use for predictions."
                                    :wrap-lines? true :enabled? false
                                    :background transparent :foreground default-text-colour)
                :center (scrollable (listbox :id "model-selector"
                                             :model
                                             (get-model-names
                                               (map #(str %)
                                                    (drop 1 (-> model-path clojure.java.io/file file-seq sort))))))
                :south (button :text "Select Model"
                               :listen [:action
                                        (fn [x] (change-menu
                                                  (prediction-menu
                                                    (sub-menu-fn (selection (select f [:#model-selector]))))))])))

;; ---------------------------------------------------------------------------------------------------------------------
;; Main Prediction Menu

(defn reset-prediction-menu-buttons []
  (config! (select f [:#manual-prediction-button]) :foreground default-text-colour)
  (config! (select f [:#bulk-prediction-button]) :foreground default-text-colour))

(def prediction-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "manual-prediction-button" :class :type :group bg :mnemonic \n
                                :text "Manual Input"
                                :listen [:action #(do (change-menu
                                                        (prediction-menu
                                                          (prediction-model-select-menu manual-prediction-menu)))
                                                    (reset-prediction-menu-buttons) (set-selected-colour %))])
                        (button :id "bulk-prediction-button" :class :type :group bg :mnemonic \b
                                :text "Bulk Input"
                                :listen [:action #(do (change-menu
                                                        (prediction-menu
                                                          (prediction-model-select-menu bulk-prediction-menu)))
                                                    (reset-prediction-menu-buttons) (set-selected-colour %))])])))

(def prediction-menu-default
  (styled-text :text "Choose whether to input prediction requests manually or bulk import them from a .csv file."
               :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour))

(defn prediction-menu
  ([] (prediction-menu prediction-menu-default))
  ([sub-menu] (border-panel :id "prediction-menu"
                            :north prediction-menu-buttons
                            :center (separator)
                            :south sub-menu)))

;; _____________________________________________________________________________________________________________________
;; Main Menu Functionality

(defn reset-main-menu-buttons [e]
  ;this is optimal - but it dont work :@
  #_(map #(config! % :foreground default-text-colour) (select e [:<javax.swing.JButton>>]))
  (config! (select f [:#scrape-button]) :foreground default-text-colour)
  (config! (select f [:#dataset-button]) :foreground default-text-colour)
  (config! (select f [:#train-button]) :foreground default-text-colour)
  (config! (select f [:#predict-button]) :foreground default-text-colour))

(def main-menu-buttons
  (let [bg (button-group)]
    (flow-panel :items [(button :id "scrape-button" :class :type :group bg :mnemonic \s
                                :text "Data Scraper"
                                :listen [:action #(do (change-menu scraper-menu)
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "dataset-button" :class :type :group bg :mnemonic \d
                                :text "Datasets"
                                :listen [:action #(do (change-menu dataset-menu)
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "train-button" :class :type :group bg :mnemonic \m
                                :text "Models"
                                :listen [:action #(do (change-menu (training-menu))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])
                        (button :id "predict-button" :class :type :group bg :mnemonic \p
                                :text "Predictions"
                                :listen [:action #(do (change-menu (prediction-menu))
                                                      (reset-main-menu-buttons %) (set-selected-colour %))])])))

(def main-menu (border-panel :north (separator) :center main-menu-buttons :south (separator)))


(defn change-menu [content]
  (config! f :content (border-panel :north title-logo :center main-menu :south content
                                    :vgap 5 :hgap 5 :border 15))
  (-> f max-width-pack!))

(change-menu (styled-text :text (str "This is a client for creating and managing "
                                     "machine learning models"
                                     "designed to predict sports match outcomes.\n\n"
                                     "Created by Scott Taylor\n"
                                     "For my BSc Computer Science Final Year Project at Teesside University")
                          :wrap-lines? true :enabled? false :background transparent :foreground default-text-colour))
(-> f max-width-pack! show!)