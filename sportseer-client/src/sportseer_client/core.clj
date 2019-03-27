(ns sportseer-client.core
  ;(:import (javax.swing JOptionPane JButton JPanel JFrame JComponent)
  ;         (java.awt.event ActionListener)
  ;         (java.awt GridLayout Graphics GridBagLayout)
  ;         (javax.imageio ImageIO))
  (:require [seesaw.core])
  (:import (javax.imageio ImageIO)))

;(defn say-hello []
;  (JOptionPane/showMessageDialog
;    nil "Hello, World!" "Greeting" JOptionPane/INFORMATION_MESSAGE))
;
;(def act (proxy [ActionListener] []
;           (actionPerformed [event] (say-hello))))
;
;(def button (doto (JButton. "Click Me!")
;              (.addActionListener act)))
;
;(def logo-panel (proxy [JPanel] []
;                  (paintComponent [g]
;                    (proxy-super paintComponent g)
;                    (.drawImage g (ImageIO/read (clojure.java.io/file "sportseer-logo-text.png")) 0 0 552 93 nil))))
;
;(def panel (doto (JPanel.)
;             (.setLayout (GridBagLayout.))
;             (.add logo-panel)
;             (.add button)))
;(def frame (doto (JFrame. "SportSeer")
;             (.setSize 800 800)
;             (.setContentPane panel)
;             (.setVisible true)))

(native!)
(def f (frame :title "SportSeer" :resizable? false
              :icon (seesaw.icon/icon (ImageIO/read (clojure.java.io/file "sportseer-logo.png")))))
(config! f :size [800 :by 800])

(defn display [content]
  (config! f :content content)
  content)

(def seasons-list ["NBA Season 2010-2011", "NBA Season 2011-2012", "NBA Season 2012-2013",
                   "NBA Season 2013-2014", "NBA Season 2014-2015", "NBA Season 2015-2016",
                   "NBA Season 2016-2017", "NBA Season 2017-2019 (incomplete)"])

(def lb (listbox :model (-> seasons-list sort)))
(selection lb {:multi? true})
(display (scrollable lb))

(def rbs (for [i [:source :doc]]
           (radio :id i :class :type :text (name i))))

(def field (display (text "This is a text field")))

(def title-logo
  (label :icon (seesaw.icon/icon (ImageIO/read (clojure.java.io/file "sportseer-logo-text-half.png")))
         :size [552 :by 94]))

(def season-selection (border-panel
                        :north (border-panel
                                 :north (label "Model Training")
                                 :center (separator)
                                 :vgap 5)
                        :center (border-panel
                                  :north (border-panel
                                           :west (button :text "Scrape More")
                                           :east (button :text "Select Data Folder"))
                                  :south (scrollable lb)
                                  :vgap 3)
                        :south (button :text "Train Model")
                        :vgap 5 :hgap 5))

(def main-panel (border-panel :west season-selection :east field))

(def container (border-panel :north title-logo :south main-panel
                             :vgap 5 :hgap 5 :border 10))



(display container)

(-> f pack! show!)