(ns worldgeneration.core
  (:require [worldgeneration.world-generation :as wg])
  (:require [seesaw.core :as seesaw])
  (:require [clojure.pprint :refer [pprint]]))

(defn get-tile [grid x y]
  (get-in grid [x y]))

(defn set-tile [grid x y value]
  (assoc-in grid [x y] value))

(defn tile-to-text [tile]
  (get wg/tile-text-map tile "?"))


(defn chunk-to-text [chunk]
  (map (fn [row]
         (apply str (map (fn [tile]
                           (let [text (tile-to-text tile)]
                             (str text (apply str " "))))
                         row)))
       chunk))

(defn print-chunk [chunk]
  (apply str (interpose "\n" (map #(apply str %) (chunk-to-text chunk)))))

(defn create-popup []
  (let [frame       (seesaw/frame :title "World Generation"
                                  :on-close :exit
                                  :minimum-size [1000 :by 1000])
        text-area   (seesaw/text :multi-line? true
                                 :editable? false
                                 :text "Generated chunk will appear here.")
        seed-text   (seesaw/label :text "World Seed:")
        seed-slider (seesaw/slider :id :seed-slider
                                   :min 1
                                   :max 100
                                   :major-tick-spacing 10
                                   :minor-tick-spacing 1
                                   :snap-to-ticks? true
                                   :value 1)
        button      (seesaw/button :text "Generate Chunk"
                                   :listen [:action (fn [e]
                                                      (let [worldseed    (seesaw/value seed-slider)
                                                            chunk-width  50
                                                            chunk-height 50
                                                            empty-atom-chunk  (atom (wg/empty-chunk chunk-width chunk-height))
                                                            chunk        (wg/generate-chunk empty-atom-chunk worldseed 0 0 chunk-width chunk-height)
                                                            chunk-str    (print-chunk chunk)]
                                                        (seesaw/config! text-area :text chunk-str :font "Monospaced")))])]
    (seesaw/config! frame
                    :content (seesaw/border-panel :north (seesaw/horizontal-panel :items [seed-text seed-slider])
                                                  :center (seesaw/border-panel :north button
                                                                               :center text-area)))
    (seesaw/invoke-later
     (seesaw/pack! frame)
     (seesaw/show! frame))))



                        
(defn -main []
  (create-popup))

;; (defn -main [& args]
;;   (let [chunk-width  50
;;         chunk-height 50
;;         empty-chunk  (wg/empty-chunk chunk-width chunk-height)
;;         chunk        (wg/generate-chunk empty-chunk 1 0 0 chunk-width chunk-height)]
;;     (pprint chunk)))