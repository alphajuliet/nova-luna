(ns novaluna.game
  (:require [novaluna.tiles :as tile]
            [novaluna.state :as state]
            [novaluna.action :as act]))

(defn load-tiles
  []
  (tile/read-tiles tile/tile-data))

(defn init-game
  [nplayers]
  (let [tiles (load-tiles)]
    (-> (state/initial-state nplayers tiles)
        (act/populate-wheel))))

;; The End
