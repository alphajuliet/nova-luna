(ns novaluna.game
  (:require [novaluna.tiles :as tile]
            [novaluna.state :as st]
            [novaluna.action :as act]))

(defn load-tiles
  []
  (tile/read-tiles tile/tile-data))

(defn score
  "Calculate the score for a player's board"
  [state player]
  (->> (st/board state player)
       act/check-tiles
       flatten
       (filter true?)
       count))

(defn init-game
  [nplayers]
  (-> nplayers
      st/initial-state
      act/populate-wheel))

;; The End
