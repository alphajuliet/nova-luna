(ns novaluna.game
  (:require [novaluna.tiles :as tile]
            [novaluna.state :as st]
            [novaluna.action :as act]))

(defn load-tiles
  []
  (tile/read-tiles tile/tile-data))

(defn score
  "Calculate the score for a player's board."
  [state player]
  (let [b (st/board state player)
        g (act/count-goals b)
        d (->> b
               act/find-goal-differences
               act/extract-numeric-values
               flatten
               (apply +))]
    (- d g)))

(defn init-game
  [nplayers]
  (-> nplayers
      st/initial-state
      act/populate-wheel))

;; The End
