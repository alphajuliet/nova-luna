(ns novaluna.game
  (:require [novaluna.tiles :as tile]
            [novaluna.state :as state]
            [novaluna.action :as act]))

(defn load-tiles
  []
  (tile/read-tiles tile/tile-data))

(defn score
  "Calculate the scope for a player's board"
  [state player]
  (let [board (get-in state [:player player :board])
        goals (act/check-tiles board)]
    (count goals)))

(defn init-game
  [nplayers]
  (-> nplayers
      state/initial-state
      act/populate-wheel))

;; The End
