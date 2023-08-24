(ns novaluna.policy
  (:require [novaluna.game :as game]
            [novaluna.state :as st]
            [novaluna.action :as act]))

(defn random-element
  "Choose a random element from a collection"
  [coll]
  (first (shuffle coll)))

(defn random-wheel-tile
  "Pick a random eligible tile from the wheel"
  [state]
  (random-element (act/eligible-tiles state)))

(defn play-random-tile
  [state player]
  (let [tile (random-wheel-tile state)
        pos (first (shuffle (act/available-spaces state player)))]
    (act/play-tile state player tile pos)))

(defn play-n-random-tiles
  [state player n]
  (reduce (fn [st _] (play-random-tile st player))
          state
          (range n)))

;; The End
