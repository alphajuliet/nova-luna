(ns novaluna.policy
  (:require [novaluna.game :as game]
            [novaluna.state :as state]
            [novaluna.action :as act]))

(defn random-wheel-tile
  "Pick a random tile from the wheel"
  [state]
  (let [wheel (:wheel state)
        tile (first (drop-while nil? (shuffle wheel)))]
    (.indexOf wheel tile)))

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
