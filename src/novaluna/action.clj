(ns novaluna.action
  (:require [novaluna.state :as state]))

(defn find-connected-color
  "Find connected tiles of the given colour"
  [board x y colour visited]
  (let [current-pos [x y]
        current-tile (get board current-pos)]
    (if (or (contains? visited current-pos)
            (nil? current-tile)
            (not= (:colour current-tile) colour))
      0
      (let [new-visited (conj visited current-pos)
            neighbours (filter #(contains? board %) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])]
        (inc (reduce +
                  (map #(find-connected-color board (first %) (second %) colour new-visited)
                       neighbours)))))))

(defn count-connected-color
  "Count adjacent tiles with the same colour"
  [board x y]
  (let [tile (get board [x y])]
    (find-connected-color board x y (:colour tile) #{})))


;; The End
