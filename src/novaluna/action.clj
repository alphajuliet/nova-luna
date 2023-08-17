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

#_(defn count-connected-color
  "Count adjacent tiles with the same colour"
  [board x y]
  (let [tile (get board [x y])]
    (find-connected-color board x y (:colour tile) #{})))

(defn check-goal
  "Check a goal for a given tile"
  [board xy g]
  (let [[x y] xy]
    (->> g
         :goal
         (map (fn [[colour target]]
                (>= (find-connected-color board x y colour #{}) target)))
         (every? true?))))

(defn check-tile-goals
  "Check each of the goals for a tile"
  [board xy]
  (let [tile (get board xy)]
    (if (nil? tile)
      nil
      ;; else
      (map #(check-goal board xy %) (:goals tile)))))

(defn check-tiles
  "Check all goals across the board"
  [board]
  (map (fn [[xy tile]] (check-tile-goals board xy)) board))

;;---------------------------
;; Wheel actions

(defn deal-tile
  "Deal one tile from the stack to an empty slot in the wheel"
  [state posn]
  (if (nil? (nth (:wheel state) posn))
    (let [tile (first (:stack state))]
      (-> state
          (update :stack rest)
          (update :wheel #(assoc % posn tile))))
    state))

(defn nil-elements
  "Return the positions of nil elements in a collection"
  [coll]
  (for [i (range (count coll))
        :when (nil? (nth coll i))]
    i))

(defn populate-wheel
  "Fill empty positions in the wheel from the stack, except for the meeple"
  [state]
  (let [empty-slots (nil-elements (:wheel state))
        meeple-posn (:meeple state)
        state' (reduce deal-tile state empty-slots)]
    (assoc-in state' [:wheel meeple-posn] nil)))

;; The End
