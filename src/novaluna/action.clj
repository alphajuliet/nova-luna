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
  (map (fn [[xy _]] (check-tile-goals board xy)) board))

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

;;---------------------------
;; Play tiles

(defn has-neighbour?
  "Test if a location is next to an existing tile"
  [board [x y]]
  (or (contains? board [(dec x) y])
      (contains? board [(inc x) y])
      (contains? board [x (inc y)])
      (contains? board [x (dec y)])))

(defn legal-play?
  "Is it legal to play a tile at the given coordinate?
   Must be both unoccupied and adjacent to an existing tile, or just the first tile."
  [state player xy]
  (let [board (get-in state [:player player :board])]
    (if (empty? board)
      true
      ;; else
      (and (not (contains? board xy))
           (has-neighbour? board xy)))))

(defn play-tile
  "Play a tile from the wheel to the board, in a legal position."
  [state player wheel-pos xy]
  (let [tile (get-in state [:wheel wheel-pos])]
    (if (legal-play? state player xy)
      (-> state
         (assoc-in [:wheel wheel-pos] nil)
         (assoc-in [:player player :board xy] tile))
      ;; else
      state)))


;; The End
