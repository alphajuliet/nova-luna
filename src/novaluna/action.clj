(ns novaluna.action
  (:require [novaluna.state :as st]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defn find-connected-color
  "Find connected tiles of the given colour"
  [board x y colour visited]
  (let [current-pos [x y]
        current-tile (get board current-pos)]
    (if (or (contains? visited current-pos)
            (nil? current-tile)
            (not= (:colour current-tile) colour))
      0
      ;; else
      (let [new-visited (conj visited current-pos)
            neighbours (filter #(contains? board %) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])]
        (inc (reduce +
                  (map #(find-connected-color board (first %) (second %) colour new-visited)
                       neighbours)))))))

(defn check-goal
  "Check the difference to a goal for the tile at xy. A goal is a key-value pair of colour and count."
  [board xy goal]
  (let [[x y] xy]
    (->> goal
         (map (fn [[colour number]]
                (- number (find-connected-color board x y colour #{}))))
         (apply +))))

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

(defn count-goals
  "Count how many goals are satisfied"
  [board]
  (->> board
      check-tiles
      flatten
      (filter zero?)
      count))

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
  {:pre [(s/valid? ::st/state state)]}
  (let [empty-slots (nil-elements (:wheel state))
        meeple-posn (:meeple state)
        state' (reduce deal-tile state empty-slots)]
    (assoc-in state' [:wheel meeple-posn] nil)))

(defn eligible-tiles
  "List the eligible tile positions to play from the wheel"
  [{:keys [wheel meeple]}]
  (let [next-posns (map #(mod (+ % meeple) 12) (range 12))]
    (->> next-posns
         (remove #(nil? (nth wheel %)))
         (take 3))))

;;---------------------------
;; Play tiles

(defn has-neighbour?
  "Test if a location is next to an existing tile"
  [board [x y]]
  (or (contains? board [(dec x) y])
      (contains? board [(inc x) y])
      (contains? board [x (inc y)])
      (contains? board [x (dec y)])))

(defn available-spaces
  "Returns the set of available coordinates to play a tile, i.e. the perimeter"
  [state player]
  (let [board (get-in state [:player player :board])
        coords (keys board)]
    (if (empty? coords)
      #{[0 0]}
      ;; else
      (as-> coords <>
       (for [xy <>]
         (st/neighbours xy))
       (apply concat <>)
       (set <>)
       (set/difference <> (set coords))))))

(defn legal-play?
  "Is it legal to play a tile at the given coordinate?
   Must be both unoccupied and adjacent to an existing tile, or just the first tile."
  [state player xy]
  (let [avail (available-spaces state player)]
    (if (empty? avail)
      true
      ;; else
      (contains? avail xy))))

(defn play-tile
  "Play a tile from the wheel to the board, in a legal position."
  [state player wheel-pos xy]
  (let [tile (get-in state [:wheel wheel-pos])]
    (if (legal-play? state player xy)
      (-> state
         (assoc-in [:wheel wheel-pos] nil)
         (assoc-in [:player player :board xy] tile)
         (assoc :meeple wheel-pos)
         (update-in [:player player :track] #(+ % (:cost tile))))
      ;; else
      state)))


;; The End
