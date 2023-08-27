(ns novaluna.action
  (:require [novaluna.state :as st]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(defn dfs-count
  "Do a DFS to find all connected tiles of the given colour surrounding the start coordinate"
  [board start-xy colour visited]
  (let [adj-tiles (st/neighbours start-xy)]
    (apply + (map (fn [adj-tile]
                   (if (or (visited adj-tile) (nil? (get board adj-tile)))
                     0
                     (let [tile-colour (:colour (get board adj-tile))]
                       (if (= tile-colour colour)
                         (let [visited (conj visited adj-tile)]
                           (reduce + 1 (map (fn [next-adj-tile] (dfs-count board next-adj-tile colour visited))
                                          (st/neighbours adj-tile))))
                         0))))
                 adj-tiles))))

(defn compare-goals-to-actual
  [board xy]
  (let [tile (get board xy)
        goals (:goals tile)]
    (map (fn [goal]
           (reduce (fn [acc [color count]]
                     (merge acc {color (max 0 (- count (dfs-count board xy color #{})))}))
                   {}
                   goal))
         goals)))

(defn find-goal-differences
  "Find the goal differences across all tiles"
  [board]
  ;; {:pre [(s/valid? ::st/board board)]}
  (reduce (fn [acc [coord _]]
            (assoc acc coord (compare-goals-to-actual board coord)))
          {}
          board))

(defn extract-numeric-values
  [m]
  (->> m
       vals
       (map (fn [v]
          (map (fn [inner-map]
                 (vals inner-map))
               v)))
       (apply concat)))

(defn sum-of-goal-differences
  [board]
  (->> board
       find-goal-differences
       extract-numeric-values
       flatten
       (apply +)))

(defn count-goals
  "Count how many goals are satisfied"
  [board]
  (->> board
      find-goal-differences
      extract-numeric-values
      (map #(apply + %))
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
  (keep-indexed (fn [i elem]
                  (when (nil? elem)
                    i))
                coll))

(defn populate-wheel
  "Fill empty positions in the wheel from the stack, except for the meeple"
  [state]
  ;; {:pre [(s/valid? ::st/state state)]}
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

(defn refresh-wheel
  "Refresh the wheel if required"
  [state]
  (if (<= (count (remove nil? (:wheel state))) 2)
    (populate-wheel state)
    ;; else
    state))

;;---------------------------
;; Play tiles

(defn available-spaces
  "Returns the set of available coordinates to play a tile, i.e. the perimeter"
  [state player]
  (let [board (get-in state [:player player :board])
        coords (keys board)]
    (if (empty? coords)
      #{[0 0]}
      (let [neighbour-sets (map (comp set st/neighbours) coords)
            all-neighbours (apply set/union neighbour-sets)]
        (set/difference all-neighbours (set coords))))))

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
  ;; {:pre [(s/valid? ::st/state state)]}
  (let [tile (get-in state [:wheel wheel-pos])]
    (if (legal-play? state player xy)
      (-> state
         (assoc-in [:wheel wheel-pos] nil)
         (assoc-in [:player player :board xy] tile)
         (assoc :meeple wheel-pos)
         (update-in [:player player :track] #(+ % (:cost tile)))
         refresh-wheel)
      ;; else
      state)))

;; The End
