(ns novaluna.state
  (:require [novaluna.tiles :as tile]
            [clojure.spec.alpha :as s]
            [spec-dict :refer [dict]]))

;;---------------------------

(defn compare-2d
  [v1 v2]
  (cond
    (= v1 v2) 0
    (< (first v1) (first v2)) -1
    (> (first v1) (first v2)) 1
    :else (if (< (second v1) (second v2)) -1 1)))

(defn sample
  "Take n samples without replacement"
  [n coll]
  (let [samples (shuffle coll)]
    (take n samples)))

(defn neighbours
  "Return the neighbours of a given coordinate"
  [[x y]]
  (vector [(dec x) y]
          [(inc x) y]
          [x (dec y)]
          [x (inc y)]))

;;---------------------------

(def init-board {})

(defn test-board
  "Initial board state for testing"
  []
  (let [tiles (tile/read-tiles tile/tile-data)
        subset (sample 16 tiles)  ; does not handle repetitions
        coords (for [x (range 4)
                     y (range 4)]
                 [x y])]
    (zipmap coords subset)))

(defn viz-state
  "Show the colours at each position on a board"
  [board]
  (let [colour-symbols {:yellow "🟨", :blue "🟦", :cyan "🟩" :red "🟥"}
        max-x (apply max (map #(first (first %)) board))
        max-y (apply max (map #(second (first %)) board))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (let [tile (get board [x y])
              symbol (get colour-symbols (:colour tile) "❌")]
          (print symbol " ")))
      (println))))

(defn viz-wheel
  "Show a simplified representation of what's in the wheel"
  [state]
  (let [colour-symbols {:yellow "🟨", :blue "🟦", :cyan "🟩" :red "🟥"}
        wheel (:wheel state)]
    (doseq [i (range 12)]
      (let [tile (nth wheel i)
            symbol (get colour-symbols (:colour tile) "❌")]
        (print (:cost tile) symbol " ")))
    (println)))

;;---------------------------
;; The game state...
;; - board: an array of boards that contain tiles
;; - stack: the unplayed tiles
;; - wheel: the moon wheel containing available tiles
;; - track: the moon track of player positions
;; - meeple: the current position (0-11)

(s/def ::colour #{:red :yellow :cyan :blue})
(s/def ::goal (s/map-of ::colour pos-int?))
(s/def ::tile (dict {:cost pos-int?
                     :colour ::colour}
                    ^:opt {:goals (s/coll-of ::goal)}))
(s/def ::coord (s/tuple int? int?))
(s/def ::board (s/map-of ::coord ::tile))
(s/def ::player (dict {:board ::board
                       :track nat-int?}))
(s/def ::nat-int-or-nil (s/or :nat nat-int? :nil nil?))
(s/def ::state (dict {:player (s/coll-of ::player)
                      :stack (s/coll-of ::tile :into ())
                      :wheel (s/coll-of ::nat-int-or-nil)
                      :meeple nat-int?}))

(defn initial-state
  "Set up the initial state before any tiles are dealt into the wheel"
  [nplayers tiles]
  {:post [(s/valid? ::state %)]}
  {:player (vec (repeat nplayers {:board init-board :track 0}))
   :stack (shuffle tiles) ; stack of tiles
   :wheel (vec (repeat 12 nil))
   :meeple 0})

(defn go
  []
  (let [s0 (test-board)]
    (viz-state s0)))

;; The End
