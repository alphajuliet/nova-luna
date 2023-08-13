(ns novaluna.state
  (:require [novaluna.tiles :as tile]))

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

;;---------------------------

(def init-board {})

(defn init-test-state
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
              colour (:colour tile)
              symbol (get colour-symbols colour "❌")]
          (print symbol " ")))
      (println))))

(defn go
  []
  (let [s0 (init-test-state)]
    (viz-state s0)))

;; The End
