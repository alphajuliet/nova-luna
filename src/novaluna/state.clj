(ns novaluna.state
  (:require [instaparse.core :as insta]
            [novaluna.tiles :as tile]
            [clojure.edn :as edn]))

;;---------------------------
(defn map-kv
  "Map f over the values of a map."
  [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

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
