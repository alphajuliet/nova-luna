(ns novaluna.state
  (:require [instaparse.core :as insta]
            [novaluna.gen-tiles :as tiles]
            [clojure.edn :as edn]))

;;---------------------------

(defn third [coll] (nth coll 2))

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
  [n coll]
  (reduce (fn [acc _] (conj acc (rand-nth coll)))
          []
          (range n)))

;;---------------------------

(defn translate-colour
  "Turn numbers into colours"
  [x]
  (case x
    "1" :cyan
    "2" :yellow
    "3" :red
    "4" :blue
    :error))

(defn create-points [p] {:cost (edn/read-string p)})
(defn create-goal [& g] {:goal (vec g) :won false})
(defn create-goals [& g] {:goals (vec g)})
(defn create-tile [& t] (-> {}
                            (into (first t))
                            (into {:colour (second t)})
                            (into (if (> (count t) 2) (third t) {}))))

(defn create-tiles
  "Walk the tree and convert to a Clojure data structure"
  [tree]
  (insta/transform
   {:tile create-tile
    :goals create-goals
    :goal create-goal
    :points create-points
    :colour translate-colour}
   tree))

(def tile-data "data/tiles.txt")

(defn read-tiles
  "Read in all 69 tiles"
  [f]
  (-> f
      tiles/read-tiles
      create-tiles))

;;---------------------------

(def init-board {})

(defn find-connected-color
  "Find connected tiles of the given colour"
  [board x y colour]
  (let [current-tile (get board [x y])
        neighbours (filter #(contains? board %) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])]
    (if (or (nil? current-tile)
            (not= (:colour current-tile) colour))
      0
      ;; else
      (let [new-board (dissoc board [x y])]
        (inc (reduce
              +
              (map #(find-connected-color new-board (first %) (second %) colour)
                   neighbours)))))))

(defn count-connected-color
  "Count adjacent tiles with the same colour"
  [board x y]
  (let [tile (get board [x y])]
    (find-connected-color board x y (:colour tile))))


(defn init-test-state
  "Initial board state for testing"
  []
  (let [tiles (read-tiles tile-data)
        subset (sample 16 tiles)  ; does not handle repetitions
        coords (for [x (range 4)
                     y (range 4)]
                 [x y])]
    (zipmap coords subset)))

(defn viz-state
  "Show the colours at each position on a board"
  [board]
  (let [color-symbols {:yellow "🟨", :blue "🟦", :cyan "🟩" :red "🟥"}
        max-x (apply max (map #(first (first %)) board))
        max-y (apply max (map #(second (first %)) board))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (let [tile (get board [x y])
              colour (:colour tile)
              symbol (get color-symbols colour " ")]
          (print symbol " ")))
      (println))))

;; The End
