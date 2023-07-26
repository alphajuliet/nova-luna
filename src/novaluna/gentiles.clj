(ns novaluna.gentiles
  (:require [dali.io :as io]
            ;; [clojure.edn :as edn]
            [instaparse.core :as insta]))

(def parse-tile-data
  "Parse the tile list"
  (insta/parser
   "Set     := line+
    <line>  := tile | <newline>
    tile    := points <space> colour goals?
    <goals> := (<space> | goal)*
    points  := #'\\d'
    colour  := '1' | '2' | '3' | '4'
    goal    := <'['> (colour|<space>)+ <']'>
    space   := #'\\s+'
    newline := '\n'"))

(defn read-tiles
  "Read in the tiles"
  [f]
  (-> f
      slurp
      parse-tile-data))

(defn translate-colour
  "Turn numbers into colours"
  [x]
  (case x
    "1" :cyan
    "2" :yellow
    "3" :red
    "4" :blue
    :error))

(defn create-number
  [x]
  [:text x {:stroke :black :font-size 30}])

(defn create-goal
  "Create a goal"
  [& xs]
  (let [v [:dali/align {:axis :center}]]
    (into v (map #(vector :circle [0 0] 10 {:stroke :black, :stroke-width 1, :fill %}) xs))))

(defn create-tile
  "Create a coloured tile containing a number of points, and 0-3 goals"
  [& xs]
  (let [[points colour goals] xs
        base [:rectangle [0 0] [50 50] {:stroke colour}]
        t [:dali/stack]]
    (-> t
        (conj base)
        (conj points)
        (conj goals))))

(defn create-set
  "Top-level structure"
  [& xs]
  (into [:dali/page] xs))

(defn generate-svg
  "Walk the tree and convert to SVG"
  [tree]
  (insta/transform
   {:Set create-set
    :tile create-tile
    :goal create-goal
    :colour translate-colour
    :points create-number}
   tree))

(defn gen-tiles
  "Generate the set of tiles as SVG"
  []
  (->> "data/tiles.txt"
       read-tiles
       generate-svg))

(def document
  [:dali/page
   [:circle
    {:stroke :indigo :stroke-width 4 :fill :darkorange}
    [30 30] 20]])

(defn -main
  [_]
  (println "Tiles"))

;; The End
