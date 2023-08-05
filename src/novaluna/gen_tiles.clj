(ns novaluna.gen-tiles
  (:require [dali.io :as io]
            [dali.layout.stack]
            [instaparse.core :as insta]))

(defn third [lst] (nth lst 2))
(defn fourth [lst] (nth lst 3))

(def parse-tile-data
  "Parse the tile list"
  (insta/parser
   "Set     := line+
    <line>  := tile | <newline>
    tile    := points <space> colour goals?
    goals := (<space> | goal)*
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
    "2" :gold
    "3" :red
    "4" :cornflowerblue
    :white))

(defn create-number
  [x]
  [:g
   [:circle {:fill :white} [0 0] 25]
   [:text {:stroke :black, :font-size "30pt", :fill :grey :x -10 :y 13} x]])

(defn create-goal
  "Create a goal with 1-4 colours"
  [& xs]
  (let [c0 [:circle {:fill :white} [0 0] 25]
        v [:g c0]]
    (case (count xs)
      1 (conj v [:circle {:fill xs} [0 0] 7.5 ])
      2 (into v [[:circle {:fill (first xs)}  [-7.5 -7.5] 7.5]
                 [:circle {:fill (second xs)} [ 7.5  7.5] 7.5]])
      3 (into v [[:circle {:fill (first xs)}  [  0 -10] 7.5]
                 [:circle {:fill (second xs)} [-10 7.5] 7.5]
                 [:circle {:fill (third xs)}  [ 10 7.5] 7.5]])
      4 (into v [[:circle {:fill (first xs)}  [-10 -10] 7.5]
                 [:circle {:fill (second xs)} [-10  10] 7.5]
                 [:circle {:fill (third xs)}  [ 10 -10] 7.5]
                 [:circle {:fill (fourth xs)} [ 10  10] 7.5]])
      v)))

(defn create-goals
  [& goals]
  (into [:g] (mapv (fn [xy g] (vector :g {:transform [:translate xy]} g))
                   (list [87 33] [33 87] [87 87])
                   goals)))

(defn create-tile
  "Create a coloured tile containing a number of points, and 0-3 goals"
  [& xs]
  (let [[points colour goals] xs
        base [:rect {:stroke :black, :stroke-width "1px" :fill colour} [0 0] [120 120] 20]
        points' [:g {:transform [:translate [33 33]]} points]]
    [:g base points' goals]))

(defn create-set
  "Top-level structure"
  [& tiles]
  (conj [:dali/page]
        (into [:dali/stack {:direction :right :gap 10}]
              tiles)))

(defn render-set!
  [& tiles]
  (doseq [t tiles]
    (io/render-png [:dali/page t] (str "data/" (java.util.UUID/randomUUID) ".png")))
  nil)

(defn generate-svg
  "Walk the tree and convert to SVG"
  [tree]
  (insta/transform
   {:Set create-set
    :tile create-tile
    :goals create-goals
    :goal create-goal
    :colour translate-colour
    :points create-number}
   tree))

(defn generate-pngs
  [tree]
  (insta/transform
   {:Set render-set!
    :tile create-tile
    :goals create-goals
    :goal create-goal
    :colour translate-colour
    :points create-number}
   tree))

(defn gen-all-tiles
  "Generate the set of tiles as SVG"
  []
  (-> "data/tiles.txt"
      read-tiles
      generate-svg
      (io/render-svg "data/tiles.svg")))

(defn render-all-tiles
  []
  (-> "data/tiles.txt"
      read-tiles
      generate-pngs
      render-set!))

(defn -main
  [_]
  (println "Tiles"))

;; The End
