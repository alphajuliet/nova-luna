(ns novaluna.tiles
  (:require [instaparse.core :as insta]
            [novaluna.gen-tiles :as gen]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]))

(defn third [coll] (nth coll 2))

(defn translate-colour
  "Turn numbers into colours"
  [x]
  (case x
    "1" :cyan
    "2" :yellow
    "3" :red
    "4" :blue
    (throw (ex-info "Invalid colour code" {:code x}))))

(defn create-points [p] {:cost (edn/read-string p)})
(defn create-goal [& g] (frequencies g))
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
      gen/read-tiles
      create-tiles))

(defn write-tiles
  "Pretty print the tiles to a file"
  [f tiles]
  (with-open [writer (java.io.FileWriter. f)]
   (pp/pprint tiles writer)))

;; The End
