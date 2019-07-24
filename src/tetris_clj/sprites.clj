(ns tetris-clj.sprites
  (:require [quil.core :as q]))

(defn- translate-color [RGB-values]
  (case RGB-values
    :white [255 255 255]
    :pink [200 127 180]
    :red [220 20 60]
    :slate [112 128 144]
    :beige [245 245 220]
    :dark-gray [169 169 169]
    :silver [192 192 192]
    :light-gray [211 211 211]
    :sky-blue [0 191 255]
    :lavender [230 230 250]
    :yellow [255 250 205]
    :forest-green [34 139 34]
    RGB-values))

(defn- get-offset [column-number spacing-size]
  (* column-number spacing-size))

(defn- get-start [number tile-size spacing-size]
  (+ (* tile-size number) (get-offset number spacing-size)))

(defn- get-tile [source-image
                 column-number
                 row-number
                 {:keys [tile-size column-spacing-size row-spacing-size] :as args}]
  (let [col-start (get-start column-number tile-size column-spacing-size)
        row-start (get-start row-number tile-size row-spacing-size)
        img (q/create-image tile-size tile-size :rgb)]
    (q/copy source-image img [col-start row-start tile-size tile-size] [0 0 tile-size tile-size])
    img))

(defn- get-tile-row
  ([image
    row-number
    {:keys [tile-size columns column-spacing-size row-spacing-size] :as args}
    accumulator
    counter]
   (let [tile-id (-> row-number
                     (* columns)
                     (+ counter)
                     (str)
                     (keyword))
         tile (get-tile image counter row-number args)
         accumulator (assoc accumulator tile-id tile)]
     (if (= counter (dec columns))
       accumulator
       (get-tile-row image row-number args accumulator (inc counter)))))
  ([image
    row-number
    {:keys [tile-size columns column-spacing-size row-spacing-size] :as args}]
   (get-tile-row image row-number args {} 0)))

(defn load-tile-map [{:keys [filename tile-size columns rows column-spacing-size row-spacing-size] :as args}]
  (let [source-image (q/load-image filename)]
    (while (not (q/loaded? source-image))
      nil)
    (->> (range rows)
         (map #(get-tile-row source-image % args))
         (into {}))))

(defn- draw-image [x y img tile-size]
  (when (q/loaded? img)
    (q/image img (* x tile-size) (* y tile-size))))

(defn- translate-color [RGB-values]
  (case RGB-values
    :white [255 255 255]
    :pink [200 127 180]
    :slate [112 128 144]
    :beige [245 245 220]
    :dark-gray [169 169 169]
    :silver [192 192 192]
    :light-gray [211 211 211]
    :sky-blue [0 191 255]
    :lavender [230 230 250]
    :forest-green [34 139 34]
    :purple [106 90 205]
    :orange [255 165 0]
    :green [50 205 50]
    :red [220 20 60]
    :yellow [255 250 205]
    RGB-values))

(defn character-to-id [id]
  (let [id (clojure.string/lower-case id)]
    (case id
      "a" :979
      "b" :980
      "c" :981
      "d" :982
      "e" :983
      "f" :984
      "g" :985
      "h" :986
      "i" :987
      "j" :988
      "k" :989
      "l" :990
      "m" :991
      "n" :1011
      "o" :1012
      "p" :1013
      "q" :1014
      "r" :1015
      "s" :1016
      "t" :1017
      "u" :1018
      "v" :1019
      "w" :1020
      "x" :1021
      "y" :1022
      "z" :1023

      "0" :947
      "1" :948
      "2" :949
      "3" :950
      "4" :951
      "5" :952
      "6" :953
      "7" :954
      "8" :955
      "9" :956
      ":" :957
      "." :958
      "%" :959

      "?" :821
      "!" :819
      " " :0
      :821)))

(defn draw-tile
  ([[x y] tile-map id tile-size]
   (let [img (id tile-map)]
     (when (q/loaded? img)
       (q/image img (* x tile-size) (* y tile-size)))))
  ([coordinate tile-map id tile-size color]
   (let [[r g b] (translate-color color)]
     (q/tint r g b)
     (draw-tile coordinate tile-map id tile-size)
     (q/no-tint))))

(defn draw-tiles
  ([coordinates tile-map id tile-size]
   (doseq [coordinate coordinates]
     (draw-tile coordinate tile-map id tile-size)))
  ([coordinates tile-map id tile-size color]
   (doseq [coordinate coordinates]
     (draw-tile coordinate tile-map id tile-size color))))

(defn draw-word-rec
  ([[x y] tile-map ids]
   (when (not (empty? ids))
     (do
       (draw-tile [x y] tile-map (first ids) 16)
       (draw-word-rec [(inc x) y] tile-map (rest ids)))))
  ([[x y] tile-map color ids]
   (when (not (empty? ids))
     (do
       (draw-tile [x y] tile-map (first ids) 16 color)
       (draw-word-rec [(inc x) y] tile-map color (rest ids))))))

(defn draw-text
  ([x y tile-map word]
   (->> word
        (map str)
        (map character-to-id)
        (draw-word-rec [x y] tile-map)))
  ([x y tile-map word color]
   (->> word
        (map str)
        (map character-to-id)
        (draw-word-rec [x y] tile-map color))))

(defn- text-center-start [container-width text]
  (let [container-mid (int (/ container-width 2))
        text-mid (int (/ (count text) 2))
        text-start (- container-mid text-mid)]
    (if (neg? text-start) 0
        text-start)))

(defn draw-text-centered [y container-width tile-map text]
  (-> (text-center-start container-width text)
      (draw-text y tile-map text)))

(defn load-tiles []
  (load-tile-map
   {:filename "resources/monochrome.png"
    :tile-size 16
    :columns 32
    :rows 32
    :column-spacing-size 1
    :row-spacing-size 1}))
