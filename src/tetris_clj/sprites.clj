(ns tetris-clj.sprites
  (:require [quil.core :as q]))

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

(defn draw-tile [x y tile-map id tile-size]
  (let [img (id tile-map)]
    (when (q/loaded? img)
      (q/image img (* x tile-size) (* y tile-size)))))

(defn draw-tiles [coordinates id tile-map tile-size]
  (doseq [[x y] coordinates]
    (draw-tile x y tile-map id tile-size)))
