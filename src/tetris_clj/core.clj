(ns tetris-clj.core
    (:use
      [tetris-clj.sprites :only [load-tile-map draw-tile]]
      [tetris-clj.utils :only [in? add-vectors inbounds?]])
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(defn setup []
  (q/background 0)
  (q/frame-rate 30)
  {:game
   {:last-tick (System/currentTimeMillis)
    :tick-length 1000 ;;length in miliseconds
    :size [16 16]
    :frozen []
    :tile-map (load-tile-map
                {:filename "resources/monochrome.png"
                 :tile-size 16
                 :columns 32
                 :rows 32
                 :column-spacing-size 1
                 :row-spacing-size 1})}})

(def tetrominoes
  {:line {:tile-id :2 :offsets [[0 0] [1 0 ] [2 0] [3 0]]}
   :l {:tile-id :2 :offsets [[0 0] [0 -1] [0 -2] [1 -2]]}
   :s {:tile-id :2 :offsets [[0 0] [0 -1] [1 -1] [2 -1]]}
   :z {:tile-id :2 :offsets [[0 0] [0 -1] [1 0] [1 1]]}
   :square {:tile-id :2 :offsets [[0 0] [1 0] [0 -1] [1 -1]]}
   :t {:tile-id :2 :offsets [[0 0] [1 0] [2 0] [1 -1]]}})

(def directions
  {:n [0 -1]
   :s [0 1]
   :w [-1 0]
   :e [1 0]})


(defn spawn-tetromino [state]
  (if (nil? (get-in state [:game :active-tetromino]))
    (assoc-in state [:game :active-tetromino]
              {:position [1 1]
               :tetromino (:square tetrominoes)})
    state)) 

(defn- move-tetromino [state dir]
   (let [tetromino (get-in state [:game :active-tetromino])
         position (get-in tetromino [:position])
         positions (map #(add-vectors position %) (:offsets (:tetromino tetromino)))
         dir-vec (dir directions)
         new-positions (map #(add-vectors dir-vec %) positions)
         valid (every? true? (map #(inbounds? (get-in state [:game :size]) %) new-positions))
         touch-frozen (some true? (map #(in? (get-in state [:game :frozen]) %) new-positions))]
      (if (and valid (not touch-frozen))
        (update-in state [:game :active-tetromino :position] #(add-vectors dir-vec %))
        state)))

(defn at-bottom? [state]
  (let [tetromino (get-in state [:game :active-tetromino])
        position (get-in tetromino [:position])
        positions (map #(add-vectors position %) (:offsets (:tetromino tetromino)))
        valid (every? true? (map #(inbounds? (get-in state [:game :size]) %) positions))
        positions-at-bottom (map #(= (second %) (dec (second (get-in state [:game :size])))) positions)
        any-at-bottom (not (every? false? positions-at-bottom))
        above-frozen-positions (map #(add-vectors (:n directions) %) (get-in state [:game :frozen]))
        touch-frozen (some #(in? above-frozen-positions %) positions)]
    (or touch-frozen any-at-bottom)))

(defn freeze-tetromino [state]
   (let [tetromino (get-in state [:game :active-tetromino])
         position (get-in tetromino [:position])
         positions (map #(add-vectors position %) (:offsets (:tetromino tetromino)))
         state (update-in state [:game :frozen] concat positions) ;;Need to conj here!
         state (assoc-in state [:game :active-tetromino] nil)]
     state))

(defn drop-tetromino [state]
  (let [state (move-tetromino state :s)]
    (if (at-bottom? state)
      (freeze-tetromino state)
      state)))

(defn get-row [frozen y]
  (filter #(= y (second %)) frozen))

(defn row-full? [frozen y width]
  (-> frozen
      (get-row y)
      (count)
      (= width)))

(defn clear-row [state y]
   (let [frozen (get-in state [:game :frozen])
         frozen (filter #(not (= y (second %))) frozen)]
     (assoc-in state [:game :frozen] frozen)))

(defn process-row [state width y]
  (if (row-full? (get-in state [:game :frozen]) y width)
    (clear-row state y)
    state))

(defn clear-rows-rec [state width counter]
  (if (zero? counter) state
    (let [state (process-row state width counter)]
      (clear-rows-rec state width (dec counter)))))

(defn clear-full-rows [state]
  (let [[width height] (get-in state [:game :size])]
    (clear-rows-rec state width height)))

(defn do-tick [state]
  (-> state
      spawn-tetromino
      drop-tetromino
      clear-full-rows))

(defn tick [state]
   (let [current-time (System/currentTimeMillis)
        tick-length (get-in state [:game :tick-length])
        last-tick (get-in state [:game :last-tick])
        next-tick (+ last-tick tick-length)]
    (if (>= current-time next-tick)
      (-> state
          do-tick
          (assoc-in [:game :last-tick] current-time))
      state)))

(defn quil-update [state]
  (-> state
      tick))

(defn draw-tetromino [position tetromino tile-map]
  (let [positions (map #(add-vectors position %) (:offsets tetromino))]
    (doseq [[x y] positions]
      (draw-tile x y tile-map (:tile-id tetromino) 16))))

(defn draw-tetrominoes [tetrominoes tile-map]
  (doseq [tetromino tetrominoes]
    (when (not (nil? tetromino))
      (draw-tetromino
        (:position tetromino)
        (:tetromino tetromino)
        tile-map))))

(defn draw-tiles [positions id tile-map]
  (doseq [[x y] positions]
    (draw-tile x y tile-map id 16)))

(defn clear-screen [state]
  (let [[width height] (get-in state [:game :size])
        tile-map (get-in state [:game :tile-map])]
    (doseq [x (range width)
            y (range height)]
      (draw-tile x y tile-map :0 16))))


(defn draw [state]
  (clear-screen state)
  (let [tetromino (get-in state [:game :active-tetromino])
        frozen (get-in state [:game :frozen])]
    (draw-tetrominoes [tetromino] (get-in state [:game :tile-map]))
    (draw-tiles frozen :2 (get-in state [:game :tile-map]))))

(defn move-left-tetromino [state]
  (move-tetromino state :w))

(defn move-right-tetromino [state]
  (move-tetromino state :e))

(defn process-input [state key-information]
  (if (nil? (get-in state [:game :active-tetromino]))
    state
    (case (:key key-information)
      :a (move-left-tetromino state)
      :d (move-right-tetromino state)
      :s (drop-tetromino state)
      state)))

(defn -main []
  (q/defsketch game-sketch
    :title "Tetris"
    :size [256 256]
    :setup setup
    :update quil-update
    :draw draw
    :key-pressed process-input
    :middleware [m/fun-mode]))

(-main)
