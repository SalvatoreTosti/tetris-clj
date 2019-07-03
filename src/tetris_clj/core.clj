(ns tetris-clj.core
    (:use
      [tetris-clj.sprites :only [load-tile-map draw-tile draw-tiles]]
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

(defn make-tetromino [offsets]
  {:tile-id :118
   :rotation 0
   :rotation-offsets offsets
   :offsets (first offsets)})

(def tetrominoes
  {
    :l (make-tetromino
        [[[0 0] [0 1] [1 1] [0 -1]]
         [[0 0] [-1 0] [-1 1] [1 0]]
         [[0 0] [0 -1] [-1 -1] [0 1]]
         [[0 0] [1 0] [1 -1] [-1 0]]])
   :line (make-tetromino
        [[[0 0] [-1 0] [-2 0] [1 0]]
         [[0 0] [0 -1] [0 -2] [0 1]]
         [[0 0] [1 0] [2 0] [-1 0]]
         [[0 0] [0 -1] [0 -2] [0 1]]])
   :z (make-tetromino
        [[[0 0] [0 -1] [1 0] [1 1]]
         [[0 0] [1 0] [0 1] [-1 1]]
         [[0 0] [0 1] [-1 0] [-1 -1]]
         [[0 0] [-1 0] [0 -1] [1 -1]]])

   :s (make-tetromino
        [[[0 0] [0 -1] [1 0] [1 1]]
         [[0 0] [1 0] [0 1] [-1 1]]
         [[0 0] [0 1] [-1 0] [-1 -1]]
         [[0 0] [-1 0] [0 -1] [1 -1]]])

   :square (make-tetromino
             [[[0 0] [1 0] [0 -1] [1 -1]]
              [[0 0] [1 0] [0 -1] [1 -1]]
              [[0 0] [1 0] [0 -1] [1 -1]]
              [[0 0] [1 0] [0 -1] [1 -1]]])
   :t (make-tetromino
        [[[0 0] [0 1] [-1 0] [1 0]]
         [[0 0] [0 1] [0 -1] [1 0]]
         [[0 0] [0 -1] [1 0] [-1 0]]
         [[0 0] [0 1] [0 -1] [-1 0]]])})

(defn rotate-tetromino [state]
  (let [current-offset (get-in state [:game :active-tetromino :tetromino :rotation])
        next-rotation (if (< current-offset 3)
                        (inc current-offset)
                        0)
        next-offset (nth (get-in state [:game :active-tetromino :tetromino :rotation-offsets]) next-rotation)]
    (println next-rotation next-offset)
    (-> state
        (assoc-in [:game :active-tetromino :tetromino :rotation] next-rotation)
        (assoc-in [:game :active-tetromino :tetromino :offsets] next-offset))))

(def directions
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]})

(defn spawn-tetromino [state]
  (if (get-in state [:game :active-tetromino])
    state
    (assoc-in state [:game :active-tetromino]
              {:position [1 1]
               :tetromino (:l tetrominoes)
;;                (-> tetrominoes
;;                    keys
;;                    rand-nth
;;                    tetrominoes)
               })
    )) 

(defn get-tetromino-positions [state]
  (let [position (get-in state [:game :active-tetromino :position])
        offsets (get-in state [:game :active-tetromino :tetromino :offsets])]
    (map #(add-vectors position %) offsets)))

(defn- move-tetromino [state dir]
   (let [dir-vec (dir directions)
         new-positions (map #(add-vectors dir-vec %) (get-tetromino-positions state))
         valid (->>  new-positions
                     (map #(inbounds? (get-in state [:game :size]) %) )
                     (every? true?))
         touch-frozen (->> new-positions
                           (map #(in? (get-in state [:game :frozen]) %))
                           (some true?))]
      (if (and valid (not touch-frozen))
        (update-in state [:game :active-tetromino :position] #(add-vectors dir-vec %))
        state)))

(defn at-bottom? [state]
  (let [positions (get-tetromino-positions state)
        valid (every? true? (map #(inbounds? (get-in state [:game :size]) %) positions))
        positions-at-bottom (map #(= (second %) (dec (second (get-in state [:game :size])))) positions)
        any-at-bottom (some true? positions-at-bottom)
        above-frozen-positions (map #(add-vectors (:up directions) %) (get-in state [:game :frozen]))
        touch-frozen (some #(in? above-frozen-positions %) positions)]
    (or touch-frozen any-at-bottom)))

(defn freeze-tetromino [state]
   (let [tetromino (get-in state [:game :active-tetromino])
         position (get-in tetromino [:position])
         positions (map #(add-vectors position %) (:offsets (:tetromino tetromino)))]
     (-> state
         (update-in [:game :frozen] concat positions)
         (assoc-in [:game :active-tetromino] nil))))

(defn drop-tetromino [state]
  (let [state (move-tetromino state :down)]
    (if (at-bottom? state)
      (freeze-tetromino state)
      state)))

(defn get-row [frozen y]
  (filter #(= y (second %)) frozen))

(defn row-full? [state row-number]
  (-> state
      (get-in [:game :frozen])
      (get-row row-number)
      count
      (= (first (get-in state [:game :size])))))

(defn clear-row [state y]
  (let [frozen (get-in state [:game :frozen])
        upper-half (filter #(> y (second %)) frozen)
        lower-half (filter #(< y (second %)) frozen)
        upper-half (map #(add-vectors (:down directions) %) upper-half)]
  (->> (concat upper-half lower-half)
       (assoc-in state [:game :frozen]))))

(defn process-row [state width row-number]
  (if (row-full? state row-number)
    (-> (clear-row state row-number)
        (process-row width row-number))
    state))

(defn clear-full-rows [state]
  (let [[width height] (get-in state [:game :size])]
    (loop [state state
           width width
           counter height]
          (if (zero? counter)
            state
            (-> state
                (process-row width counter)
                (recur width (dec counter)))))))

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

(defn quil-update [state] (tick state))

(defn draw-active-tetromino [state]
    (doseq [[x y] (get-tetromino-positions state)]
        (draw-tile
          x
          y
          (get-in state [:game :tile-map])
          (get-in state [:game :active-tetromino :tetromino :tile-id])
          16)))

(defn draw-frozen-tiles [state]
  (draw-tiles
    (get-in state [:game :frozen])
    :119
    (get-in state [:game :tile-map])
    16))

(defn clear-screen [state]
  (let [[width height] (get-in state [:game :size])
        tile-map (get-in state [:game :tile-map])]
    (doseq [x (range width)
            y (range height)]
      (draw-tile x y tile-map :0 16))))

(defn draw [state]
  (clear-screen state)
  (draw-active-tetromino state)
  (draw-frozen-tiles state))

(defn process-input [state key-information]
  (if (nil? (get-in state [:game :active-tetromino]))
    state
    (case (:key key-information)
      :a (move-tetromino state :left)
      :d (move-tetromino state :right)
      :s (drop-tetromino state)
      :e (rotate-tetromino state)
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
