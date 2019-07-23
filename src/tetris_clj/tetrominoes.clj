(ns tetris-clj.tetrominoes
   (:use
     [tetris-clj.utils :only [add-vectors inbounds?]]))

(defn make-tetromino [offsets color]
  {:tile-id :118
   :rotation 0
   :rotation-offsets offsets
   :offsets (first offsets)
   :color color})

(def tetrominoes
  {:l (make-tetromino
        [[[0 0] [0 1] [1 1] [0 -1]]
         [[0 0] [-1 0] [-1 1] [1 0]]
         [[0 0] [0 -1] [-1 -1] [0 1]]
         [[0 0] [1 0] [1 -1] [-1 0]]]
        :pink)
    :j (make-tetromino
        [[[0 0] [1 0] [-1 0] [-1 -1]]
         [[0 0] [0 1] [0 -1] [1 -1]]
         [[0 0] [1 0] [-1 0] [1 1]]
         [[0 0] [0 -1] [0 1] [-1 1]]]
         :purple)
   :line (make-tetromino
        [[[0 0] [-1 0] [-2 0] [1 0]]
         [[0 0] [0 -1] [0 -2] [0 1]]
         [[0 0] [1 0] [2 0] [-1 0]]
         [[0 0] [0 -1] [0 -2] [0 1]]]
         :orange)
   :z (make-tetromino
        [[[0 0] [0 -1] [1 0] [1 1]]
         [[0 0] [1 0] [0 1] [-1 1]]
         [[0 0] [0 1] [-1 0] [-1 -1]]
         [[0 0] [-1 0] [0 -1] [1 -1]]]
         :green)
   :s (make-tetromino
        [[[0 0] [0 -1] [-1 -1] [1 0]]
         [[0 0] [0 1] [1 0] [1 -1]]
         [[0 0] [0 1] [1 1] [-1 0]]
         [[0 0] [0 -1] [-1 0] [-1 1]]]
         :red)
   :o (make-tetromino
        [[[0 0] [1 0] [0 -1] [1 -1]]
         [[0 0] [1 0] [0 -1] [1 -1]]
         [[0 0] [1 0] [0 -1] [1 -1]]
         [[0 0] [1 0] [0 -1] [1 -1]]]
         :yellow)
   :t (make-tetromino
        [[[0 0] [0 1] [0 -1] [1 0]]
         [[0 0] [0 1] [-1 0] [1 0]]
         [[0 0] [0 1] [0 -1] [-1 0]]
         [[0 0] [0 -1] [1 0] [-1 0]]]
         :purple)})

(defn can-rotate? [state next-offsets]
  (let [position (get-in state [:active-tetromino :position])
        next-positions (map #(add-vectors position %) next-offsets)
        next-inbounds (map #(inbounds? (:size state) %) next-positions)]
    (every? true? next-inbounds)))

(defn rotate-tetromino [state]
  (let [current-offset (get-in state [:active-tetromino :rotation])
        next-rotation (if (< current-offset 3)
                        (inc current-offset)
                        0)
        next-offsets (nth (get-in state [:active-tetromino :rotation-offsets]) next-rotation)]
    (if (can-rotate? state next-offsets)
      (-> state
          (assoc-in [:active-tetromino :rotation] next-rotation)
          (assoc-in [:active-tetromino :offsets] next-offsets))
      state)))

(defn swap-tetromino [state]
  (let [counter (inc (:tetromino-counter state))]
    (-> state
        (assoc-in [:active-tetromino] (nth (:tetromino-sequence state) counter))
        (assoc-in [:next-tetromino] (nth (:tetromino-sequence state) (inc counter)))
        (assoc-in [:tetromino-counter] counter))))

(defn spawn-tetromino [state]
  (if (get-in state [:active-tetromino])
    state
    (-> state
        swap-tetromino
        (assoc-in [:active-tetromino :position] [5 0]))))

(defn random-tetromino []
  (-> tetrominoes
      keys
      rand-nth
      tetrominoes))

(defn random-tetromino-sequence []
 (repeatedly #(random-tetromino)))
