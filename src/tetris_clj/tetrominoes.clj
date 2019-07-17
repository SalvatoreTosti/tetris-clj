(ns tetris-clj.tetrominoes)

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
        [[[0 0] [0 1] [-1 0] [1 0]]
         [[0 0] [0 1] [0 -1] [1 0]]
         [[0 0] [0 -1] [1 0] [-1 0]]
         [[0 0] [0 1] [0 -1] [-1 0]]]
         :purple)})

(defn rotate-tetromino [state]
  (let [current-offset (get-in state [:active-tetromino :rotation])
        next-rotation (if (< current-offset 3)
                        (inc current-offset)
                        0)
        next-offset (nth (get-in state [:active-tetromino :rotation-offsets]) next-rotation)]
    (-> state
        (assoc-in [:active-tetromino :rotation] next-rotation)
        (assoc-in [:active-tetromino :offsets] next-offset))))

(defn spawn-tetromino [state]
  (if (get-in state [:active-tetromino])
    state
    (assoc-in state [:active-tetromino]
              (-> tetrominoes
                  keys
                  rand-nth
                  tetrominoes
                  (assoc :position [5 0])))))
