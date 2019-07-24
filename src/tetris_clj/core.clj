(ns tetris-clj.core
  (:use
   [tetris-clj.sprites :only [draw-tile draw-tiles load-tiles draw-text]]
   [tetris-clj.utils :only [in? add-vectors inbounds?]]
   [tetris-clj.tetrominoes :only [rotate-tetromino spawn-tetromino random-tetromino-sequence]])
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(def directions
  {:up [0 -1]
   :down [0 1]
   :left [-1 0]
   :right [1 0]})

(defn make-game []
  "Generates a new game with default start values."
  {:mode :play
   :last-tick (System/currentTimeMillis)
   :tick-length 500 ;;length in miliseconds
   :size [10 16]
   :score 0
   :frozen []
   :tetromino-sequence (random-tetromino-sequence)
   :tetromino-counter 0
   :tile-map (load-tiles)})

(defn setup []
  "This function is run one time when sketch starts."
  (q/background 0)
  (q/frame-rate 30)
  (make-game))

(defn get-active-piece-positions [game]
  "Adds the active tetromino position and offsets.
   Gives the positions occupied by the active tetromino."
  (let [position (get-in game [:active-tetromino :position])
        offsets (get-in game [:active-tetromino :offsets])]
    (map #(add-vectors position %) offsets)))

(defn move-tetromino [game dir]
  "Attempts to move the active tetromino.
   If active any tile positions will be invalid, move is not performed."
  (let [dir-vec (dir directions)
        new-positions (map #(add-vectors dir-vec %) (get-active-piece-positions game))
        valid (->>  new-positions
                    (map #(inbounds? (get-in game [:size]) %))
                    (every? true?))
        touch-frozen (->> new-positions
                          (map #(in? (get-in game [:frozen]) %))
                          (some true?))]
    (if (and valid (not touch-frozen))
      (update-in game [:active-tetromino :position] #(add-vectors dir-vec %))
      game)))

(defn at-bottom? [game]
  "Determines if active tetromino is at the bottom or touching frozen positions."
  (let [positions (get-active-piece-positions game)
        valid (every? true? (map #(inbounds? (get-in game [:size]) %) positions))
        positions-at-bottom (map #(= (second %) (dec (second (get-in game [:size])))) positions)
        any-at-bottom (some true? positions-at-bottom)
        above-frozen-positions (map #(add-vectors (:up directions) %) (get-in game [:frozen]))
        touch-frozen (some #(in? above-frozen-positions %) positions)]
    (or touch-frozen any-at-bottom)))

(defn freeze-tetromino [game]
  "Adds active tetromino positions into frozen pool and sets active tetromino to nil."
  (let [tetromino (get-in game [:active-tetromino])
        position (get-in tetromino [:position])
        positions (map #(add-vectors position %) (:offsets tetromino))]
    (-> game
        (update-in [:frozen] concat positions)
        (assoc-in [:active-tetromino] nil))))

(defn drop-tetromino [game]
  "Attempts to move active tetromino down 1 position.
   If any tile positions will be invalid, move is not performed."
  (let [game (move-tetromino game :down)]
    (if (at-bottom? game)
      (freeze-tetromino game)
      game)))

(defn get-row [frozen y]
  "Returns list of all items where Y coordinate is equal to the given value."
  (filter #(= y (second %)) frozen))

(defn row-full? [game row-number]
  "Determines if a given row is filled with frozen tiles."
  (-> game
      (get-in [:frozen])
      (get-row row-number)
      count
      (= (first (get-in game [:size])))))

(defn clear-row [game y]
  "Removes a given row from the pool of frozen tiles."
  (let [frozen (get-in game [:frozen])
        upper-half (filter #(> y (second %)) frozen)
        lower-half (filter #(< y (second %)) frozen)
        upper-half (map #(add-vectors (:down directions) %) upper-half)]
    (->> (concat upper-half lower-half)
         (assoc-in game [:frozen]))))

(defn process-row [game width row-number]
  "Removes given row if it's full of frozen tiles and adds to score accordingly."
  (if (row-full? game row-number)
    (-> game
        (clear-row row-number)
        (process-row width row-number)
        (update :score #(+ % 100)))
    game))

(defn clear-full-rows [game]
  "Removes all full frozen rows."
  (let [[width height] (get-in game [:size])]
    (loop [game game
           width width
           counter height]
      (if (zero? counter)
        game
        (-> game
            (process-row width counter)
            (recur width (dec counter)))))))

(defn game-over-processing [game]
  "Determines if the player has lost the game.
   Changes mode to :game-over when frozen items reach the top of the play area."
  (if (->> game
       :frozen
       (map #(second %))
       (some neg?))
    (assoc-in game [:mode] :game-over)
    game))

(defmulti quil-update
  (fn [game]
    (:mode game)))

(defmethod quil-update :play [game]
  (let [current-time (System/currentTimeMillis)
        next-tick (+ (get-in game [:last-tick])
                     (get-in game [:tick-length]))]
    (if (< current-time next-tick)
      game
      (-> game
          spawn-tetromino
          drop-tetromino
          game-over-processing
          clear-full-rows
          (assoc-in [:last-tick] current-time)))))

(defmethod quil-update :game-over [game]
  game)

(defn draw-active-piece [game]
  (doseq [position (get-active-piece-positions game)]
    (draw-tile
     position
     (get-in game [:tile-map])
     (get-in game [:active-tetromino :tile-id])
     16
     (get-in game [:active-tetromino :color]))))

(defn draw-frozen-tiles [game]
  (draw-tiles
   (get-in game [:frozen])
   (get-in game [:tile-map])
   :119
   16
   :sky-blue))

(defn clear-screen [game]
  "Overwrites entire screen with black."
  (let [[width height] (get-in game [:size])
        tiles (for [x (range 16)
                    y (range 16)]
                [x y])]
    (draw-tiles
     tiles
     (get-in game [:tile-map])
     :0
     16)))

(defn pad-string-front [string length padding]
  "Adds padding to given string until it's equal to the given length."
  (let [difference (- length (count string))
        padding (->> (repeat padding)
                     (take difference)
                     (reduce str))]
    (if (neg? difference)
      string
      (str padding string))))

(defn draw-score [game]
  "Displays score information."
  (draw-text 11 0 (get-in game [:tile-map]) "Score")
  (draw-text
    11
    1
    (get-in game [:tile-map])
    (pad-string-front (str (:score game)) 5 "0")))

(defn draw-separator [game]
  "Displays play area separator."
  (doseq [y (range (second (:size game)))]
    (draw-tile [10 y] (get-in game [:tile-map]) :54 16 :slate)))

(defn draw-next-tetromino [game]
  "Draw the next tetromino that will drop."
   (doseq [position (map #(add-vectors [13 8] %) (get-in game [:next-tetromino :offsets]))]
     (draw-tile
      position
      (get-in game [:tile-map])
      (get-in game [:next-tetromino :tile-id])
      16
      (get-in game [:next-tetromino :color]))))

(defmulti draw
  (fn [game]
    (:mode game)))

(defmethod draw :play [game]
  "Draw play mode screen."
  (clear-screen game) ;;make whole screen black
  (draw-active-piece game) ;;:draw the active piece
  (draw-frozen-tiles game) ;;draw all the inactive pieces
  (draw-score game)
  (draw-next-tetromino game)
  (draw-text 11 5 (get-in game [:tile-map]) "Next")
  (draw-separator game))

(defmethod draw :game-over [game]
  "Draw game over screen."
  (clear-screen game) ;;make whole screen black
  (draw-frozen-tiles game) ;;draw all the inactive pieces
  (draw-score game)
  (draw-next-tetromino game)
  (draw-text 11 5 (get-in game [:tile-map]) "Next")
  (draw-separator game)
  (draw-text 0 6 (get-in game [:tile-map]) "                ")
  (draw-text 0 7 (get-in game [:tile-map]) "   game over    ")
  (draw-text 0 8 (get-in game [:tile-map]) "  press enter   ")
  (draw-text 0 9 (get-in game [:tile-map]) "                "))

(defmulti process-input
  (fn [game key-information]
    (:mode game)))

(defmethod process-input :play [game key-information]
  "Process input in play status/"
  (if (get-in game [:active-tetromino])
    (-> (case (:key key-information)
          :a (move-tetromino game :left)
          :d (move-tetromino game :right)
          :s (drop-tetromino game)
          :e (rotate-tetromino game)
          game)
        clear-full-rows)
    game))

(defmethod process-input :game-over [game key-information]
  "Process input in game over state."
  (-> (case (:key-code key-information)
        10 (make-game) ;enter key
        game)))

(defn -main []
  (q/defsketch game-sketch
               :title "Tetris"
               :size [256 256]
               :setup setup
               :update quil-update
               :draw draw
               :key-pressed process-input
               :middleware [m/fun-mode]))

;; (-main)
