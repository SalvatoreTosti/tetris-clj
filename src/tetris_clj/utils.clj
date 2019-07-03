(ns tetris-clj.utils)

(defn in?
  "true if coll contains element"
  [coll element]
  (some #(= element %) coll))

(defn add-vectors [a b]
  [(+ (first a) (first b))
   (+ (second a) (second b))])

(defn inbounds? [[width height] [x y]]
  (and
    (>= x 0)
    (< x width)
    (>= y 0)
    (< y height)))
