(ns cftbat.05.core
  (require [clojure.set :as set]
           (:gen-class)))

;; comp
(println ((comp inc *)2 3))

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))

(println (c-int character))
(println (c-str character))
(println (c-dex character))

(defn spell-slots
  [char]
  (int (inc (/ (c-int char) 2))))

(println (spell-slots character))

(def spell-slots-comp (comp int inc #(/ % 2) c-int))
(println (spell-slots-comp character))

(defn two-comp
  [f g]
  (fn [& args]
    (f (apply g args))))

(println ((two-comp inc +) 2 3))

;; memoize
(defn sleepy-identity
  [x]
  (Thread/sleep 1000)
  x)

(def memo-sleepy-identity (memoize sleepy-identity))
;(time (memo-sleepy-identity "likeleon"))
;(time (memo-sleepy-identity "likeleon"))

;; assoc-in, get-in
(println (assoc-in {} [:cookie :monster :vocals] "Finntroll"))
(println (get-in {:cookie {:monster {:vocals "Finntroll"}}} [:cookie :monster]))

;; Peg Thing
(declare successful-move prompt-move game-over prompt-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([](tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))
;(println (take 5 tri))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))
;(println (triangular? 5))
;(println (triangular? 6))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))
;(println (row-tri 1))
;(println (row-tri 2))
;(println (row-tri 3))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))
;(println (row-num 1))
;(println (row-num 5))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connection p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))
;(println (connect {} 15 1 2 4))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

;(println (connect-down-left {} 15 1))
;(println (connect-down-right {} 15 3))