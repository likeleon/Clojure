(ns cftbat.03.core)

;; let
(def dalmatian-list
  ["Pongo" "Perdita" "Puppy 1" "Puppy2"])
(let [dalmatians (take 2 dalmatian-list)]
  (println dalmatians))
(let [[pongo & rest] dalmatian-list]
  (println [pongo rest]))

(def x 0)
(let [x (inc x)] (println x))

;; into
(println (into [] ["a" "b" "c"]))
(println (into [] (set [:a :a])))

;; loop
(loop [iteration 0]
  (println (str "Iteration " iteration))
  (if (> iteration 3)
    (println "Goodbye!")
    (recur (inc iteration))))

(defn recursive-printer
  ([]
   (recursive-printer 0))
  ([iteration]
   (println (str "Iteration " iteration))
   (if (> iteration 3)
     (println "Goodbye!")
     (recursive-printer (inc iteration)))))
(recursive-printer)

;; regular expressions
(println (re-find #"^left-" "left-eye"))
(println (re-find #"^left-" "cleft-chin"))
(println (re-find #"^left-" "wongleblart"))

;; reduce
(println (reduce + [1 2 3 4]))
(println (reduce + 15 [1 2 3 4]))

(defn my-reduce
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce f head tail)))

(println (my-reduce + 15 [1 2 3 4]))

;; hobbit
(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  ":name과 :size가 대응되는 일련의 순서쌍을 기대"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))
(println (symmetrize-body-parts asym-hobbit-body-parts))

(defn better-symmetrize-body-parts
  ":name과 :size가 대응되는 일련의 순서쌍을 기대"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))
(println (better-symmetrize-body-parts asym-hobbit-body-parts))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(println (hit asym-hobbit-body-parts))

;; excercise 1
(println(str "Hello" ", World"))
(println (vector 1 2 3 4 5))
(println (list 1 2 3 4 5))
(println (hash-map :name "likeleon" :age 34))
(println (hash-set 1 1 2 3))

;; excercies 2
(defn add-100
  [num]
  (+ num 100))
(println (add-100 5))

;; exercise 3
(defn dec-maker
  [dec-by]
  #(- % dec-by))
(def dec9 (dec-maker 9))
(println (dec9 10))

;; exercise 4
(defn mapset-loop
  [f coll]
  (loop [remaining coll
         result #{}]
    (if (empty? remaining)
      result
      (recur (rest remaining) (into result #{(f (first remaining))})))))
(println (mapset-loop inc [1 1 2 2]))

(defn mapset-map
  [f coll]
  (set (map f coll)))
(println (mapset-map inc [1 1 2 2]))
