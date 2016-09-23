(ns cftbat.03)

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

;;
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

