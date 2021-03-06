(ns cftbat.10.core)

;; exercise 1
(def counter (atom 0))
(swap! counter inc)
(swap! counter inc)
(swap! counter inc)
(println @counter)

;; exercise 2
(defn quote-word-count-single
  []
  (frequencies (re-seq #"\w+" (slurp "http://www.braveclojure.com/random-quote"))))

(defn quote-word-count
  [quote-count]
  (let [counts (atom {})
        counts-per-quote (doall (repeatedly quote-count #(future (quote-word-count-single))))]
    (doseq [count-per-quote counts-per-quote]
      (swap! counts #(merge-with + % @count-per-quote)))
    @counts))

;; exercise 3
(def first-character (ref {:hp 15}))
(def second-character (ref #{:potion}))

(defn heal
  [healer target]
  (dosync
    (alter healer disj target :potion)
    (alter target update-in [:hp] + 10)))