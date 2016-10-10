(ns cftbat.10.core)

;; exercise 1
(def counter (atom 0))
(swap! counter inc)
(swap! counter inc)
(swap! counter inc)
(println @counter)
