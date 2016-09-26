(ns cftbat.04.core)

(def filename "src/cftbat/04/suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\r\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(def records (mapify (parse (slurp filename))))
(println (glitter-filter 3 records))

;; exercise 1
(defn glitter-filter-name
  [minimum-glitter records]
  (map #(:name %)
       (glitter-filter minimum-glitter records)))

(println (glitter-filter-name 3 records))

;; exercise 2
(defn append
  [records name glitter-index]
    (conj records {:name name
                   :glitter-index glitter-index}))

;; exercise 3
(defn validate
  [validators, record]
  (every? true?
          (map #((get validators %) (get record %))
               vamp-keys)))

(def not-nil? (complement nil?))

(println (validate {:name not-nil?
                    :glitter-index not-nil?}
                   {:name "likeleon" :glitter-index 5}))

;; exercise 4
(defn to-csv
  [records]
  (clojure.string/join
    "\r\n"
    (map #(clojure.string/join "," (vals %)) records)))

(println (to-csv records))