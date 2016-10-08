(ns cftbat.09.core)

(defmacro wait
  "Sleep 'timeout' seconds before evaluating body"
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

(time @(let [saying3 (promise)]
         (future (deliver saying3 (wait 100 "Cheerio!")))
         @(let [saying2 (promise)]
            (future (deliver saying2 (wait 400 "Pip pip!")))
            @(let [saying1 (promise)]
               (future (deliver saying1 (wait 200 "'Ello, gov'na!")))
               (println @saying1)
               saying1)
            (println @saying2)
            saying2)
         (println @saying3)
         saying3))

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

(time @(-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
           (enqueue saying (wait 400 "Pip pip!") (println @saying))
           (enqueue saying (wait 100 "Cheerio!") (println @saying))))

;; exercise 1
(defn make-google-search-url
  [keyword]
  (str "https://www.google.co.kr/?gws_rd=ssl#q=" keyword))

(defn make-bing-search-url
  [keyword]
  (str "https://www.bing.com/search?q=" keyword))

(def search-engines {:google make-google-search-url :bing make-bing-search-url})

(defn search-with-engine
  [keyword engine]
  (let [url-builder (engine search-engines)
        url (url-builder keyword)]
    (slurp url)))

(defn get-search-result
  ([keyword [& engines]]
   (let [result (promise)]
     (doseq [engine engines]
       (future (deliver result (search-with-engine keyword engine))))
     (deref result 2000 "timed out")))
  ([keyword]
   (get-search-result keyword (keys search-engines))))

(defn extract-urls
  [html-body]
  (let [matches (re-seq #"href=\"([^\" ]*)\"" html-body)]
    (filter #(clojure.string/starts-with? % "http") (map second matches))))

(defn get-urls-with-engine
  [keyword engine]
  (let [result (search-with-engine keyword engine)]
    (extract-urls result)))

(defn get-urls-search
  ([keyword [& engines]]
   (let [futures (map #(future (get-urls-with-engine keyword %)) engines)
         urls-each-engine (map #(deref % 2000 nil) futures)]
     (into [] (reduce concat urls-each-engine))))
  ([keyword]
   (get-urls-search keyword (keys search-engines))))