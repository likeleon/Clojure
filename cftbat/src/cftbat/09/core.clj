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
(defn google-search-url
  [keyword]
  (str "https://www.google.co.kr/?gws_rd=ssl#q=" keyword))

(defn bing-search-url
  [keyword]
  (str "https://www.bing.com/search?q=" keyword))

(defn search-web
  ([keyword search-engines]
   (let [result (promise)]
     (doseq [url (map #(% keyword) search-engines)]
       (future (deliver result (slurp url))))
     (deref result 2000 "timed out")))
  ([keyword]
   (search-web keyword [google-search-url bing-search-url])))

(defn search-web-all
  ([keyword search-engines]
   (let [futures (map #(slurp (% keyword)) search-engines)]
     map #(deref % 2000 nil) futures))
  ([keyword]
   (search-web-all keyword [google-search-url bing-search-url])))