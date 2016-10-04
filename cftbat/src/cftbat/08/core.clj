(ns cftbat.08.core)

(def order-details
  {:name "Mitchard Blimmons"
   :email "mitchard.blimmonsgmail.com"})

(def order-details-validations
  {:name
   ["Please enter a name" not-empty]
   :email
   ["Please enter an email address" not-empty
    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(println (error-messages-for "" ["Please enter a name" not-empty]))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(println (validate order-details order-details-validations))

(defn if-valid-bad-version
  [record validations success-code failure-code]
  (let [errors (validate record validations)]
    (if (empty? errors)
      success-code
      failure-code)))

(println (if-valid-bad-version order-details order-details-validations
                               (println :success)
                               (println :failure)))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(println
  (if-valid order-details order-details-validations my-error-name
            (println :success)
            (println :failure my-error-name)))

;; exercise 1
(defmacro when-valid
  [to-validate validations & body]
  `(let [error# (validate ~to-validate ~validations)]
     (when (empty? error#) ~@body)))

(defn render [k] k)

(println
  (when-valid order-details order-details-validations
              (println "It's a success!")
              (render :success)))

;; exercise 2
(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & next]
    `(let [or# ~x]
       (if or# or# (my-or ~@next)))))

;; exercise 3
(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(defn define-attr-fn
  [fn-name attr]
  `(def ~fn-name (comp ~attr :attributes)))

(defmacro defattrs
  ([] nil)
  ([fn-name attr]
   (define-attr-fn fn-name attr))
  ([fn-name attr & rest]
   `(do
      (defattrs ~fn-name ~attr)
      (defattrs ~@rest))))

(println (macroexpand `(defattrs c-int :intelligence
                                 c-str :strength
                                 c-dex :dexterity)))

(defattrs c-int :intelligence
          c-str :strength
          c-dex :dexterity)

(println (c-int character))
(println (c-str character))
(println (c-dex character))
