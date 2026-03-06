(ns ^:no-doc io.github.evenmoreirrelevance.testcomments.util
  (:require [clojure.string :as str]))

(defn assoc-truthy
  [m k v]
  (if-not v
    m
    (assoc m k v)))

(defmacro catching
  [body & catch-specs]
  `(try (do ~body)
     ~@(for [spec catch-specs]
         `(catch ~@spec))))

(defmacro ensuring-cleanup
  [body & cleanup]
  `(try (do ~body) (finally ~@cleanup)))

(defn -throw-validation-failure
  [map_]
  (when map_
    (throw
      (ex-info (str/join "\n- " (cons "validation errors:" (keys map_)))
        map_))))

(defmacro ensure-false
  [& codes->falsities]
  `(-throw-validation-failure
     (-> nil
       ~@(doall
           (for [[c f :as part] (partition-all 2 codes->falsities)]
             (if (not= 2 (count part))
               (throw (ex-info "provide pairs of keys" {:form &form}))
               `(assoc-truthy ~c (catching ~f (Exception e# e#)))))))))

(defn strict-uninterleave
  [n coll]
  (let [out
        (vec  
          (if-not (seq coll)
            (repeat n []) 
            (apply map vector (partition-all n coll))))]
    (when (= n (count out))
      out)))


(comment
  (strict-uninterleave 2 [1 2 3 4 5 6])
  (strict-uninterleave 2 [1 2 3 4 5 6 7])

  (ensure-false
    :foo (not= 0 1))
  *e)
