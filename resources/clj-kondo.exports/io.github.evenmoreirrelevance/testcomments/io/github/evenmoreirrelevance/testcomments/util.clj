(ns io.github.evenmoreirrelevance.testcomments.util
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

(defmacro ensure-false
  [& codes->falsities]
  `(when-let [m# (-> nil
                   ~@(doall
                       (for [[c f :as part] (partition-all 2 codes->falsities)]
                         (if (not= 2 (count part))
                           (throw (ex-info "provide pairs of keys" {:form &form}))
                           `(assoc-truthy ~c (catching ~f (Exception e# e#)))))))]
     (throw (ex-info (str/join "\n- " (cons "validation errors:" (keys m#))) m#))))
