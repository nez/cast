(ns cast.core
  (:require [clojure.set :refer :all]
            [clojure.string :as s]
            [mongerr.core :refer :all]))

(defn dash
  [id]
  (:data (db-findf :dash {:id id})))

(defn dash-update
  [id newdata]
  (db-update :dash
             {:id id}
             {:data newdata}))

(defn new-widget
  [id widget]
  (dash-update id (conj (dash id) widget)))

(defn edit-widget
  [id widget]
  (dash-update id
               (map #(if (= (:name widget) ;TODO if you change the name then it doesnt update
                            (:name %))
                       widget
                       %)
                    (dash id))))

(defn delete-widget
  [id name]
  (dash-update id (remove #(= name (:name %))
                          (dash id))))
