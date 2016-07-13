(ns cast.core
  (:require [clojure.set :refer :all]
            [clojure.string :as s]
            [mongerr.core :refer :all]))

(defmulti dash number?)

(defmethod dash true [id] (:data (db-findf :dash {:id id})))

(defmethod dash false [id] (dash (read-string id)))

(defmulti dash-update (fn [id newdata] (number? id)))

(defmethod dash-update false [id newdata]
  (dash-update (read-string id)
               newdata))

(defmethod dash-update true [id newdata]
  (db-upsert :dash {:id id} (assoc  {:data newdata} :id id)))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn widget [dash-id uuid]
  (first (filter #(= uuid (:uuid %))
                 (dash dash-id))))

(defn new-widget
  [id widget]
  (dash-update id
               (conj (dash id)
                     (assoc widget :uuid (uuid)))))

(defn edit-widget
  [id widget]
  (let [il-dash (dash id)
        dash-update (map #(if (= (:uuid widget) ;TODO if you change the name then it don update
                                 (:uuid %))
                            widget
                            %)
                         il-dash)]
    (dash-update id
                 (if (= il-dash dash-update)
                   (conj il-dash (assoc widget :uuid (uuid)))
                   dash-update))))

(defn delete-widget
  [id uuid]
  (dash-update id (remove #(= uuid (:uuid %))
                          (dash id))))

(defn update-widget-data [widget]
  (try (assoc widget :data (eval (read-string (:dataexpr widget))))
       (catch Exception e widget)))

(defmulti update-dash-data [id] number?)

(defmethod update-dash-data true [id]
  (dash-update id (map update-widget-data (dash-id))))

(defmethod update-dash-data false [id]
  (update-dash-data (read-string id)))
