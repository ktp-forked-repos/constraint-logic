(ns constraint.common)

(def vert-id  "vertex")
(def str-vert-id  (partial str vert-id))
(def edge-id  "edge")

(def color->value {:red 1
                   :blue 2})

(def map-padding [50 50])

(defn inflow [edges vertex]
  (let [incoming-values (for [[_ [_ to color]] edges
                              :when (= to vertex)]
                          (color->value color))]
    (apply + incoming-values)))


(defn ok-to-flip? [{:keys [vertices edges]} [_ to color]]
  (let [edge-value      (color->value color)
        to-inflow       (inflow edges to)
        to-vertex-value (first (vertices to))]
    (>= (- to-inflow edge-value) to-vertex-value)))



(defn prepare-edge [locations world-state [start end color player :as edge]]
  [(locations start)
   (locations end)
   color
   (ok-to-flip? world-state edge)
   player])


(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))


(defn prepare-edges [{:keys [vertices edges] :as world-state}]
  (let [locations (fmap second vertices)]
    (fmap (partial prepare-edge locations world-state) edges)))


(defn prepare-vertices [{:keys [vertices edges]}]
  (into {} (for [[vertex-id vertex-data] vertices
                 :let [incoming-flow (inflow edges vertex-id)]]
             [vertex-id (conj vertex-data incoming-flow)])))


(defn map-size [{:keys [vertices]}]
  (->> vertices
       (vals)
       (map second)
       (apply map max)
       (map + map-padding)))
