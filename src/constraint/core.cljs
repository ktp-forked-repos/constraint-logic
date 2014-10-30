(ns constraint.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])

  (:require
    [constraint.svg :refer [make-svg]]
    [dataview.loader :refer [fetch-text]]
    [dommy.core :as dommy]
    [crate.core :as crate]
    [big-bang.core :refer [big-bang!]]
    [cljs.reader :as reader]
    [goog.net.XhrIo :as xhr]
    [cljs.core.async :as async :refer [<! chan close!]]))



(defn GET [url]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (let [res (-> event .-target .getResponseText)]
                  (go (>! ch res)
                      (close! ch)))))
    ch))



(def color->value {:red 1
                   :blue 2})

(defn inflow [edges vertex]
  (let [in-going (for [[_ [_ to color]] edges
                       :when (= to vertex)]
                   (color->value color))]
    (apply + in-going)))



(defn event->targetid [e]
  (-> e
      (js->clj)
      (.-target)
      (.-id)))

(def vert-id "vertex")
(def str-vert-id (partial str vert-id))
(def edge-id "edge")

(defn nameless->named-map [the-name coll]
  (let [ids (map (partial str the-name) (range))]
    (into {} (map vector ids coll))))

(def name-edges
  (partial nameless->named-map edge-id))

(def name-vertices
  (partial nameless->named-map vert-id))

(defn name-vertices-in-one-edge [[start end color]]
  [(str-vert-id start)
   (str-vert-id end)
   color])

(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn name-vertices-in-all-edges [edges]
  (fmap name-vertices-in-one-edge edges))

(defn ok-to-flip? [{:keys [vertices edges]} [_ to color]]
  (let [edge-value (color->value color)
        to-inflow (inflow edges to)
        to-vertex-value (first (vertices to))]
    (>= (- to-inflow edge-value) to-vertex-value)))



(defn prepare-edge [locations world-state [start end color :as edge]]
  [(locations start)
   (locations end)
   color
   (ok-to-flip? world-state edge)])



(defn map-size [{:keys [vertices]}]
  (->> vertices
       (vals)
       (map second)
       (apply map max)
       (map + [50 50])))



(defn make-edges [{:keys [vertices edges] :as world-state}]
  (let [locations (fmap second vertices)]
    (fmap (partial prepare-edge locations world-state) edges)))



(defn make-vertices [{:keys [vertices edges]}]
  (for [[vertex-id _ :as all] vertices]
    (conj all (inflow edges vertex-id))))



(defn draw-world [world-state]
  (dommy/replace! (dommy.core/sel1 :#forsvg)
                  (crate/html [:div#forsvg
                               (make-svg (map-size world-state)
                                         (make-edges world-state)
                                         (make-vertices world-state))])))



(defn flip-edge [world-state [from to color :as edge]]
  (if (ok-to-flip? world-state edge)
    [to from color]
    edge))

(defn move-the-vertex [world-state event]
  (let [moving (:vertex-moving world-state)
        rect (.getBoundingClientRect (dommy.core/sel1 :svg))
        pos [(- (.-clientX event) (.-left rect))
             (- (.-clientY event) (.-top rect))]]
    (-> world-state
        (update-in [:vertices moving 1] (constantly pos))
        (update-in [:moving] not))))


(defn update-state [event world-state]
  (let [clicked-what (event->targetid event)
        constrained-flip (partial flip-edge world-state)
        is-edge? (re-matches #"edge.*" clicked-what)
        is-vertex? (re-matches #"vertex.*" clicked-what)]
    (cond
      (:moving world-state) (move-the-vertex world-state event)
      is-edge? (update-in world-state [:edges clicked-what] constrained-flip)
      is-vertex? (merge world-state {:moving true
                                     :vertex-moving clicked-what})
      :else world-state)))


(defn parse-state [state]
  (let [named-edges (update-in state [:edges]
                               (comp name-vertices-in-all-edges name-edges))
        named-vertices (update-in named-edges [:vertices] name-vertices)
        added-moving (merge named-vertices {:vertex-moving nil
                                            :moving false})]
    added-moving))


(go
  (let [read-state (reader/read-string (<! (GET "./state.edn")))]
    (big-bang!
      :initial-state (parse-state read-state)
      :to-draw draw-world
      :on-click update-state))
  )
