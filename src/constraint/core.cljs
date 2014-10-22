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



(defn name-edges [bare-edges]
  (let [edge-ids (map (partial str "edge") (range))]
    (into {}
        (map vector edge-ids bare-edges))))



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
       (map second)
       (apply map max)
       (map + [50 50])))



(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))



(defn make-edges [{:keys [vertices edges] :as world-state}]
  (let [locations (mapv second vertices)]
    (fmap (partial prepare-edge locations world-state) edges)))



(defn make-vertices [{:keys [vertices edges]}]
  (map vector
       (range)
       (map (partial inflow edges) (range))
       vertices))



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



(defn update-state [event world-state]
  (let [clicked-what (event->targetid event)
        constrained-flip (partial flip-edge world-state)]
    (if (re-matches #"edge.*" clicked-what)
      (update-in world-state [:edges clicked-what] constrained-flip)
      world-state)))



(go
  (let [state (update-in (reader/read-string (<! (GET "./state.edn")))
                         [:edges] name-edges)]
    (big-bang!
      :initial-state state
      :to-draw draw-world
      :on-click update-state))
  )
