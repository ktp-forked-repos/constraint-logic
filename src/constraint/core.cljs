(ns constraint.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])

  (:require
    [constraint.svg :refer [make-svg]]
    [constraint.edit :refer [handle-editing]]
    [constraint.common :refer [vert-id
                               str-vert-id
                               edge-id
                               color->value]]
    [dataview.loader :refer [fetch-text]]
    [clojure.string :as string]
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


(def map-padding [50 50])


(defn inflow [edges vertex]
  (let [incoming-values (for [[_ [_ to color]] edges
                              :when (= to vertex)]
                          (color->value color))]
    (apply + incoming-values)))



(defn event->targetid [e]
  (-> e
      (js->clj)
      (.-target)
      (.-id)))



(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))


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
       (map + map-padding)))



(defn prepare-edges [{:keys [vertices edges] :as world-state}]
  (let [locations (fmap second vertices)]
    (fmap (partial prepare-edge locations world-state) edges)))



(defn flip [function]
  (fn
    ([] (function))
    ([x] (function x))
    ([x y] (function y x))))



(defn prepare-vertices [{:keys [vertices edges]}]
  (for [[vertex-id vertex-data] vertices
        :let [incoming-flow (inflow edges vertex-id)]]
    `(~vertex-id ~@vertex-data ~incoming-flow)))


(defn print-state [world-state]
  (let [textarea (dommy.core/sel1 :#data)
        newlined-state (string/replace (str world-state) #", " ",\n")]
    (dommy/set-text! textarea newlined-state)))


(defn draw-world [world-state]
  (print-state world-state)
  (dommy/replace! (dommy.core/sel1 :#forsvg)
                  (crate/html [:div#forsvg
                               (make-svg (map-size world-state)
                                         (prepare-edges world-state)
                                         (prepare-vertices world-state)
                                         (:editing? world-state)
                                         (:selected world-state))])))


(defn flip-edge [world-state [from to color :as edge]]
  (if (ok-to-flip? world-state edge)
    [to from color]
    edge))

(defn toggle-editing [world-state]
  (update-in world-state [:editing?] not))

(defn flip-update-edge [clicked-what world-state]
  (let [clicked-edge [:edges clicked-what]
        flip-it (partial flip-edge world-state)]
    (update-in world-state clicked-edge flip-it)))

(defn handle-playing [clicked-what world-state]
  (let [clicked-edit? (re-matches #"edit" clicked-what)
        clicked-edge? (re-matches #"edge.*" clicked-what)]
    (cond
      clicked-edit? (toggle-editing world-state)
      clicked-edge? (flip-update-edge clicked-what world-state)
      :else world-state)))


(defn update-state [event world-state]
  (let [clicked-what (event->targetid event)
        clicked-edit? (re-matches #"edit" clicked-what)
        is-vertex? (re-matches #"vertex.*" clicked-what)]
    (if (:editing? world-state)
      (if clicked-edit?
        (merge world-state {:selected nil
                            :editing? false})
        (handle-editing clicked-what event world-state))
      (handle-playing clicked-what world-state))))


(defn reset-edit [state]
  (merge state {:editing? false
                :selected nil}))

(go
  (let [read-state (reader/read-string (<! (GET "./state.edn")))]
    (big-bang!
      :initial-state (reset-edit read-state)
      :to-draw draw-world
      :on-click update-state))
  )

