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



(defn prepare-edges [{:keys [vertices edges] :as world-state}]
  (let [locations (fmap second vertices)]
    (fmap (partial prepare-edge locations world-state) edges)))



(defn flip [function]
  (fn
    ([] (function))
    ([x] (function x))
    ([x y] (function y x))))



(defn prepare-vertices [{:keys [vertices edges]}]
  (for [[vertex-id _ :as all] vertices]
    (update-in all [1]
               (partial (flip conj) (inflow edges vertex-id)))))



(defn draw-world [world-state]
  (dommy/set-text! (dommy.core/sel1 :#data)
                              (string/replace (str world-state)
                                              #", "
                                              ", \n"))
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


(defn handle-playing [clicked-what world-state]
  (let [clicked-edit? (re-matches #"edit" clicked-what)
        clicked-edge? (re-matches #"edge.*" clicked-what)]
    (cond
      clicked-edit? (update-in world-state [:editing?] not)
      clicked-edge? (update-in world-state
                               [:edges clicked-what]
                               (partial flip-edge world-state))
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


(defn parse-state [state]
  (let [named-edges (update-in state [:edges]
                               (comp name-vertices-in-all-edges name-edges))
        named-vertices (update-in named-edges [:vertices] name-vertices)
        added-edit (merge named-vertices {:editing? false
                                          :selected nil})]
    added-edit))


(go
  (let [read-state (reader/read-string (<! (GET "./state.edn")))]
    (big-bang!
      :initial-state (parse-state read-state)
      :to-draw draw-world
      :on-click update-state))
  )

