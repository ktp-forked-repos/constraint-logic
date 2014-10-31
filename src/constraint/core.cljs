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
  (dommy/replace! (dommy.core/sel1 :#forsvg)
                  (crate/html [:div#forsvg
                               (make-svg (map-size world-state)
                                         (prepare-edges world-state)
                                         (prepare-vertices world-state)
                                         (:editing? world-state))])))



(defn flip-edge [world-state [from to color :as edge]]
  (if (ok-to-flip? world-state edge)
    [to from color]
    edge))

(defn move-the-vertex [world-state event]
  (let [moving (:selected world-state)
        rect (.getBoundingClientRect (dommy.core/sel1 :svg))
        pos [(- (.-clientX event) (.-left rect))
             (- (.-clientY event) (.-top rect))]]
    (update-in world-state [:vertices moving 1] (constantly pos))))

(defn connect-or-disconnect [from to world-state]
  (let [connections (filter #(#{[from to] [to from]}
                                      (butlast (second %))) (:edges world-state))
        largest-key (-> (:edges world-state)
                        (keys)
                        (sort)
                        (reverse)
                        (first))
        next-key (str edge-id (->> largest-key
                                   (drop 4)
                                   (apply str)
                                   (js/parseInt)
                                   (inc)))]
    (if (empty? connections)
      (update-in world-state [:edges] #(conj % [next-key [from to :red]]))
      (update-in world-state [:edges] #(dissoc % (first (first connections)))))
    )
  )

(defn edit-vertex-or-connections [clicked-vertex world-state]
  (let [selected (:selected world-state)]
    (if (= selected clicked-vertex)
      (update-in world-state [:vertices selected 0] #(inc (mod % 2)))
      (connect-or-disconnect selected clicked-vertex world-state)
      )))

(defn handle-editing [event world-state]
  (let [clicked-what (event->targetid event)
        clicked-edge? (re-matches #"edge.*" clicked-what)
        clicked-vertex? (re-matches #"vertex.*" clicked-what)]
    (if (:selected world-state)
      (merge (if clicked-vertex?
               (edit-vertex-or-connections clicked-what world-state)
               (move-the-vertex world-state event))
        {:selected nil})
      (cond
        clicked-edge? (update-in world-state [:edges clicked-what 2]
                                 #(if (= :blue %) :red :blue))
        clicked-vertex? (merge world-state {:selected clicked-what})
        :else world-state))))


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
        (handle-editing event world-state))
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

