(ns constraint.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])

  (:require
    [dataview.loader :refer [fetch-text]]
    [dommy.core :as dommy]
    [crate.core :as crate]
    [clojure.string :as string]
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


(def triangle-marker
  [:svg:marker
   {:id "Triangle"
    :viewBox "0 0 10 10"
    :refX "3"
    :refY "5"
    :markerWidth "3"
    :markerHeight "3"
    :orient "auto"}
   [:svg:path {:d "M0,0 L 10,5 L0,10 z"}]])


(defn make-line-dirs [from to]
  (let [middle (map (comp #(/ % 2) +) from to)
        coords (map (partial string/join ",") [from middle to])]
    (string/join " " (map str ["M" "L" "L"] coords))))


(defn svg-edge [[id [from to color]]]
  [:svg:path
   {:class "clickable"
    :id id
    :d (make-line-dirs from to)
    :stroke (name color)
    :stroke-width "10px"
    :fill "none"
    :marker-mid "url(#Triangle)"
    :opacity 0.6
    }])

(def color->value {:red 1
                   :blue 2})

(defn svg-vertex [edges id [weight [x y]]]
  (let [vert-inflow (inflow edges id)]
    (list
      [:svg:circle
       {:cx x :cy y
        :r 25
        :fill "white"
        :stroke "black"
        :stroke-width 3
        }]
      [:svg:text
       {:x x :y (+ 5 y)
        :fill "black"
        :text-anchor "middle"}
       (str id " (" vert-inflow "/" weight ")") ]))
  )

(defn event->targetid [e]
  (-> e
      (js->clj)
      (.-target)
      (.-id)))


(defn name-edges [bare-edges]
  (let [edge-ids (map (partial str "edge") (range))]
    (into {}
        (map vector edge-ids bare-edges))))


(defn locate-vertices [locations [start end color]]
  [(locations start)
   (locations end)
   color])


(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))


(defn vertex-id->points [named-edges locations]
  (fmap (partial locate-vertices locations) named-edges))


(defn make-edges [{:keys [vertices edges]}]
  (vertex-id->points edges (mapv second vertices)))



(defn make-svg [[width height] {:keys [vertices edges] :as world-state}]
  [:svg:svg {:width width :height height}
   [:svg:defs
    triangle-marker]
   (map svg-edge (make-edges world-state))
   (map (partial svg-vertex edges) (range) vertices)

   ])



(defn draw-world [world-state]
  (dommy/replace! (dommy.core/sel1 :#forsvg)
                  (crate/html [:div#forsvg
                                (make-svg [1000 1000] world-state)])))

(defn inflow [edges vertex]
  (let [in-going (for [[_ [_ to color]] edges
                       :when (= to vertex)]
                   (color->value color))]
    (apply + in-going)))

(defn ok-to-flip? [{:keys [vertices edges]} [_ to color]]
  (let [edge-value (color->value color)
        to-inflow (inflow edges to)
        to-vertex-value (first (vertices to))]
    (>= (- to-inflow edge-value) to-vertex-value)))

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
  (let [vertices (<! (GET "./vertices.edn"))
        bare-edges (<! (GET "./edges.edn"))]

    (big-bang!
      :initial-state {:vertices (reader/read-string vertices)
                      :edges (name-edges (reader/read-string bare-edges))}
      :to-draw draw-world
      :on-click update-state))
  )
