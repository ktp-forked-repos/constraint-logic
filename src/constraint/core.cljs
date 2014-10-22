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


(defn make-triangle-marker [id arrow]
  [:svg:marker
   {:id id
    :viewBox "-5 -5 20 20"
    :refX "5"
    :refY "5"
    :markerWidth "7"
    :markerHeight "7"
    :orient "auto"}
   arrow])

(def triangle-marker-ok
  (make-triangle-marker "TriangleOK"
        [:svg:path {:fill "white" :stroke "black" :d "M0,0 L 10,5 L0,10 z"}]))

(def triangle-marker-not-ok
  (make-triangle-marker "TriangleNotOK"
    [:svg:path {:fill "black" :stroke "black" :d "M0,0 L 10,5 L0,10 z"}]))


(defn make-line-dirs [from to]
  (let [middle (map (comp #(/ % 2) +) from to)
        coords (map (partial string/join ",") [from middle to])]
    (string/join " " (map str ["M" "L" "L"] coords))))


(defn svg-edge [[id [from to color flippable?]]]
  [:svg:path
   {:class "clickable"
    :id id
    :d (make-line-dirs from to)
    :stroke (name color)
    :stroke-width "10px"
    :fill "none"
    :marker-mid (if flippable?
                  "url(#TriangleOK)"
                  "url(#TriangleNotOK)")
    :opacity 0.6
    }])

(def color->value {:red 1
                   :blue 2})

(defn inflow [edges vertex]
  (let [in-going (for [[_ [_ to color]] edges
                       :when (= to vertex)]
                   (color->value color))]
    (apply + in-going)))

(defn svg-vertex [edges id [weight [x y]]]
  (let [vert-inflow (inflow edges id)
        free (- vert-inflow weight)]
    (list
      [:svg:circle
       {:cx x :cy y
        :r (+ 5 (* weight 10))
        :fill (if (> free 0)
                "white"
                "black ")
        :stroke "black"
        :stroke-width 3
        }]
      [:svg:text
       {:x x :y (+ 5 y)
        :fill "black"
        :text-anchor "middle"}
       (str free) ]))
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


(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))


(defn make-edges [{:keys [vertices edges] :as world-state}]
  (let [locations (mapv second vertices)]
    (fmap (partial prepare-edge locations world-state) edges)
    ))



(defn make-svg [[width height] {:keys [vertices edges] :as world-state}]
  [:svg:svg {:width width :height height}
   [:svg:defs
    triangle-marker-ok
    triangle-marker-not-ok]
   (map svg-edge (make-edges world-state))
   (map (partial svg-vertex edges) (range) vertices)

   ])



(defn draw-world [world-state]
  (dommy/replace! (dommy.core/sel1 :#forsvg)
                  (crate/html [:div#forsvg
                                (make-svg [1000 1000] world-state)])))



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
