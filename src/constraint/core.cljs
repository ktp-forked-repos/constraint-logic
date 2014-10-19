(ns constraint.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [dommy.core :as dommy]
            [crate.core :as crate]
            [clojure.string :as string]
            [big-bang.core :refer [big-bang!]]))

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


(def circle-marker
  [:svg:marker
   {:id "Vertex"
    :refX "2"
    :refY "2"
    :markerWidth "5"
    :markerHeight "5"
    :orient "auto"}
   [:svg:circle
    {:fill "black"
     :stroke "none"
     :cx 2 :cy 2
     :r 2}]])


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
    :marker-start "url(#Vertex)"
    :marker-mid "url(#Triangle)"
    :marker-end "url(#Vertex)"}])



(defn event->targetid [e]
  (-> e
      (js->clj)
      (.-target)
      (.-id)))


(def vertices [[0 [40 40]]
               [1 [300 40]]
               [2 [300 300]]
               [1 [40 300]]])


(def bare-edges [[0 1 :red]
                 [1 2 :blue]
                 [2 3 :red]
                 [3 1 :red]])

(def color->value {:red 1
                   :blue 2})

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


(defn make-svg [[width height] edges]
  [:svg:svg {:width width :height height}
   [:svg:defs
    triangle-marker
    circle-marker]
   (map svg-edge edges)])



(defn draw-world [world-state]
  (dommy/replace! (dommy.core/sel1 :#forsvg)
                  (crate/html [:div#forsvg
                                (make-svg [1000 1000]
                                          (make-edges world-state))])))

(defn ok-to-flip? [{:keys [vertices edges]} [left right color]]
  (let [this-value (color->value color)
        entering-right (filter #(= right (second (second %))) edges)
        sum-into-right (apply + (map #(color->value ((second %) 2)) entering-right))
        right-value (first (vertices right))]
    (if (< (- sum-into-right this-value) right-value)
      false
      true)))

(defn flip-edge [world-state [left right color :as edge]]
  (if (ok-to-flip? world-state edge)
    [right left color]
    edge))


(defn update-state [event world-state]
  (let [clicked-what (event->targetid event)
        constrained-flip (partial flip-edge world-state)]
    (if (re-matches #"edge.*" clicked-what)
      (update-in world-state [:edges clicked-what] constrained-flip)
      world-state)))

(go
  (big-bang!
    :initial-state {:vertices vertices
                    :edges (name-edges bare-edges)}
    :to-draw draw-world
    :on-click update-state))
