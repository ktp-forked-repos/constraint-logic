(ns constraint.core
  (:require [dommy.core :as dommy]
            [crate.core :as crate]
            [clojure.string :as string]))

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
    :stroke color
    :stroke-width "10px"
    :fill "none"
    :marker-start "url(#Vertex)"
    :marker-mid "url(#Triangle)"
    :marker-end "url(#Vertex)"}])



(defn e->tid [e]
  (-> e
      (js->clj)
      (.-target)
      (.-id)))

(defn click-handler [e]
  (.log js/console (e->tid e))
  (js/alert "123"))

(def vertex-loc [[40 40]
                 [300 40]
                 [300 300]
                 [40 300]])

(def edges-vec [[0 1 "red"]
                [1 2 "blue"]
                [2 3 "red"]
                [3 1 "red"]])


(defn edge->endpoints [locations [start end color]]
  [(locations start) (locations end) color])


(defn make-edges [edges locations]
  (let [edge-ids (map (partial str "edge") (range))
        edges-with-endpoints (map (partial edge->endpoints locations) edges)]
    (into {} (map vector edge-ids edges-with-endpoints))))



(.log js/console (str (make-edges edges-vec vertex-loc)))

(defn make-svg [[width height] edges]
  [:svg:svg {:width width :height height}
   [:svg:defs
    triangle-marker
    circle-marker]
   (map svg-edge edges)]
  )

(.appendChild (dommy.core/sel1 :.forsvg)
              (crate/html (make-svg [1000 1000]
                                    (make-edges edges-vec vertex-loc)
                                    )))

(map #(dommy.core/listen! % :click click-handler) (dommy.core/sel :.clickable))
; (dommy.core/append (dommy.core/parent (dommy.core/sel1 :circle))
;                    (crate/html
;                      [:svg:line]))

