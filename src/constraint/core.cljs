(ns constraint.core
  (:require [dommy.core :as dommy]
            [crate.core :as crate]
            [clojure.string :as string]))

(def triangle-marker
  [:svg:marker
   {:id "Triangle"
    :viewBox "0 0 10 10"
    :refX "1"
    :refY "5"
    :markerWidth "6"
    :markerHeight "6"
    :orient "auto"}
   [:svg:path {:d "M0,0 L 10,5 L0,10 z"}]]
  )

(defn make-line-dirs [from to]
  (let [middle (map (comp #(/ % 2) +) from to)
        coords (map (partial string/join ",") [from middle to])]
    (string/join " " (map str ["M" "L" "L"] coords))))


(defn line [from to color]
  [:svg:path
   {:d (make-line-dirs from to)
    :stroke color
    :stroke-width "4px"
    :fill "none"
    :marker-mid "url(#Triangle)"}])


(.appendChild (dommy.core/sel1 :.forsvg)
              (crate/html [:svg:svg {:width 1000 :height 1000}
                           [:svg:defs triangle-marker]
                           (line [100, 20] [200, 40] "#6666ff")
                           (line [200, 40] [100, 80] "red")
                           [:svg:circle {:id "button"
                                         :fill "yellow"
                                         :stroke "green"
                                         :stroke-width 4
                                         :cx 50 :cy 50
                                         :r 50}]]))

(defn click-handler [e]
  (js/alert "123"))



(dommy.core/listen! (dommy.core/sel1 :circle) :click click-handler)

(dommy.core/append (dommy.core/parent (dommy.core/sel1 :circle))
                   (crate/html
                     [:svg:line]))

