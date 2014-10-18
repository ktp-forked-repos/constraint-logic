(ns constraint.core
  (:require [dommy.core :as dommy]
            [crate.core :as crate]))

(.appendChild (dommy.core/sel1 :.forsvg)
              (crate/html [:svg:svg
                           {:width 1000 :height 1000}
                           [:svg:defs
                            [:svg:marker
                             {:id "markerCircle"
                              :markerWidth 8
                              :markerHeight 8
                              :refX 5
                              :refY 5}
                             [:svg:circle {:cx 5
                                           :cy 5
                                           :r 3
                                           :stroke "none"
                                           :fill "#000000"}]
                             ]
                            ]
                           [:svg:path
                            {:d "M100,10 L150,10 L150,60"
                             ; :style (str "stroke: #6666ff; stroke-width: 1px; fill: none;"
                             ;             " marker-start: url (#markerCircle); "
                             ;             " marker-end: url (#markerCircle); ")
                             :stroke "#6666ff"
                             :stroke-width "1px"
                             :fill "none"
                             :marker-start "url(#markerCircle)"
                             :marker-end "url(#markerCircle)"
                             }]
                           [:svg:circle {:id "button"
                                         :fill "yellow"
                                         :stroke "green"
                                         :stroke-width 4
                                         :cx 50 :cy 50
                                         :r 50
                                         }]]))

(defn click-handler [e]
  (js/alert "123"))



(dommy.core/listen! (dommy.core/sel1 :circle) :click click-handler)

(dommy.core/append (dommy.core/parent (dommy.core/sel1 :circle))
                   (crate/html
                     [:svg:line]))

