(ns constraint.core
  (:require [dommy.core :as dommy]
            [crate.core :as crate]))

(.appendChild (dommy.core/sel1 :.forsvg)
              (crate/html [:svg:svg
                           {:width 1000 :height 1000}
                           [:svg:defs
                            [:svg:marker
                             {:id "Triangle"
                              :viewBox "0 0 10 10" 
                              :refX "1"
                              :refY "5"
                              :markerWidth "6"
                              :markerHeight "6"
                              :orient "auto"}
                             [:svg:path {:d "M0,0 L 10,5 L0,10 z"}]]
                            ]
                           [:svg:path
                            {:d "M100,20 L150,20 L200,20"
                             :stroke "#6666ff"
                             :stroke-width "4px"
                             :fill "none"
                             :marker-mid "url(#Triangle)"}]
                           [:svg:path
                            {:d "M200,40 L150,60 L100,80"
                             :stroke "#6666ff"
                             :stroke-width "4px"
                             :fill "none"
                             :marker-mid "url(#Triangle)"}]
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

