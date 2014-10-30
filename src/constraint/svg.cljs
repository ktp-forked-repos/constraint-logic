(ns constraint.svg
  (:require
    [clojure.string :as string]))


(defn make-triangle-marker [id arrow]
  [:svg:marker
   {:id id
    :viewBox "-5 -5 20 20"
    :refX "5"
    :refY "5"
    :markerWidth "5"
    :markerHeight "5"
    :orient "auto"}
   arrow])


(def arrow-path "M0,0 L 10,5 L0,10 z")

(def triangle-marker-ok
  (make-triangle-marker "TriangleOK"
                        [:svg:path {:fill "white"
                                    :stroke "black"
                                    :d arrow-path}]))



(def triangle-marker-not-ok
  (make-triangle-marker "TriangleNotOK"
                        [:svg:path {:fill "black"
                                    :stroke "black"
                                    :d arrow-path}]))



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



(defn svg-vertex [[id [weight [x y]] inflow]]
  (let [free (- inflow weight)]
    (list
      [:svg:g
       [:svg:circle
        {:cx x :cy y
         :r (+ 5 (* weight 10))
         :fill (cond
                 (> free 0) "white"
                 (= free 0) "gray"
                 :else      "red")
         :stroke "black"
         :stroke-width 3
         }]
       [:svg:text
        {:x x :y (+ 5 y)
         :fill "black"
         :text-anchor "middle"}
        (str free)]
       [:svg:circle
        {:class "clickable"
         :id id
         :cx x :cy y
         :r (+ 5 (* weight 10))
         :fill "none"
         :pointer-events "all"}]])))



(defn make-svg [[width height] edges vertices]
  [:svg:svg {:width width :height height}
   [:svg:defs
    triangle-marker-ok
    triangle-marker-not-ok]

   ; this is a workaround for firefoxes resizing of getBoundingClientRect when
   ; moving the svg elements
   [:svg:rect
    {:id "workaround"
     :x 0 :y 0
     :width 1 :height 1}]
   (map svg-edge edges)
   (map svg-vertex vertices)])
