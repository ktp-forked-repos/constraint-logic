(ns constraint.svg)


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
                        [:svg:path {:fill "GreenYellow"
                                    :stroke "black"
                                    :d arrow-path}]))



(def triangle-marker-not-ok
  (make-triangle-marker "TriangleNotOK"
                        [:svg:path {:fill "gray"
                                    :stroke "black"
                                    :d arrow-path}]))



(defn make-line-dirs [[xfrom yfrom] [xto yto]]
  (let [xmiddle (/ (+ xfrom xto) 2)
        ymiddle (/ (+ yfrom yto) 2)]
    (str "M" xfrom   "," yfrom   " "
         "L" xmiddle "," ymiddle " "
         "L" xto     "," yto     " ")))



(defn svg-edge [[id [from to color flippable? player]]]
  [:svg:g
   {:class "edge"}
   [:svg:path
    {:class "value"
     :id id
     :d (make-line-dirs from to)
     :stroke (name color)
     :stroke-width "12px"
     :fill "none"
     :marker-mid (if flippable?
                   "url(#TriangleOK)"
                   "url(#TriangleNotOK)")
     :opacity 0.6
     }]
   [:svg:path
    {:class "player"
     :id id
     :d (make-line-dirs from to)
     :stroke (cond (= 1 player) "white"
                   (= 2 player) "black"
                   :else "none")
     :stroke-width "5px"
     :fill "none"
     :opacity 1}]])

(defn vertex-fill-color [selected? free]
  (cond
    selected?  "yellow"
    (> free 0) "white"
    (= free 0) "gray"
    :else      "red"))

(defn vertex [[x y] free size selected?]
  (list [:svg:circle {:cx x :cy y
                      :r size
                      :fill (vertex-fill-color selected? free)
                      :stroke "black"
                      :stroke-width 3}]
        [:svg:text {:x x :y (+ 5 y)
                    :fill "black"
                    :text-anchor "middle"}
         (str free)]))

(defn cellophane [id [x y] size]
  "creates a clickable invisible circle, a layer above the vertex to handle problems with clicking on the internal text"
  [:svg:circle
   {:class "cellophane"
    :id id
    :cx x :cy y
    :r size
    :fill "none"
    :pointer-events "all"}])

(defn svg-vertex [selected [id [weight pos inflow]]]
  (let [free (- inflow weight)
        size (+ 5 (* weight 10))
        selected? (= id selected)]
    [:svg:g
     (vertex pos free size selected?)
     (cellophane id pos size)]))

(defn edit-button [x y width height editing?]
  [:svg:g
   [:svg:rect
    {:x x :y y
     :width width :height height
     :fill (if editing? "green" "blue")}]
   [:svg:text {:x (+ x (/ width 2))
               :y (+ y 5 (/ height 2))
               :fill "white"
               :text-anchor "middle"}
    (if editing?
      "go play"
      "go edit")]
   [:svg:rect
    {:class "cellophane"
     :id "edit"
     :x x :y y
     :width width :height height
     :stroke-width 1
     :stroke "black"
     :fill "none"
     :pointer-events "all"}] ])


(defn make-svg [[width height] edges vertices editing? selected]
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

   (edit-button 10 10 100 20 editing?)


   (map svg-edge edges)
   (map (partial svg-vertex selected) vertices)])
