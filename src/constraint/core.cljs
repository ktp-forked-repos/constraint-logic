(ns constraint.core
  (:require [monet.canvas :as canvas]))

(def canvas-dom (.getElementById js/document "canvas"))

(def monet-canvas (canvas/init canvas-dom "2d"))

(canvas/add-entity monet-canvas :background
                   (canvas/entity {:x 0 :y 0
                                   :w 100 :h 100} ; val
                                  nil                       ; update function
                                  (fn [ctx val]             ; draw function
                                    (-> ctx
                                        (canvas/fill-style "#191d21")
                                        (canvas/fill-rect val)))))


(canvas/add-entity monet-canvas :asdf
                   (canvas/entity {:x 50 :y 50
                                   :w 100 :h 100
                                   :r 30} ; val
                                  nil                       ; update function
                                  (fn [ctx val]             ; draw function
                                    (-> ctx
                                        ; (canvas/rounded-rect val)
                                        (canvas/fill-style "green")
                                        (canvas/circle val)
                                        (canvas/fill)
                                        ))))




(.log js/console "Hello World!")
