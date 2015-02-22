(ns constraint.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])

  (:require
    [constraint.svg :refer [make-svg]]
    [constraint.edit :refer [handle-editing]]
    [constraint.common :refer [vert-id
                               str-vert-id
                               edge-id
                               ok-to-flip?]]
    [dataview.loader :refer [fetch-text]]
    [clojure.string :as string]
    [dommy.core :as dommy]
    [crate.core :as crate]
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



(defn event->targetid [e]
  (-> e
      (js->clj)
      (.-target)
      (.-id)))


(defn flip [function]
  (fn
    ([] (function))
    ([x] (function x))
    ([x y] (function y x))))


(defn print-state [world-state]
  (let [textarea       (dommy.core/sel1 :#data)
        newlined-state (string/replace (str world-state) #", " ",\n")]
    (dommy/set-text! textarea newlined-state)))


(defn draw-world [world-state]
  (print-state world-state)
  (let [new-hiccup [:div#forsvg (make-svg world-state)]
        old-svg    (dommy.core/sel1 :#forsvg)
        new-svg    (crate/html new-hiccup)]
    (dommy/replace! old-svg new-svg)))


(defn flip-edge [world-state [from to color player :as edge]]
  (if (ok-to-flip? world-state edge)
    [to from color player]
    edge))


(defn reset-edit [state]
  (merge state {:editing? false
                :selected nil}))

(defn toggle-button-text
  [buttid condition valid invalid]
  (dommy/set-text! (dommy.core/sel1 buttid)
                   (if-not condition
                     valid
                     invalid)))

(defn toggle-editing [world-state]
  (toggle-button-text :#edit
                      (:editing? world-state)
                      "go play"
                      "go edit")
  (if-not (:editing? world-state)
    (update-in world-state [:editing?] not)
    (reset-edit world-state)))


(defn flip-update-edge [clicked-what world-state]
  (let [clicked-edge [:edges clicked-what]
        flip-it      (partial flip-edge world-state)]
    (update-in world-state clicked-edge flip-it)))


(defn toggle-random
  [world-state]
  (toggle-button-text :#auto
                      (:random? world-state)
                      "go manual"
                      "go auto")
  (update-in world-state [:random?] not))

(defn handle-playing [clicked-what world-state]
  (let [clicked-edge? (re-matches #"edge.*" clicked-what)]
    (if clicked-edge?
      (flip-update-edge clicked-what world-state)
      world-state)))


(defn handle-button-click
  [clicked-what world-state]
  (let [clicked-edit? (re-matches #"edit" clicked-what)
        clicked-auto? (re-matches #"auto" clicked-what)]
      (cond
        clicked-edit? (toggle-editing world-state)
        clicked-auto? (toggle-random world-state)
        :else         world-state)))


(defn clicked-button?
  [event]
  (->> event
       .-target
       .-nodeName
       (re-matches #"(?i)button")))


(defn update-state [event world-state]
  (let [clicked-what (event->targetid event)]
    (if (clicked-button? event)
      (handle-button-click clicked-what world-state)
      (if (:editing? world-state)
        (handle-editing clicked-what event world-state)
        (handle-playing clicked-what world-state)))))



(defn reset-random
  [state]
  (merge state {:random? false}))

(def interval (atom 10))

(def ticker (atom @interval))

(defn get-legal-moves
  [world-state]
  (filter
   #(ok-to-flip? world-state (second %))
   (world-state :edges)))

(defn get-random-legal-move-name
  [world-state]
  (some->> world-state
       get-legal-moves
       seq
       rand-nth
       first))


(defn move-randomly
  [world-state]
  (swap! ticker dec)
  (if (neg? @ticker)
    (do 
      (reset! ticker @interval)
      (if-let [m (get-random-legal-move-name world-state)]
        (flip-update-edge m world-state)
        (:initial world-state)))
    world-state))


(defn random-move 
  [_ world-state]
  (if (:random? world-state)
    (move-randomly world-state)
    world-state))


(defn change-interval
  []
  (->> (dommy.core/sel1 :#randomrange)
       .-value
       js/parseInt
       (- 60)
       (reset! interval)))

(defn listen-on-slider-change
  []
  (dommy/listen! (dommy.core/sel1 :#randomrange) :change
                 change-interval))

(defn remember-initial-state
  [world-state]
  (merge world-state
         {:initial (dissoc world-state :initial)}))


(defn parse-state
  [state]
  (->>
   state
   reader/read-string
   reset-edit
   reset-random
   remember-initial-state))

(go
  (listen-on-slider-change)
  (let [read-state (parse-state (<! (GET "./state.edn")))]
    (big-bang!
      :initial-state read-state 
      :to-draw draw-world
      :on-tick random-move
      :on-click update-state)))
