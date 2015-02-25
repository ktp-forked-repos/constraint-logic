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
    [goog.string :as gstring]
    [goog.string.format]
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


(defn dec-if-matters
  [flips-matter? flips]
  (if flips-matter?
    (dec flips)
    flips))


(defn is-legal?
  [world-state move]
  (and
   (ok-to-flip? world-state move)
   (if (:flips-matter? world-state)
     (pos? (last move))
     true)
   )) 

(defn flip-edge [world-state [from to color player flips :as edge]]
  (if (is-legal? world-state edge)
    [to from color player (dec-if-matters (:flips-matter? world-state) flips)]
    edge))


(defn reset-edit [state]
  (merge state {:editing? false
                :selected nil}))


(defn reset-stats
  [world-state]
  (merge world-state
         {:stats {:runs 0,
                  :success 0},
          :length 0,
          :lengths []}))


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


(defn toggle-flips
  [world-state]
  (toggle-button-text :#flips
                      (:flips-matter? world-state)
                      "make number of flips not matter"
                      "make number of flips matter")
  (update-in world-state [:flips-matter?] not))

(defn handle-playing [clicked-what world-state]
  (let [clicked-edge? (re-matches #"edge.*" clicked-what)]
    (if clicked-edge?
      (flip-update-edge clicked-what world-state)
      world-state)))


(defn reset-to-initial
  [{:keys [initial length lengths stats random?]}]
  (merge initial
         {:lengths lengths,
          :length 0
          :stats stats,
          :random? random?,
          :initial initial}))



(defn remember-initial-state
  [world-state]
  (merge world-state
         {:initial (dissoc world-state :initial)}))



(defn handle-button-click
  [clicked-what world-state]
  (let [what-to-do (condp re-matches clicked-what
                     #"edit"        toggle-editing
                     #"auto"        toggle-random
                     #"save"        remember-initial-state
                     #"manualreset" reset-to-initial
                     #"flips"       toggle-flips
                     identity)]
    (what-to-do world-state)))


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
   (comp (partial is-legal? world-state) second)
   (world-state :edges)))

(defn get-random-legal-move-name
  [world-state]
  (some->> world-state
       get-legal-moves
       seq
       rand-nth
       first))

(defn get-target-edges
  [world-state]
  (into {}
        (filter
         (comp #{:green}
               #(nth % 2)
               second)
         (:edges world-state))))


(defn any-target-flipped?
  [world-state]
  (->>
   [(:initial world-state) world-state]
   (map get-target-edges)
   (apply merge-with not=)
   vals
   (some true?)))


(defn make-coords-map-for-js
  [x y]
  {"color" (if (pos? y) "blue" "red")
   "x"     x
   "y"     (Math/abs y)})

(defn make-a-map-for-graph
  [s]
  (mapv make-coords-map-for-js (range) (take-last 100 s)))

(defn avg
  [coll]
  (if (= 0 (count coll))
    0.0
    (/ (apply + coll) (count coll))))

(defn zero-if-empty-apply
  [f coll]
  (if (empty? coll)
    0
    (apply f coll)))

(defn make-stats-for-js
  [{:keys [lengths]}]
  (let [groupped  (merge {true [], false []} (group-by pos? lengths))
        successes (groupped true)
        failures  (map #(Math/abs %) (groupped false))]
    (gstring/format
     "Runs: %d, win: %d, win max/min: %d/%d, win avg: %.2f\n 
fail: %d, fail max/min: %d/%d, fail avg: %.2f"
    (count lengths)
    (count successes)
    (zero-if-empty-apply max successes)
    (zero-if-empty-apply min successes)
    (avg successes)
    (count failures)
    (zero-if-empty-apply max failures)
    (zero-if-empty-apply min failures)
    (avg failures)
    )
  ))

(defn graph
  [world-state]
  (js/js_graph
   (clj->js (str (make-stats-for-js world-state)))
   (clj->js [{"key"    "Series #1",
              "values" (make-a-map-for-graph (:lengths world-state))
              "color"  "#0000ff"}]))
  world-state)


(defn successful-run
  [world-state]
  (let [inc-stats (partial merge-with + {:runs 1, :success 1})]
    (-> world-state
        reset-to-initial
        (update-in [:stats] inc-stats)
        (update-in [:lengths] conj (:length world-state))
        (assoc-in [:length] 0)
        graph)))


(defn unsuccessful-run
  [world-state]
  (-> world-state
      reset-to-initial
      (update-in [:lengths] conj (- (:length world-state)))
      (assoc-in [:length] 0)
      (update-in [:stats :runs] inc)
      graph))

(defn move-randomly
  [world-state]
  (swap! ticker dec)
  (if (neg? @ticker)
    (do 
      (reset! ticker @interval)
      (if (any-target-flipped? world-state)
        (successful-run world-state)
        (if-let [m (get-random-legal-move-name world-state)]
          (flip-update-edge m
                            (update-in world-state [:length] inc))
          (unsuccessful-run world-state))))
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



(defn make-flips-matter
  [world-state]
  (merge world-state
         {:flips-matter? true}))


(defn parse-state
  [state]
  (->>
   state
   reader/read-string
   reset-edit
   reset-random
   make-flips-matter
   reset-stats
   remember-initial-state))

(go
  (listen-on-slider-change)
  (let [read-state (parse-state (<! (GET "./state.edn")))]
    (big-bang!
      :initial-state read-state 
      :to-draw draw-world
      :on-tick random-move
      :on-click update-state)))
