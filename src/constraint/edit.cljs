(ns constraint.edit
  (:require [constraint.common :refer [edge-id]]))

(def edge-id-len (count edge-id))

(defn where-svg-was-clicked [event]
  (let [svg-rect (.getBoundingClientRect (dommy.core/sel1 :svg))
        click-position [(.-clientX event) (.-clientY event)]
        rect-position [(.-left svg-rect) (.-top svg-rect)]]
    (map - click-position rect-position)))


(defn move-the-vertex [event world-state]
  (let [moving (:selected world-state)
        position-to-update [:vertices moving 1]
        where (where-svg-was-clicked event)]
    (update-in world-state position-to-update (constantly where))))

(defn get-key [edge-str]
  (->> edge-str
       (drop edge-id-len)
       (apply str)
       (js/parseInt)))

(defn largest-key [{:keys [edges]}]
  (-> (map get-key (keys edges))
      (sort)
      (reverse)
      (first)))

(defn next-key [key-num]
  (str edge-id (inc key-num)))

(defn first-connected-edge [from to {:keys [edges]}]
  (let [connected-either-way? #{[from to] [to from]}
        get-edge-ends (comp butlast second)
        connected? (comp connected-either-way? get-edge-ends)]
    (first (filter connected? edges))))

(defn make-new-edge [from to world-state]
  (let [next-to-largest-key (next-key (largest-key world-state))]
    [next-to-largest-key [from to :red]])
  )

(defn add-or-delete-edge [from to world-state]
  (let [connected-id (first (first-connected-edge from to world-state))
        add-new-edge #(conj % (make-new-edge from to world-state))
        delete-edge #(dissoc % connected-id)
        add-or-delete (if (nil? connected-id) add-new-edge delete-edge)]
    (update-in world-state [:edges] add-or-delete)))

(defn toggle-vertex-size [selected world-state]
  (let [toggle-between-sizes #(inc (mod % 2))
        selected-vertex-size [:vertices selected 0]]
    (update-in world-state
               selected-vertex-size
               toggle-between-sizes)))

(defn edit-vertex-or-connections [clicked-vertex world-state]
  (let [selected (:selected world-state)]
    (if (= selected clicked-vertex)
      (toggle-vertex-size selected world-state)
      (add-or-delete-edge selected clicked-vertex world-state))))


(defn handle-selected [clicked-what event world-state]
  (let [clicked-vertex? (re-matches #"vertex.*" clicked-what)]
    (if clicked-vertex?
      (edit-vertex-or-connections clicked-what world-state)
      (move-the-vertex event world-state))))


(defn handle-editing [clicked-what event world-state]
  (let [unselect #(merge % {:selected nil})
        clicked-edge? (re-matches #"edge.*" clicked-what)
        clicked-vertex? (re-matches #"vertex.*" clicked-what) ]
    (if (:selected world-state)
      (unselect (handle-selected clicked-what event world-state))
      (cond
        clicked-edge? (update-in world-state [:edges clicked-what 2]
                                 #(if (= :blue %) :red :blue))
        clicked-vertex? (merge world-state {:selected clicked-what})
        :else world-state))))
