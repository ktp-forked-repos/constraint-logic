(ns constraint.edit
  (:require [constraint.common :refer [edge-id
                                       vert-id]]))

(def vertex-regex (re-pattern (str vert-id ".*")))
(def edge-regex (re-pattern (str edge-id ".*")))



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



(defn get-key [len edge-str]
  (->> edge-str
       (drop len)
       (apply str)
       (js/parseInt)))



(defn largest-key [id coll]
  (let [get-number-part (partial get-key (count id))]
    (-> (map get-number-part (keys coll))
        (sort)
        (reverse)
        (first))))



(defn inc-key [id key-num]
  (str id (inc key-num)))



(defn next-key [id coll]
  (inc-key id (largest-key id coll)))




(defn get-vertex-connections [from to {:keys [edges]}]
  (let [connected-either-way? #{[from to] [to from]}
        get-edge-ends (comp butlast second)
        connected? (comp connected-either-way? get-edge-ends)]
    (filter connected? edges)))



(defn make-new-edge [from to {:keys [edges]}]
  (let [new-key (next-key edge-id edges)]
    [new-key [from to :red]]))


(def first-connection-id
  (comp first first get-vertex-connections))


(defn add-or-delete-edge [from to world-state]
  (let [connection-id (first-connection-id from to world-state)
        add-new-edge #(conj % (make-new-edge from to world-state))
        delete-edge #(dissoc % connection-id)
        add-or-delete (if (nil? connection-id) add-new-edge delete-edge)]
    (update-in world-state [:edges] add-or-delete)))



(defn cycle-vertex-size [selected world-state]
  (let [cycle-between-sizes #(mod (inc %) 3)
        selected-vertex-size [:vertices selected 0]]
    (update-in world-state
               selected-vertex-size
               cycle-between-sizes)))



(defn edit-vertex-or-connections [clicked-vertex world-state]
  (let [selected (:selected world-state)]
    (if (= selected clicked-vertex)
      (cycle-vertex-size selected world-state)
      (add-or-delete-edge selected clicked-vertex world-state))))



(defn event->class [e]
  (-> e
      (js->clj)
      (.-target)
      (.-className)
      (.-baseVal)))

(defn is-connected? [vertex [_ [from to _]]]
  (or (= vertex from)
      (= vertex to)))

(defn all-edge-ids-connected-to [vertex world-state]
  (let [edges (:edges world-state)
        connected-edges (filter (partial is-connected? vertex) edges)]
    (map first connected-edges)))


(defn dissoc-edge [world-state edge-id]
  (update-in world-state [:edges] #(dissoc % edge-id)))


(defn delete-all-connected [id world-state]
  (reduce
    dissoc-edge
    world-state
    (all-edge-ids-connected-to id world-state)))

(defn delete-vertex [id world-state]
  (let [deleted-edges (delete-all-connected id world-state)]
    (update-in deleted-edges
               [:vertices]
               #(dissoc % id))))


(defn handle-selected [clicked-what event world-state]
  (let [clicked-vertex? (re-matches vertex-regex clicked-what)
        clicked-delete? (= "delete" (event->class event))]
    (cond
      clicked-delete? (delete-vertex clicked-what world-state )
      clicked-vertex? (edit-vertex-or-connections clicked-what world-state)
      :else (move-the-vertex event world-state))))



(defn select-vertex [world-state clicked-vertex]
  (merge world-state {:selected clicked-vertex}))



(defn toggle-edge-value [world-state clicked-edge]
  (let [clicked-edge-color [:edges clicked-edge 2]
        toggle-color {:red :blue :blue :red}]
    (update-in world-state clicked-edge-color toggle-color)))



(defn make-new-vertex [where {:keys [vertices]}]
  (let [new-key (next-key vert-id vertices)]
    [new-key [1 where]]))



(defn add-vertex [event world-state]
  (let [where (where-svg-was-clicked event)
        new-vertex (make-new-vertex where world-state)
        add-the-new #(conj % new-vertex)]
    (update-in world-state [:vertices] add-the-new)))



(defn handle-unslected [clicked-what event world-state]
  (let [clicked-edge? (re-matches edge-regex clicked-what)
        clicked-vertex? (re-matches vertex-regex clicked-what)]
    (cond
        clicked-edge? (toggle-edge-value world-state clicked-what)
        clicked-vertex? (select-vertex world-state clicked-what)
        :else (add-vertex event world-state))))



(defn handle-editing [clicked-what event world-state]
  (let [unselect #(merge % {:selected nil})]
    (if (:selected world-state)
      (unselect (handle-selected clicked-what event world-state))
      (handle-unslected clicked-what event world-state))))
