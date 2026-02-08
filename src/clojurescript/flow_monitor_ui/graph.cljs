(ns clojurescript.flow-monitor-ui.graph
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [clojurescript.flow-monitor-ui.layout :as layout]
            [clojurescript.flow-monitor-ui.global :refer [global-state global-pings
                                                          send-socket-data]]
            [clojurescript.flow-monitor-ui.components.modal :as component]
            [clojurescript.flow-monitor-ui.components.log-modal :as log-modal]
            [clojurescript.flow-monitor-ui.utils.helpers :refer [>dis]]))

;; ============================================================================
;; State atoms
;; ============================================================================

(def view-state (r/atom {:x 0 :y 0 :w 1200 :h 800
                         :panning? false
                         :pan-start nil}))

(def search-term (r/atom ""))
(def rank-dir (r/atom :TB))
(def show-edge-labels? (r/atom true))
(def show-buffer-info? (r/atom true))
(def selected-node (r/atom nil))

;; ============================================================================
;; Node type classification
;; ============================================================================

(def node-colors
  {:source  {:fill "#0d2818" :border "#166534" :badge "#22c55e" :label "SRC"}
   :process {:fill "#111833" :border "#2d3f7a" :badge "#6c8cff" :label "PROC"}
   :sink    {:fill "#2a1608" :border "#9a3412" :badge "#f97316" :label "SINK"}})

(defn classify-proc
  "Classify a process as :source, :sink, or :process based on connections."
  [proc-id conns]
  (let [has-incoming (some (fn [[[_ _] [to _]]] (= to proc-id)) conns)
        has-outgoing (some (fn [[[from _] [_ _]]] (= from proc-id)) conns)]
    (cond
      (and (not has-incoming) has-outgoing) :source
      (and has-incoming (not has-outgoing)) :sink
      :else :process)))

;; ============================================================================
;; SVG path computation
;; ============================================================================

(defn edge-path
  "Compute a cubic bezier SVG path between two points.
   direction is :TB or :LR."
  [x1 y1 x2 y2 direction node-width node-height]
  (let [vertical? (= direction :TB)
        ;; Offset start/end from node center to node edge
        [sx sy ex ey]
        (if vertical?
          [x1 (+ y1 (/ node-height 2)) x2 (- y2 (/ node-height 2))]
          [(+ x1 (/ node-width 2)) y1 (- x2 (/ node-width 2)) y2])
        mid-offset (/ (Math/abs (- ey sy)) 2)
        [cx1 cy1 cx2 cy2]
        (if vertical?
          [sx (+ sy mid-offset) ex (- ey mid-offset)]
          [(+ sx mid-offset) sy (- ex mid-offset) ey])]
    (str "M" sx "," sy
         " C" cx1 "," cy1
         " " cx2 "," cy2
         " " ex "," ey)))

(defn edge-midpoint
  "Compute midpoint of the bezier for label placement."
  [x1 y1 x2 y2 direction node-width node-height]
  (let [vertical? (= direction :TB)
        [sx sy ex ey]
        (if vertical?
          [x1 (+ y1 (/ node-height 2)) x2 (- y2 (/ node-height 2))]
          [(+ x1 (/ node-width 2)) y1 (- x2 (/ node-width 2)) y2])]
    {:x (/ (+ sx ex) 2)
     :y (/ (+ sy ey) 2)}))

;; ============================================================================
;; Edge label
;; ============================================================================

(defn edge-label-component
  "SVG group for an edge label at midpoint."
  [mid-x mid-y from-port to-port]
  (let [label-text (str (name from-port) " -> " (name to-port))]
    [:g {:transform (str "translate(" mid-x "," mid-y ")")}
     [:rect {:x -50 :y -10 :width 100 :height 20
             :rx 4 :fill "#1a1a2e" :opacity 0.85}]
     [:text {:x 0 :y 4 :text-anchor "middle"
             :fill "#9ca3af" :font-size 11
             :font-family "monospace"}
      label-text]]))

;; ============================================================================
;; Edge buffer label
;; ============================================================================

(defn buffer-fill-color [buf-count buf-capacity]
  (when (and buf-count buf-capacity (pos? buf-capacity))
    (let [ratio (/ buf-count buf-capacity)]
      (cond
        (>= ratio 0.9) "#ef4444"
        (>= ratio 0.6) "#f59e0b"
        :else          "#22c55e"))))

(defn edge-buffer-label [mid-x mid-y buf-count buf-capacity]
  (when (and buf-capacity (pos? buf-capacity))
    (let [label-text (str buf-count "/" buf-capacity)
          fill-color (or (buffer-fill-color buf-count buf-capacity) "#9ca3af")]
      [:g {:transform (str "translate(" mid-x "," (+ mid-y 18) ")")}
       [:rect {:x -30 :y -9 :width 60 :height 18
               :rx 9 :fill "#0f0f17" :opacity 0.9
               :stroke fill-color :stroke-width 1}]
       [:text {:x 0 :y 4 :text-anchor "middle"
               :fill fill-color :font-size 11
               :font-weight "600" :font-family "monospace"}
        label-text]])))

;; ============================================================================
;; Node component
;; ============================================================================

(defn titleize-keyword [kw]
  (as-> (name kw) $
        (str/replace $ #"-" " ")
        (str/split $ #"\s+")
        (map str/capitalize $)
        (str/join " " $)))

(defn node-component
  "SVG group for a single process node."
  [proc-id pos node-type ping-data is-dimmed is-selected conns]
  (let [{:keys [x y]} pos
        colors (get node-colors node-type)
        {:keys [fill border badge label]} colors
        node-w 200
        node-h 80
        half-w (/ node-w 2)
        half-h (/ node-h 2)
        paused? (= :paused (:clojure.core.async.flow/status ping-data))
        errors (-> @global-state :errors proc-id)
        has-errors? (and errors (pos? (count errors)))]
    [:g {:transform (str "translate(" (- x half-w) "," (- y half-h) ")")
         :style {:cursor "pointer"
                 :opacity (if is-dimmed 0.25 1)}
         :on-click (fn [e]
                     (.stopPropagation e)
                     (reset! selected-node proc-id)
                     (swap! global-state assoc :active-proc-pid proc-id))}
     ;; Main rect
     [:rect {:width node-w :height node-h :rx 8
             :fill fill :stroke border :stroke-width 2}]
     ;; Paused overlay
     (when paused?
       [:rect {:width node-w :height node-h :rx 8
               :fill "url(#paused-pattern)"
               :opacity 0.5}])
     ;; Type badge
     [:g {:transform (str "translate(" (- node-w 36) ",8)")}
      [:rect {:width 28 :height 16 :rx 4 :fill badge}]
      [:text {:x 14 :y 12 :text-anchor "middle"
              :fill "#000" :font-size 9 :font-weight "bold"}
       label]]
     ;; Error indicator
     (when has-errors?
       [:circle {:cx (- node-w 8) :cy (- node-h 8) :r 6
                 :fill "#ef4444"}])
     ;; Process name
     [:text {:x (/ node-w 2) :y (+ (/ node-h 2) 5)
             :text-anchor "middle"
             :fill "#e5e7eb" :font-size 16 :font-weight "600"}
      (titleize-keyword proc-id)]]))

;; ============================================================================
;; Build graph data from flow state
;; ============================================================================

(defn build-graph-data
  "Extract nodes and edges from flow connection data, enriched with buffer info from ping."
  [conns ping]
  (let [node-set (reduce (fn [s [[from _] [to _]]]
                           (-> s (conj from) (conj to)))
                         #{} conns)
        edges (mapv (fn [[[from from-port] [to to-port]]]
                      (let [buf (-> (get ping to)
                                    :clojure.core.async.flow/ins
                                    (get to-port)
                                    :buffer)]
                        {:from from :to to
                         :from-port from-port :to-port to-port
                         :buf-count (:count buf)
                         :buf-capacity (:capacity buf)}))
                    conns)]
    {:nodes (vec node-set)
     :edges edges}))

;; ============================================================================
;; Main graph component
;; ============================================================================

(defn zoom-in! []
  (swap! view-state (fn [vs]
                      (let [cx (+ (:x vs) (/ (:w vs) 2))
                            cy (+ (:y vs) (/ (:h vs) 2))
                            nw (* (:w vs) 0.8)
                            nh (* (:h vs) 0.8)]
                        (assoc vs
                               :w nw :h nh
                               :x (- cx (/ nw 2))
                               :y (- cy (/ nh 2)))))))

(defn zoom-out! []
  (swap! view-state (fn [vs]
                      (let [cx (+ (:x vs) (/ (:w vs) 2))
                            cy (+ (:y vs) (/ (:h vs) 2))
                            nw (* (:w vs) 1.25)
                            nh (* (:h vs) 1.25)]
                        (assoc vs
                               :w nw :h nh
                               :x (- cx (/ nw 2))
                               :y (- cy (/ nh 2)))))))

(defn reset-view! [positions node-w node-h]
  (if (seq positions)
    (let [xs (map :x (vals positions))
          ys (map :y (vals positions))
          min-x (- (apply min xs) (/ node-w 2) 40)
          max-x (+ (apply max xs) (/ node-w 2) 40)
          min-y (- (apply min ys) (/ node-h 2) 40)
          max-y (+ (apply max ys) (/ node-h 2) 40)
          w (max 400 (- max-x min-x))
          h (max 300 (- max-y min-y))]
      (reset! view-state {:x min-x :y min-y :w w :h h
                          :panning? false :pan-start nil}))
    (reset! view-state {:x 0 :y 0 :w 1200 :h 800
                        :panning? false :pan-start nil})))

(defn on-wheel [e]
  (.preventDefault e)
  (let [delta (.-deltaY e)
        rect (.getBoundingClientRect (.-currentTarget e))
        ;; Mouse position relative to SVG element
        mouse-x (- (.-clientX e) (.-left rect))
        mouse-y (- (.-clientY e) (.-top rect))
        el-w (.-width rect)
        el-h (.-height rect)]
    (swap! view-state
           (fn [vs]
             (let [factor (if (pos? delta) 1.1 0.9)
                   ;; Convert mouse position to viewBox coordinates
                   vb-mouse-x (+ (:x vs) (* (/ mouse-x el-w) (:w vs)))
                   vb-mouse-y (+ (:y vs) (* (/ mouse-y el-h) (:h vs)))
                   nw (* (:w vs) factor)
                   nh (* (:h vs) factor)
                   ;; Keep the point under cursor fixed
                   nx (- vb-mouse-x (* (/ mouse-x el-w) nw))
                   ny (- vb-mouse-y (* (/ mouse-y el-h) nh))]
               (assoc vs :w nw :h nh :x nx :y ny))))))

(defn on-mouse-down [e]
  (when (= (.-button e) 0)
    (.preventDefault e)
    (swap! view-state assoc
           :panning? true
           :pan-start {:x (.-clientX e) :y (.-clientY e)})))

(defn on-mouse-move [e]
  (when (:panning? @view-state)
    (let [{:keys [pan-start w h]} @view-state
          rect (.getBoundingClientRect (.-currentTarget e))
          el-w (.-width rect)
          el-h (.-height rect)
          dx (- (.-clientX e) (:x pan-start))
          dy (- (.-clientY e) (:y pan-start))
          ;; Scale mouse delta to viewBox units
          vb-dx (* (/ dx el-w) w)
          vb-dy (* (/ dy el-h) h)]
      (swap! view-state (fn [vs]
                          (-> vs
                              (update :x - vb-dx)
                              (update :y - vb-dy)
                              (assoc :pan-start {:x (.-clientX e)
                                                 :y (.-clientY e)})))))))

(defn on-mouse-up [_e]
  (swap! view-state assoc :panning? false :pan-start nil))

(defn matches-search? [proc-id term]
  (or (str/blank? term)
      (str/includes? (str/lower-case (name proc-id))
                     (str/lower-case term))))

(defn graph-component []
  (let [node-w 200
        node-h 80
        last-positions (r/atom nil)]
    (fn []
      (let [data (:data @global-state)
            conns (:conns data)
            ping (:flow-ping @global-pings)
            direction @rank-dir
            term @search-term
            show-labels? @show-edge-labels?
            show-buffer? @show-buffer-info?
            sel @selected-node]
        (if (empty? conns)
          [:div.flow-graph-container
           [:p {:style {:color "#9ca3af" :text-align "center" :margin-top "100px"}}
            "No flow data available."]]
          (let [{:keys [nodes edges]} (build-graph-data conns ping)
                positions (layout/layout-graph
                            nodes edges
                            {:direction direction
                             :node-width node-w
                             :node-height node-h
                             :roots (:root data)})
                _ (when (not= @last-positions positions)
                    (reset! last-positions positions)
                    ;; Auto-fit on first render or layout change
                    (js/setTimeout #(reset-view! positions node-w node-h) 50))
                {:keys [x y w h]} @view-state]
            [:div.flow-graph-container
             ;; Toolbar
             [:div.flow-graph-toolbar
              ;; Search
              [:div.flow-graph-search
               [:input {:type "text"
                        :placeholder "Search nodes..."
                        :value term
                        :on-change #(reset! search-term (.. % -target -value))}]]
              ;; Direction toggle
              [:div.flow-graph-controls
               [:button {:class (str "ctrl-btn" (when (= direction :TB) " active"))
                         :on-click #(reset! rank-dir :TB)} "TB"]
               [:button {:class (str "ctrl-btn" (when (= direction :LR) " active"))
                         :on-click #(reset! rank-dir :LR)} "LR"]
               [:button {:class (str "ctrl-btn" (when show-labels? " active"))
                         :on-click #(swap! show-edge-labels? not)} "Labels"]
               [:button {:class (str "ctrl-btn" (when show-buffer? " active"))
                         :on-click #(swap! show-buffer-info? not)} "Buffer"]
               [:button.ctrl-btn {:on-click zoom-in!} "+"]
               [:button.ctrl-btn {:on-click zoom-out!} "-"]
               [:button.ctrl-btn {:on-click #(reset-view! positions node-w node-h)} "R"]
               [:button.ctrl-btn {:on-click #(>dis [::log-modal/set-log-modal-visibility true])} "Logs"]]]
             ;; SVG
             [:svg {:view-box (str x " " y " " w " " h)
                    :width "100%" :height "100%"
                    :style {:background "#0f0f17" :display "block"}
                    :on-wheel on-wheel
                    :on-mouse-down on-mouse-down
                    :on-mouse-move on-mouse-move
                    :on-mouse-up on-mouse-up
                    :on-mouse-leave on-mouse-up}
              ;; Defs
              [:defs
               [:marker {:id "arrowhead" :markerWidth 10 :markerHeight 7
                         :refX 10 :refY 3.5 :orient "auto"
                         :markerUnits "strokeWidth"}
                [:polygon {:points "0 0, 10 3.5, 0 7" :fill "#4b5563"}]]
               [:pattern {:id "paused-pattern" :width 4 :height 4
                          :patternUnits "userSpaceOnUse"}
                [:circle {:cx 2 :cy 2 :r 0.8 :fill "rgba(255,255,255,0.3)"}]]]
              ;; Edges
              [:g.edges
               (doall
                 (for [{:keys [from to from-port to-port buf-count buf-capacity]} edges
                       :let [from-pos (get positions from)
                             to-pos (get positions to)]
                       :when (and from-pos to-pos)]
                   ^{:key (str from "-" from-port "-" to "-" to-port)}
                   [:g
                    [:path {:d (edge-path (:x from-pos) (:y from-pos)
                                          (:x to-pos) (:y to-pos)
                                          direction node-w node-h)
                            :fill "none"
                            :stroke "#4b5563"
                            :stroke-width 2
                            :marker-end "url(#arrowhead)"}]
                    (when (or show-labels? show-buffer?)
                      (let [mid (edge-midpoint (:x from-pos) (:y from-pos)
                                               (:x to-pos) (:y to-pos)
                                               direction node-w node-h)]
                        [:<>
                         (when show-labels?
                           [edge-label-component (:x mid) (:y mid) from-port to-port])
                         (when show-buffer?
                           [edge-buffer-label (:x mid) (:y mid) buf-count buf-capacity])]))]))]
              ;; Nodes
              [:g.nodes
               (doall
                 (for [node-id nodes
                       :let [pos (get positions node-id)]
                       :when pos]
                   ^{:key node-id}
                   [node-component
                    node-id
                    pos
                    (classify-proc node-id conns)
                    (get ping node-id)
                    (not (matches-search? node-id term))
                    (= sel node-id)
                    conns]))]]]))))))
