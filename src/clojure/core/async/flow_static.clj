(ns clojure.core.async.flow-static
  (:require
    [hiccup2.core :as h]
    [clojure.string :as str]))

;; ============================================================================
;; Layout algorithm (pure Clojure version of layout.cljs)
;; ============================================================================

(defn- in-degree [nodes edges]
  (let [base (into {} (map (fn [n] [n 0]) nodes))]
    (reduce (fn [m {:keys [to]}]
              (update m to (fnil inc 0)))
            base edges)))

(defn- topological-ranks [nodes edges]
  (let [in-deg (in-degree nodes edges)
        adj (reduce (fn [m {:keys [from to]}]
                      (update m from (fnil conj []) to))
                    {} edges)
        initial (filterv (fn [n] (zero? (get in-deg n 0))) nodes)
        queue (into clojure.lang.PersistentQueue/EMPTY initial)]
    (loop [q queue
           ranks {}
           in-deg in-deg]
      (if (empty? q)
        (reduce (fn [r n]
                  (if (contains? r n) r (assoc r n 0)))
                ranks nodes)
        (let [node (peek q)
              q (pop q)
              rank (reduce (fn [mx pred]
                             (if-let [pr (get ranks pred)]
                               (max mx (inc pr))
                               mx))
                           0
                           (keep (fn [{:keys [from to]}]
                                   (when (= to node) from))
                                 edges))
              ranks (assoc ranks node rank)
              neighbors (get adj node [])
              [q' in-deg']
              (reduce (fn [[q d] nb]
                        (let [d' (update d nb dec)]
                          (if (zero? (get d' nb))
                            [(conj q nb) d']
                            [q d'])))
                      [q in-deg] neighbors)]
          (recur q' ranks in-deg'))))))

(defn- assign-ranks [nodes edges roots]
  (let [base-ranks (topological-ranks nodes edges)]
    (if (seq roots)
      (let [min-root-rank (apply min (map #(get base-ranks % 0) roots))
            shifted (reduce-kv (fn [m k v] (assoc m k (- v min-root-rank))) {} base-ranks)]
        (reduce-kv (fn [m k v] (assoc m k (max 0 v))) {} shifted))
      base-ranks)))

(defn- group-by-rank [ranks]
  (->> (group-by val ranks)
       (map (fn [[rank entries]] [rank (mapv first entries)]))
       (sort-by first)))

(defn- barycenter [node edges rank-nodes-positions]
  (let [neighbors (keep (fn [{:keys [from to]}]
                          (cond
                            (= to node) (get rank-nodes-positions from)
                            (= from node) (get rank-nodes-positions to)
                            :else nil))
                        edges)]
    (when (seq neighbors)
      (/ (reduce + 0 neighbors) (count neighbors)))))

(defn- order-rank [rank-nodes edges fixed-positions]
  (let [scored (map (fn [n]
                      [n (or (barycenter n edges fixed-positions)
                             (get fixed-positions n 0))])
                    rank-nodes)]
    (mapv first (sort-by second scored))))

(defn- crossing-minimization [ranked-groups edges sweeps]
  (let [initial-order (into {} (map (fn [[rank nodes]] [rank (vec nodes)]) ranked-groups))
        ranks-sorted (mapv first (sort-by first ranked-groups))]
    (loop [order initial-order
           sweep 0]
      (if (>= sweep sweeps)
        order
        (let [order-fwd
              (reduce (fn [ord i]
                        (if (zero? i)
                          ord
                          (let [prev-rank (nth ranks-sorted (dec i))
                                curr-rank (nth ranks-sorted i)
                                prev-nodes (get ord prev-rank)
                                prev-positions (into {} (map-indexed (fn [idx n] [n idx]) prev-nodes))
                                reordered (order-rank (get ord curr-rank) edges prev-positions)]
                            (assoc ord curr-rank reordered))))
                      order
                      (range (count ranks-sorted)))
              order-bwd
              (reduce (fn [ord i]
                        (if (= i (dec (count ranks-sorted)))
                          ord
                          (let [next-rank (nth ranks-sorted (inc i))
                                curr-rank (nth ranks-sorted i)
                                next-nodes (get ord next-rank)
                                next-positions (into {} (map-indexed (fn [idx n] [n idx]) next-nodes))
                                reordered (order-rank (get ord curr-rank) edges next-positions)]
                            (assoc ord curr-rank reordered))))
                      order-fwd
                      (reverse (range (count ranks-sorted))))]
          (recur order-bwd (inc sweep)))))))

(defn- assign-coordinates [rank-order node-width node-height x-gap y-gap]
  (reduce-kv
    (fn [positions rank nodes]
      (let [n (count nodes)
            total-span (+ (* n node-width) (* (dec n) x-gap))
            start (- (/ total-span 2))]
        (reduce
          (fn [pos [idx node]]
            (assoc pos node {:x (+ start (* idx (+ node-width x-gap)))
                             :y (* rank (+ node-height y-gap))}))
          positions
          (map-indexed vector nodes))))
    {}
    rank-order))

(defn- layout-graph [nodes edges opts]
  (let [{:keys [node-width node-height x-gap y-gap sweeps roots]
         :or {node-width 200 node-height 80 x-gap 60 y-gap 100 sweeps 24}} opts
        node-set (set nodes)
        valid-edges (filterv (fn [{:keys [from to]}] (and (node-set from) (node-set to))) edges)
        ranks (assign-ranks (vec node-set) valid-edges roots)
        ranked-groups (group-by-rank ranks)
        rank-order (crossing-minimization ranked-groups valid-edges sweeps)]
    (assign-coordinates rank-order node-width node-height x-gap y-gap)))

;; ============================================================================
;; Graph data extraction
;; ============================================================================

(defn- titleize-keyword [kw]
  (as-> (name kw) $
        (str/replace $ #"-" " ")
        (str/split $ #"\s+")
        (map str/capitalize $)
        (str/join " " $)))

(defn- classify-proc [proc-id conns]
  (let [has-incoming (some (fn [[[_ _] [to _]]] (= to proc-id)) conns)
        has-outgoing (some (fn [[[from _] [_ _]]] (= from proc-id)) conns)]
    (cond
      (and (not has-incoming) has-outgoing) :source
      (and has-incoming (not has-outgoing)) :sink
      :else :process)))

(def ^:private node-colors
  {:source  {:fill "#0d2818" :border "#166534" :badge "#22c55e" :label "SRC"}
   :process {:fill "#111833" :border "#2d3f7a" :badge "#6c8cff" :label "PROC"}
   :sink    {:fill "#2a1608" :border "#9a3412" :badge "#f97316" :label "SINK"}})

(defn- build-graph-data [conns]
  (let [node-set (reduce (fn [s [[from _] [to _]]] (-> s (conj from) (conj to))) #{} conns)
        edges (mapv (fn [[[from from-port] [to to-port]]]
                      {:from from :to to :from-port from-port :to-port to-port})
                    conns)]
    {:nodes (vec node-set) :edges edges}))

;; ============================================================================
;; SVG generation
;; ============================================================================

(defn- edge-path [x1 y1 x2 y2 node-h]
  (let [sy (+ y1 (/ node-h 2))
        ey (- y2 (/ node-h 2))
        mid (/ (Math/abs (- ey sy)) 2)]
    (str "M" x1 "," sy " C" x1 "," (+ sy mid) " " x2 "," (- ey mid) " " x2 "," ey)))

(defn- edge-midpoint [x1 y1 x2 y2 node-h]
  {:x (/ (+ x1 x2) 2)
   :y (/ (+ (+ y1 (/ node-h 2)) (- y2 (/ node-h 2))) 2)})

(defn- svg-node [proc-id pos node-type node-w node-h]
  (let [{:keys [x y]} pos
        colors (get node-colors node-type)
        {:keys [fill border badge label]} colors
        half-w (/ node-w 2)
        half-h (/ node-h 2)
        tx (- x half-w)
        ty (- y half-h)]
    [:g {:transform (str "translate(" tx "," ty ")")}
     [:rect {:width node-w :height node-h :rx 8
             :fill fill :stroke border :stroke-width 2}]
     [:g {:transform (str "translate(" (- node-w 36) ",8)")}
      [:rect {:width 28 :height 16 :rx 4 :fill badge}]
      [:text {:x 14 :y 12 :text-anchor "middle"
              :fill "#000" :font-size 9 :font-weight "bold"}
       label]]
     [:text {:x (/ node-w 2) :y (+ (/ node-h 2) 5)
             :text-anchor "middle"
             :fill "#e5e7eb" :font-size 16 :font-weight "600"}
      (titleize-keyword proc-id)]]))

(defn- svg-edge [from-pos to-pos from-port to-port node-w node-h]
  (let [label-text (str (name from-port) " -> " (name to-port))
        mid (edge-midpoint (:x from-pos) (:y from-pos) (:x to-pos) (:y to-pos) node-h)]
    [:g
     [:path {:d (edge-path (:x from-pos) (:y from-pos) (:x to-pos) (:y to-pos) node-h)
             :fill "none" :stroke "#4b5563" :stroke-width 2
             :marker-end "url(#arrowhead)"}]
     [:g {:transform (str "translate(" (:x mid) "," (:y mid) ")")}
      [:rect {:x -50 :y -10 :width 100 :height 20 :rx 4 :fill "#1a1a2e" :opacity 0.85}]
      [:text {:x 0 :y 4 :text-anchor "middle" :fill "#9ca3af" :font-size 11
              :font-family "monospace"}
       label-text]]]))

(defn- compute-viewbox [positions node-w node-h]
  (if (seq positions)
    (let [xs (map :x (vals positions))
          ys (map :y (vals positions))
          min-x (- (apply min xs) (/ node-w 2) 60)
          max-x (+ (apply max xs) (/ node-w 2) 60)
          min-y (- (apply min ys) (/ node-h 2) 60)
          max-y (+ (apply max ys) (/ node-h 2) 60)]
      {:x min-x :y min-y :w (- max-x min-x) :h (- max-y min-y)})
    {:x 0 :y 0 :w 1200 :h 800}))

;; ============================================================================
;; Template generation
;; ============================================================================

(defn- pan-zoom-js []
  "
(function() {
  var svg = document.querySelector('svg');
  if (!svg) return;
  var vb = svg.viewBox.baseVal;
  var panning = false, startX, startY;
  svg.addEventListener('wheel', function(e) {
    e.preventDefault();
    var rect = svg.getBoundingClientRect();
    var mx = e.clientX - rect.left, my = e.clientY - rect.top;
    var factor = e.deltaY > 0 ? 1.1 : 0.9;
    var vbmx = vb.x + (mx / rect.width) * vb.width;
    var vbmy = vb.y + (my / rect.height) * vb.height;
    var nw = vb.width * factor, nh = vb.height * factor;
    vb.x = vbmx - (mx / rect.width) * nw;
    vb.y = vbmy - (my / rect.height) * nh;
    vb.width = nw; vb.height = nh;
  }, {passive: false});
  svg.addEventListener('mousedown', function(e) {
    if (e.button === 0) { panning = true; startX = e.clientX; startY = e.clientY; }
  });
  svg.addEventListener('mousemove', function(e) {
    if (!panning) return;
    var rect = svg.getBoundingClientRect();
    var dx = (e.clientX - startX) / rect.width * vb.width;
    var dy = (e.clientY - startY) / rect.height * vb.height;
    vb.x -= dx; vb.y -= dy;
    startX = e.clientX; startY = e.clientY;
  });
  svg.addEventListener('mouseup', function() { panning = false; });
  svg.addEventListener('mouseleave', function() { panning = false; });
})();
")

(defn template [{:keys [conns]} root]
  (let [{:keys [nodes edges]} (build-graph-data conns)
        node-w 200
        node-h 80
        positions (layout-graph nodes edges {:node-width node-w :node-height node-h :roots root})
        {:keys [x y w h]} (compute-viewbox positions node-w node-h)]
    (str
      (h/html
        [:html
         [:head
          [:title "Flow Chart"]
          [:style (h/raw "
html, body { margin: 0; padding: 0; background: #0f0f17; height: 100vh; overflow: hidden; }
svg { display: block; width: 100vw; height: 100vh; cursor: grab; }
svg:active { cursor: grabbing; }
")]]
         [:body
          [:svg {:viewBox (str x " " y " " w " " h)
                 :xmlns "http://www.w3.org/2000/svg"}
           [:defs
            [:marker {:id "arrowhead" :markerWidth 10 :markerHeight 7
                      :refX 10 :refY 3.5 :orient "auto" :markerUnits "strokeWidth"}
             [:polygon {:points "0 0, 10 3.5, 0 7" :fill "#4b5563"}]]]
           ;; Edges
           (for [{:keys [from to from-port to-port]} edges
                 :let [fp (get positions from)
                       tp (get positions to)]
                 :when (and fp tp)]
             (svg-edge fp tp from-port to-port node-w node-h))
           ;; Nodes
           (for [node-id nodes
                 :let [pos (get positions node-id)]
                 :when pos]
             (svg-node node-id pos (classify-proc node-id conns) node-w node-h))]
          [:script (h/raw (pan-zoom-js))]]]))))

(defn graph [flow-config & root]
  (tagged-literal 'flare/html {:html (template flow-config (first root))
                               :title "Flow Chart"}))
