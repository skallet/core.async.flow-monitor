(ns clojurescript.flow-monitor-ui.layout)

(defn- in-degree
  "Returns map of node-id -> number of incoming edges."
  [nodes edges]
  (let [base (into {} (map (fn [n] [n 0]) nodes))]
    (reduce (fn [m {:keys [to]}]
              (update m to (fnil inc 0)))
            base edges)))

(defn- topological-ranks
  "Kahn's algorithm. Returns map of node-id -> rank (0-based).
   Nodes in cycles get rank 0."
  [nodes edges]
  (let [in-deg (in-degree nodes edges)
        adj (reduce (fn [m {:keys [from to]}]
                      (update m from (fnil conj []) to))
                    {} edges)
        initial (filterv (fn [n] (zero? (get in-deg n 0))) nodes)
        queue (into #queue [] initial)]
    (loop [q queue
           ranks {}
           in-deg in-deg]
      (if (empty? q)
        ;; any remaining nodes are in cycles -> rank 0
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
                           ;; find predecessors
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

(defn- assign-ranks
  "Returns map of node-id -> rank. If roots are provided, those get rank 0
   and everything is shifted accordingly."
  [nodes edges roots]
  (let [base-ranks (topological-ranks nodes edges)]
    (if (seq roots)
      (let [min-root-rank (apply min (map #(get base-ranks % 0) roots))
            shifted (reduce-kv (fn [m k v]
                                 (assoc m k (- v min-root-rank)))
                               {} base-ranks)
            ;; clamp negatives to 0
            clamped (reduce-kv (fn [m k v]
                                 (assoc m k (max 0 v)))
                               {} shifted)]
        clamped)
      base-ranks)))

(defn- group-by-rank
  "Returns sorted vector of [rank [node-ids...]]."
  [ranks]
  (->> (group-by val ranks)
       (map (fn [[rank entries]]
              [rank (mapv first entries)]))
       (sort-by first)))

(defn- barycenter
  "Compute barycenter position of a node in rank r+1 based on positions of
   neighbors in rank r."
  [node edges rank-nodes-positions]
  (let [neighbors (keep (fn [{:keys [from to]}]
                          (cond
                            (= to node) (get rank-nodes-positions from)
                            (= from node) (get rank-nodes-positions to)
                            :else nil))
                        edges)]
    (if (seq neighbors)
      (/ (reduce + 0 neighbors) (count neighbors))
      nil)))

(defn- order-rank
  "Order nodes in a single rank using barycenter heuristic relative to fixed rank."
  [rank-nodes edges fixed-positions]
  (let [scored (map (fn [n]
                      [n (or (barycenter n edges fixed-positions)
                             (get fixed-positions n 0))])
                    rank-nodes)]
    (mapv first (sort-by second scored))))

(defn- crossing-minimization
  "Barycenter crossing minimization with configurable sweeps.
   Returns map of rank -> ordered vector of nodes."
  [ranked-groups edges sweeps]
  (let [initial-order (into {} (map (fn [[rank nodes]] [rank (vec nodes)]) ranked-groups))
        ranks-sorted (mapv first (sort-by first ranked-groups))]
    (loop [order initial-order
           sweep 0]
      (if (>= sweep sweeps)
        order
        (let [;; forward sweep
              order-fwd
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
              ;; backward sweep
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

(defn- assign-coordinates
  "Assign x,y coordinates. direction is :TB or :LR.
   Returns map of node-id -> {:x int :y int}."
  [rank-order direction node-width node-height x-gap y-gap]
  (let [vertical? (= direction :TB)]
    (reduce-kv
      (fn [positions rank nodes]
        (let [n (count nodes)
              total-span (if vertical?
                           (+ (* n node-width) (* (dec n) x-gap))
                           (+ (* n node-height) (* (dec n) y-gap)))
              start (- (/ total-span 2))]
          (reduce
            (fn [pos [idx node]]
              (let [primary-offset (+ start (if vertical?
                                              (* idx (+ node-width x-gap))
                                              (* idx (+ node-height y-gap))))
                    secondary-offset (* rank (if vertical?
                                               (+ node-height y-gap)
                                               (+ node-width x-gap)))]
                (assoc pos node
                       (if vertical?
                         {:x primary-offset :y secondary-offset}
                         {:x secondary-offset :y primary-offset}))))
            positions
            (map-indexed vector nodes))))
      {}
      rank-order)))

(defn layout-graph
  "Layout a directed graph for visualization.
   nodes - collection of node ids
   edges - collection of {:from id :to id} maps
   opts  - {:direction :TB/:LR  (default :TB)
            :node-width  (default 200)
            :node-height (default 80)
            :x-gap       (default 60)
            :y-gap       (default 100)
            :sweeps      (default 24)
            :roots       (default nil)}"
  [nodes edges opts]
  (let [{:keys [direction node-width node-height x-gap y-gap sweeps roots]
         :or {direction :TB
              node-width 200
              node-height 80
              x-gap 60
              y-gap 100
              sweeps 24
              roots nil}} opts
        node-set (set nodes)
        valid-edges (filterv (fn [{:keys [from to]}]
                               (and (node-set from) (node-set to)))
                             edges)
        ranks (assign-ranks (vec node-set) valid-edges roots)
        ranked-groups (group-by-rank ranks)
        rank-order (crossing-minimization ranked-groups valid-edges sweeps)]
    (assign-coordinates rank-order direction node-width node-height x-gap y-gap)))
