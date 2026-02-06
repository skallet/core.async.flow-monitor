(ns clojurescript.flow-monitor-ui.global
  (:require
    [reagent.core :as r]
    [re-frame.core :as rf]
    [cognitect.transit :as t]
    [clojurescript.flow-monitor-ui.events :as events]
    [clojurescript.flow-monitor-ui.utils.helpers :refer [>dis <sub]]))

(defonce global-state (r/atom {:ws-chan nil
                               :ws-connected false
                               :data nil
                               :flow-ping {}
                               :errors {}
                               :messages []
                               :active-tab :errors
                               :active-proc-pid :nil}))

(defonce global-pings (r/atom {:flow-ping {}}))

;; Are the cards expanded {:proc-io true}
(def proc-card-state (r/atom {}))
(def chan-representation (r/atom :meter)) ; :meter or :line

(def writer (t/writer :json))
(def reader (t/reader :json))

; region == WebSocket
(defn send-socket-data [data]
  (.send (:ws-chan @global-state) (.stringify js/JSON (t/write writer data))))

(defn tagged-value? [^js v]
  (try
    (and (.-tag v) (.-rep v))
    (catch :default _ false)))

(defn transform-tagged-values [data]
  (cond
    (tagged-value? data) [(.-tag ^js data) (transform-tagged-values (.-rep ^js data))]
    (map? data) (reduce-kv (fn [m k v] (assoc m k (transform-tagged-values v))) {} data)
    (vector? data) (mapv transform-tagged-values data)
    (set? data) (into #{} (map transform-tagged-values data))
    :else data))

(defn process-ws-message [msg]
  (let [data (->> msg .-data (t/read reader) transform-tagged-values)
        action (:action data)]
    (case action
      :datafy (do
                (swap! global-state assoc :data (:data data))
                (let [procs (reduce (fn [res [[from _] [to _]]]
                                      (conj res from to))
                                    #{}
                                    (-> data :data :conns))
                      ; set the initial state of the box expansion
                      proc-state (reduce (fn [res proc]
                                           (assoc res proc true)) {} procs)]
                  (reset! proc-card-state proc-state)))
      :ping (let [current-time (js/Date.)]
              (swap! global-pings update-in [:flow-ping]
                     (fn [ping-map]
                       (merge-with #(assoc (merge %1 %2) :last-updated current-time)
                                   ping-map
                                   (:data data)))))
      :error (let [pid (keyword (second (re-find #":pid :(.*)," (:data data))))]
               (swap! global-state update-in [:errors pid]
                      (fnil (comp #(take-last 100 %) conj) []) (:data data)))
      :message (swap! global-state update-in [:messages]
                      (fnil (comp #(take-last 1000 %) conj) []) (:data data))
      "Default")))

(rf/reg-sub
  ::get-params
  (fn [db _]
    (get-in db [:query-params])))

(defn make-websocket! []
  (let [params (<sub [::get-params])
        location (.-location js/window)
        protocol (if (= (.-protocol location) "https:") "wss://" "ws://")
        host (.-host location)
        socket-url (str protocol host "/flow-socket")]
    (if (not (:ws-connected @global-state))
     (if-let [chan (js/WebSocket. socket-url)]
       (do
         (swap! global-state assoc :ws-connected true)
         (set! (.-onmessage chan) (fn [msg]
                                    (process-ws-message msg)))
         (set! (.-onclose chan) (fn [msg]
                                  (.log js/console "Websocket was closed. Connection should be reestablished.")
                                  (swap! global-state assoc :ws-connected false)
                                  (>dis [::events/websocket-server-closed-alert "Websocket server was closed!"])))
         (swap! global-state assoc :ws-chan chan)
         (.log js/console (str "Websocket connection established with: " socket-url)))
       (throw (js/Error. "Websocket connection failed!"))))))
; endregion
