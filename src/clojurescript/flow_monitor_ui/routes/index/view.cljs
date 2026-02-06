(ns clojurescript.flow-monitor-ui.routes.index.view
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [clojurescript.flow-monitor-ui.components.modal :as component]
            [clojurescript.flow-monitor-ui.components.log-modal :refer [log-modal]]
            [clojurescript.flow-monitor-ui.global :refer [make-websocket!
                                                          global-state global-pings
                                                          send-socket-data]]
            [clojurescript.flow-monitor-ui.graph :as graph]
            [clojurescript.flow-monitor-ui.utils.helpers :refer [<sub >dis]]))


; = Data Shaping Functions =====================================================
(defn titleize-keyword [kw]
  (as-> (name kw) $
        (clojure.string/replace $ #"-" " ")
        (clojure.string/split $ #"\s+")
        (map clojure.string/capitalize $)
        (clojure.string/join " " $)))

(defn format-number [n]
  (if n
    (.format (js/Intl.NumberFormat. "en-US") n)
    "--"))

(defn escape-html [text]
  (-> text
      (str/replace #"&" "&amp;")
      (str/replace #"<" "&lt;")
      (str/replace #">" "&gt;")))

(defn fmt-state
  [data]
  (letfn [(format-map [m indent-level]
            (if (empty? m)
              "{}"
              (let [indent (str/join (repeat indent-level "  "))
                    inner-indent (str indent "  ")
                    pairs (for [[k v] m]
                            (str inner-indent (pr-str k) " "
                                 (if (map? v)
                                   (format-map v (+ indent-level 1))
                                   (pr-str v))))
                    formatted (str "{\n" (str/join ",\n" pairs) "\n" indent "}")]
                formatted)))]
    (if (map? data)
      (format-map data 0)
      (pr-str data))))

(defn seconds-since [^js/Date t]
  (let [now (js/Date.)
        diff (- (.getTime now) (.getTime t))]
    (Math/floor (/ diff 1000))))

; = Components =================================================================

(defn ws-connect-btn []
  [:div.centered-button-container
   [:button.button
    {:on-click (fn [e]
                 (make-websocket!))}
    "Flow Connect"]])

(defn proc-card [proc proc-stats]
  (let [errors (-> @global-state :errors proc)
        last-updated (:last-updated (proc (:flow-ping @global-pings)))
        since-last-updated (if last-updated (seconds-since last-updated) 0)
        paused? (= :paused (:status proc-stats))]
    [:div.middle-section-one-container
     [:div.title-container [:h2.title (titleize-keyword proc)]]
     (when (not-empty (:state proc-stats))
       [:div.state [:pre.code-block [:code (fmt-state (:state proc-stats))]]])
     [:div.call-count (format-number (:count proc-stats))]
     [:div.action-buttons
      [:div.action-button
       {:on-click (fn [evt]
                    (swap! global-state assoc :active-tab :inject)
                    (swap! global-state assoc :active-proc-pid proc)
                    (.add (.-classList (.-body js/document)) "modal-open")
                    (>dis [::component/set-modal-visibility true]))}
       [:img {:src "assets/img/inject_icon.svg"}]]
      [:div.action-button
       {:class (when (-> errors count zero? not) "error")
        :on-click (fn [evt]
                    (swap! global-state assoc :active-tab :errors)
                    (swap! global-state assoc :active-proc-pid proc)
                    (.add (.-classList (.-body js/document)) "modal-open")
                    (>dis [::component/set-modal-visibility true]))}
       [:img {:src (if (-> errors count zero? not) "assets/img/error_icon_red.svg" "assets/img/error_icon.svg")}]
       (when (-> errors count zero? not) [:div {:style {:margin-left "5px" :color "#E12D39" :padding-right "5px"}} (count errors)])]
      [:div.action-button
       {:on-click (fn [e]
                    (if (= :running (:status proc-stats))
                      (send-socket-data {:action :pause-proc :pid proc})
                      (send-socket-data {:action :resume-proc :pid proc})))}
       [:img {:src (if paused? "assets/img/pause_icon_orange.svg" "assets/img/pause_icon_white.svg")}]]]
     (when (> since-last-updated 10) [:div.stale "Last Updated: " since-last-updated " seconds ago."])]))

(defn buffer-meter [label buffer-stats]
  (let [buf-type (-> buffer-stats :buffer :type)
        buf-count (-> buffer-stats :buffer :count)
        buf-cap (-> buffer-stats :buffer :capacity)]
    (when buf-type
      [:div {:style {:margin-bottom "4px"}}
       [:div {:style {:font-size "12px" :color "#9ca3af" :margin-bottom "2px"}}
        (str (name label) " - " buf-type ": " (format-number buf-count) " / " (format-number buf-cap))]
       [:div {:style {:background "#1f2937" :border-radius "2px" :height "8px" :overflow "hidden"}}
        [:div {:style {:background "#3b82f6"
                       :height "100%"
                       :width (str (if (and buf-count buf-cap (pos? buf-cap))
                                     (* 100 (/ buf-count buf-cap))
                                     0) "%")
                       :transition "width 0.3s ease"}}]]])))

(defn detail-panel []
  (let [sel @graph/selected-node
        ping (:flow-ping @global-pings)
        data (:data @global-state)
        conns (:conns data)]
    (when sel
      (let [proc-ping (get ping sel)
            proc-stats {:status (:clojure.core.async.flow/status proc-ping)
                        :count (:clojure.core.async.flow/count proc-ping)
                        :state (:clojure.core.async.flow/state proc-ping)
                        :ins (:clojure.core.async.flow/ins proc-ping)
                        :outs (:clojure.core.async.flow/outs proc-ping)}
            proc-type (graph/classify-proc sel conns)
            errors (-> @global-state :errors sel)
            paused? (= :paused (:status proc-stats))]
        [:div.flow-graph-detail-panel
         [:div.detail-panel-header
          [:h3 {:style {:margin "0 0 4px" :color "#e5e7eb" :font-size "18px"}}
           (titleize-keyword sel)]
          [:button {:style {:background "none" :border "none" :color "#9ca3af"
                            :cursor "pointer" :font-size "16px" :padding "0"}
                    :on-click #(reset! graph/selected-node nil)} "X"]]
         ;; Type badge
         [:div {:style {:margin-bottom "12px"}}
          [:span {:style {:background (get-in graph/node-colors [proc-type :badge])
                          :color "#000" :padding "2px 8px" :border-radius "4px"
                          :font-size "11px" :font-weight "bold"}}
           (get-in graph/node-colors [proc-type :label])]]
         ;; Status
         [:div {:style {:color "#9ca3af" :font-size "13px" :margin-bottom "8px"}}
          "Status: "
          [:span {:style {:color (if paused? "#f97316" "#22c55e")}}
           (if paused? "paused" (or (some-> (:status proc-stats) name) "unknown"))]]
         ;; Call count
         [:div {:style {:color "#9ca3af" :font-size "13px" :margin-bottom "12px"}}
          "Call Count: " [:span {:style {:color "#e5e7eb"}} (format-number (:count proc-stats))]]
         ;; State
         (when (not-empty (:state proc-stats))
           [:div {:style {:margin-bottom "12px"}}
            [:div {:style {:color "#9ca3af" :font-size "13px" :margin-bottom "4px"}} "State:"]
            [:pre {:style {:background "#1f2937" :padding "8px" :border-radius "4px"
                           :color "#e5e7eb" :font-size "12px" :overflow-x "auto"
                           :margin 0 :max-height "120px"}}
             (fmt-state (:state proc-stats))]])
         ;; Ins
         (when (seq (:ins proc-stats))
           [:div {:style {:margin-bottom "12px"}}
            [:div {:style {:color "#9ca3af" :font-size "13px" :margin-bottom "4px"}} "Ins:"]
            (doall (for [[k v] (:ins proc-stats)
                         :when (map? v)]
                     ^{:key k} [buffer-meter k v]))])
         ;; Outs
         (when (seq (:outs proc-stats))
           [:div {:style {:margin-bottom "12px"}}
            [:div {:style {:color "#9ca3af" :font-size "13px" :margin-bottom "4px"}} "Outs:"]
            (doall (for [[k v] (:outs proc-stats)
                         :when (map? v)]
                     ^{:key k} [buffer-meter k v]))])
         ;; Action buttons
         [:div {:style {:display "flex" :gap "8px" :margin-top "12px"}}
          [:button.ctrl-btn
           {:on-click (fn [_]
                        (swap! global-state assoc :active-tab :inject)
                        (swap! global-state assoc :active-proc-pid sel)
                        (.add (.-classList (.-body js/document)) "modal-open")
                        (>dis [::component/set-modal-visibility true]))}
           "Inject"]
          [:button.ctrl-btn
           {:class (when (and errors (pos? (count errors))) "error-btn")
            :on-click (fn [_]
                        (swap! global-state assoc :active-tab :errors)
                        (swap! global-state assoc :active-proc-pid sel)
                        (.add (.-classList (.-body js/document)) "modal-open")
                        (>dis [::component/set-modal-visibility true]))}
           (str "Errors" (when (and errors (pos? (count errors)))
                           (str " (" (count errors) ")")))]
          [:button.ctrl-btn
           {:on-click (fn [_]
                        (if (= :running (:status proc-stats))
                          (send-socket-data {:action :pause-proc :pid sel})
                          (send-socket-data {:action :resume-proc :pid sel})))}
           (if paused? "Resume" "Pause")]]]))))

; = Template ===================================================================
(defn template []
  [:<>
   [:div
    [component/report-modal]
    [log-modal]
    (if (:ws-connected @global-state)
      [:div {:style {:display "flex" :height "100vh"}}
       [:div {:style {:flex 1 :overflow "hidden"}}
        [graph/graph-component]]
       [detail-panel]]
      [ws-connect-btn])]])
