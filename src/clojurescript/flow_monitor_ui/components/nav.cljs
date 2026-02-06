(ns clojurescript.flow-monitor-ui.components.nav
  (:require
    [clojurescript.flow-monitor-ui.components.log-modal :as log-modal]
    [clojurescript.flow-monitor-ui.graph :as graph]
    [clojurescript.flow-monitor-ui.utils.helpers :refer [>dis <sub]]
    [reagent.core :as r]))

(defn settings-bar []
  (let [menu-open? (r/atom false)]
    (fn []
      (let [direction @graph/rank-dir
            show-labels? @graph/show-edge-labels?]
        [:div.settings-icons-container
         [:div.log-icon-wrapper
          [:button.log-icon
           {:on-click #(>dis [::log-modal/set-log-modal-visibility true])}
           [:img {:src "assets/img/message_icon.svg"}]]]
         [:div.settings-container
          [:div.settings-icon-wrapper
           [:button.settings-icon {:class (when @menu-open? "opened")
                                   :on-click #(swap! menu-open? not)}
            [:img {:src "assets/img/settings.svg"}]]]]
         [:div.settings-dropdown {:class (when @menu-open? "active")}
          [:div.settings-header
           [:h3 "Settings"]
           [:button.close-settings {:on-click #(reset! menu-open? false)} ""]]
          [:div.settings-content
           [:h4 "Graph Layout"]
           [:div.setting-option
            [:label "Layout Direction"]
            [:div.pill-toggle
             [:button.pill-btn.pill-left
              {:class (when (= :TB direction) "active")
               :on-click (fn [e] (reset! graph/rank-dir :TB))}
              "Top-Bottom"]
             [:button.pill-btn.pill-right
              {:class (when (= :LR direction) "active")
               :on-click (fn [e] (reset! graph/rank-dir :LR))}
              "Left-Right"]]]
           [:div.setting-option
            [:label "Edge Labels"]
            [:div.pill-toggle
             [:button.pill-btn.pill-left
              {:class (when show-labels? "active")
               :on-click (fn [e] (reset! graph/show-edge-labels? true))}
              "Show"]
             [:button.pill-btn.pill-right
              {:class (when-not show-labels? "active")
               :on-click (fn [e] (reset! graph/show-edge-labels? false))}
              "Hide"]]]]]]))))
