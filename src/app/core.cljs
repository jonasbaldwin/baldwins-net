(ns app.core
  "This namespace contains your application and is the entrypoint for 'yarn start'."
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]
            [reitit.coercion.spec :as rss]
            [app.christmas :as christmas]
            [app.events :as events]))

(defn home-page "display home page" []
  [:div.content
   [:h1 "Welcome to Baldwins.net"]
   [:ul
    [:li [:a {:href (rfe/href ::events/family-events {:family "baldwin"})} "Events"] " - See a list of the next year's birthdays and anniversaries."]
    [:li [:a {:href (rfe/href ::christmas/list)} "Christmas"] " - See who you have for Christmas."]]])

(def home-routes
  [["/"
    {:name ::home
     :view home-page}]])

(def routes
  (concat
   home-routes
   christmas/routes
   events/routes))

(defonce match (r/atom nil))

(defn app-links []
  [:nav
   [:a {:href (rfe/href ::home)} "Home"]
  ;;  [:a {:href (rfe/href ::events/family-list)} "Events"]
   [:a {:href (rfe/href ::events/family-events {:family "baldwin"})} "Events"]
   [:a {:href (rfe/href ::christmas/list)} "Christmas"]])

(defn current-page []
  (if @match
    (let [view (:view (:data @match))]
      [:div
       (app-links)
       [view @match]])
    [:div app-links
     [:div "Page not found."]]))

(defn ^:export main []
  (rfe/start!
   (rf/router routes {:data {:coercion rss/coercion}})
   (fn [m] (reset! match m) (reset! christmas/offset 0))
    ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (rd/render [current-page] (.getElementById js/document "app")))
