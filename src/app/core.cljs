(ns app.core
  "This namespace contains your application and is the entrypoint for 'yarn start'."
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [reitit.frontend :as rf]
            [reitit.frontend.easy :as rfe]
            [reitit.coercion.spec :as rss]
            [app.christmas :as christmas]))

(defn home-page "display home page" []
  [:div christmas/links])

(def home-routes
  [["/"
    {:name ::home
     :view home-page}]])

(def routes
  (concat
   home-routes
   christmas/routes))

(defonce match (r/atom nil))

(defn current-page []
  (if @match
    (let [view (:view (:data @match))]
      [:div [:a {:href (rfe/href ::home) :class ["link"]} "Home"]
       [view @match]])
    [:div [:a {:href (rfe/href ::home) :class ["link"]} "Home"]
     [:div "Page not found."]]))

(defn ^:export main []
  (rfe/start!
   (rf/router routes {:data {:coercion rss/coercion}})
   (fn [m] (reset! match m))
    ;; set to false to enable HistoryAPI
   {:use-fragment true})
  (rd/render [current-page] (.getElementById js/document "app")))
