(ns app.christmas
  (:require [cljs-time.core :as time]
            [reagent.core :as r]
            [reitit.frontend.easy :as rfe]))

(def families {:baldwin {:year-offset 1 :members ["Gene" "Kyle" "Chris" "Devin" "Jonas" "Melissa"]}
               :breese {:year-offset 1 :members ["Sydney" "Michael" "Savannah" "Taylor" "Drew" "Cole"]}
               :porter {:year-offset 1 :members ["Ryan" "Sharon" "Melinda" "Blake"]}
               :ray {:year-offset 1 :members ["Cindy Lynn" "Jason" "Josh" "Rachel" "Jenna" "Jared"]}})

(def offset (r/atom 0))

(def year (.getYear (time/now)))

(defn next-year [offset]
  [:span.link {:on-click #(swap! offset inc)} "Next Year →"])

(defn prev-year [offset]
  [:span.link {:on-click #(swap! offset dec)} "← Previous Year"])

(defn reset-year [offset]
  [:span.link {:on-click #(reset! offset 0)} "Reset"])

(def empty-span [:span])

(defn crazy-finger-message [o]
  (condp < o
    4000 "What exactly do you expect to see at this point?"
    3000 "Is the earth even going to be around at this point?"
    2000 "...two medals!"
    1000 "You deserve a medal!"
    350 "Wow! You are REALLY presistant."
    222 "Are you seriously still at this?"
    122 "Okay, fine. Have it your way."
    121 "I can see I'm not getting through to you."
    120 "Are you done yet?"
    82 "We got to stop meeting like this."
    45 "You know there's no extra credit...right?"
    37 "Seriously?"
    nil))

(defn assign-people
  "Takes list people and a display-year and returns vector of vectors with [givers recievers]"
  [people display-year year-offset]
  ; year-offset used to adjust each family individually, it's usually 1, but if a member leaves the group we have the chance to adjust it
  (let [display-year (+ display-year year-offset)                     ; sync up with where we are in the rotation
        offset       (+ (mod display-year (- (count people) 1)) 1)
        recievers    (into (subvec people offset) (subvec people 0 offset))]
    (partition 2 (interleave people recievers))))

(defn christmas-page [match]
  (let [{:keys [path]} (:parameters match)
        {:keys [family]} path
        calculation-year (+ year @offset)
        {:keys [year-offset members]} ((keyword family) families)]
    (if members
      [:div [:div.content
             [:div [:h1 "Christmas " calculation-year]
              (when (crazy-finger-message @offset) [:div (crazy-finger-message @offset)])
              [:div.recipients
               (for [p (assign-people members (+ year @offset) year-offset)]
                 [:p {:key (str "gifter-" p)}
                  (first p) " has " (last p) "."])]
              [:nav
               (if (> @offset 0) (prev-year offset) empty-span)
               (if (> @offset 0) (reset-year offset) empty-span)
               (next-year offset)]]]
       [:div.disclaimer "* If you notice errors on this page, please write a detailed note on a $20 bill and send it to Jonas."]]
      [:div [:div.content
             [:div "Family not found, but Merry Christmas anyway."]]])))


(def links [:div
            [:h2 "Christmas Lists"]
            [:ul
             (for [[k _] families]
               [:li {:key k} [:a {:href (rfe/href ::christmas {:family (name k)})} (str (clojure.string/capitalize (name k)))]])]])

(def routes
  [["/christmas/:family"
    {:name ::christmas
     :view christmas-page
     :parameters {:path {:family string?}}}]])
