(ns app.christmas
  (:require [tick.core :as t]
            [reagent.core :as r]
            [reitit.frontend.easy :as rfe]))

(def families {:baldwin {:members ["Gene" "Kyle" "Chris" "Devin" "Jonas" "Missy"]}
               :breese {:members ["Sydney" "Michael" "Savannah" "Taylor" "Drew" "Cole"]}
               :porter {:members ["Ryan" "Sharon" "Melinda" "Blake"]}
               :ray {:year-offset 3 :members ["Cindy Lynn" "Jason" "Josh" "Rachel" "Jenna" "Jared"]}})

;; (def time (r/atom (t/instant)))
(def now (r/atom (t/date-time))) ; this is to compute the countdown til Christmas
(defonce year (int (t/year (t/date)))) ; this is what the current year is (don't use `now` since this only needs to be done once)
(def offset (r/atom 0)) ; this is for calculating which year should be displayed

(defn set-time []
  (js/setTimeout (fn []
                   (swap! now t/now)
                   (set-time))
                 1000))

(set-time)

(defn countdown [current-year calculation-year current-time]
  (let [delta (t/between @current-time (t/at (t/new-date calculation-year 12 25) (t/new-time 0 0 0)))
        days (t/days delta)
        hours (t/hours (t/- delta
                            (t/new-duration days :days)))
        minutes (t/minutes (t/- delta
                                (t/new-duration hours :hours)
                                (t/new-duration days :days)))
        seconds (t/seconds (t/- delta
                                (t/new-duration minutes :minutes)
                                (t/new-duration hours :hours)
                                (t/new-duration days :days)))]
    [:div.christmas-countdown
     (if (pos? seconds)
       [:<>
        [:div.lable "Time until Christmas" (when (not= current-year calculation-year) (str " " calculation-year))]
        [:div.time [:span.days days] " " [:span.hours hours] " " [:span.minutes minutes] " " [:span.seconds seconds]]]
       [:div.shout "Merry Christmas!!!"])]))

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
      [:<> [:div.content
            [:div [:h1 "Christmas " calculation-year]
             (when (crazy-finger-message @offset) [:div (crazy-finger-message @offset)])
             [:div.recipients
              (for [p (assign-people members calculation-year (or year-offset 1))]
                [:p {:key (str "gifter-" p)}
                 (first p) " has " (last p) "."])]
             [:nav
              (if (> @offset 0) (prev-year offset) empty-span)
              (if (> @offset 0) (reset-year offset) empty-span)
              (next-year offset)]]
            [:<> (countdown year calculation-year now)]]
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
