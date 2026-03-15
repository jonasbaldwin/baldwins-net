(ns app.events
  (:require [ajax.core :as ajax :refer [GET]]
            [testdouble.cljs.csv :refer [read-csv]]
            [ajax.edn :as ajax-edn]
            [reagent.core :as r]
            [reitit.frontend.easy :as rfe]
            [tick.core :as t]
            [tick.locale-en-us]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            ["canvas-confetti" :as confetti]))

;; (def confetti (js/require "confetti"))

; shows birthdays and anniversaries
; displays calendar with event dates highlited
; displays list of next year of events


(def families {:baldwin {:short :b}})

(def event-data (r/atom nil))

(def now (t/date))
(def this-year (int (t/format (t/formatter "yyyy") now)))
(def next-year (inc this-year))

(defn pap [m v]
  (println m (type v))
  (pprint/pprint v)
  v)

(defn cleanup-map [m]
  (apply dissoc
         m
         (for [[k v] m :when (or (nil? v) (string/blank? v))] k)))

(defn present-value? [value]
  (and (some? value)
       (not (and (string? value) (string/blank? value)))))

(defn next-recurring-date [month day context]
  (when (and (present-value? month) (present-value? day))
    (try
      (let [event-date (t/new-date this-year month day)]
        (if (>= event-date now)
          event-date
          (t/new-date next-year month day)))
      (catch :default error
        (js/console.warn "Skipping invalid recurring date" (clj->js (assoc context :month month :day day :error (.-message error))))
        nil))))

(defn normalize-anniversary [anniversary]
  (if (and (string? anniversary)
           (re-matches #"\d{2}-\d{2}" anniversary))
    (str this-year "-" anniversary)
    anniversary))

(defn anniversary-date [anniversary context]
  (let [anniversary (normalize-anniversary anniversary)]
    (when (present-value? anniversary)
      (try
        (t/date anniversary)
        (catch :default error
          (js/console.warn "Skipping invalid anniversary date" (clj->js (assoc context :anniversary anniversary :error (.-message error))))
          nil)))))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ; header
            (map keyword)
            repeat)
       (rest csv-data)))

(defn parse-person
  "parse a person's details"
  [{:keys [partner] :as person}]
  (let [person (-> person
                   (assoc :id (keyword (:id person))
                          :partner (when (and partner (not (string/blank? partner))) (keyword partner)))
                   cleanup-map)
        next-birthday (next-recurring-date (:birth-month person) (:birth-day person) {:id (:id person) :type :birthday})]
    (cond-> person
      next-birthday (assoc :next-birthday next-birthday))))

(defn birthday [[_ {:keys [next-birthday preferred-name relation] :as person}]]
  {:type :birthday :person preferred-name :date next-birthday :relation relation})

(defn anniversary [[_ {:keys [id partner anniversary]}]]
  ; tick's modification is still TBD so get values manually
  (when-let [wedding (anniversary-date anniversary {:id id :partner partner :type :anniversary})]
    (let [wedding-month (t/month wedding)
          wedding-day (t/day-of-month wedding)
          next-anniversary (next-recurring-date wedding-month wedding-day {:id id :partner partner :type :anniversary})]
      (when next-anniversary
        {:type :anniversary :people [id partner] :date next-anniversary :years (t/years (t/between wedding next-anniversary))}))))

(defn prep-events [event-list]
  (let [people (reduce #(assoc %1 (-> %2 :id keyword) (parse-person %2)) {} event-list)
        birthdays (->> people
                       (filter (fn [[_ person]] (:next-birthday person)))
                       (map birthday))
        anniversaries (as-> people $
                        (map anniversary $)
                        (remove #(nil? %) $)
                        (apply list $))
        events (sort-by :date (concat birthdays anniversaries))]
    {:people people
     :events events}))

(GET "data/events.csv"
  {:handler #(->> %
                  read-csv
                  csv-data->maps
                  prep-events
                  (reset! event-data))})

(defn rand-in-range [min max]
  (-> (rand)
      (* (- max min))
      (+ min)))

(defn confetti-frame [ticks]
  (confetti (clj->js {:particleCount 7 :origin {:x (rand) :y (rand) :ticks ticks :gravity (rand-in-range 0.4 0.6) :drift (rand-in-range -5 5)}})))

(defn shower-confetti []
  (doall (for [i (range 250)] (confetti-frame 15))))

(defn calendar [now events]
  (let [day-of-week (t/day-of-week now)]
    [:div.calendar
     "yo!"
     (str day-of-week)
     (t/day-of-month now)]))

(defn display-date [date]
  [:div.date (t/format (t/formatter "MMMM d") date)])

(defn display-birthday [{:keys [person date]} today?]
  (let [possisive-name (if (= (last person) \s) (str person "'") (str person "'s"))]
    [:div.birthday
     (display-date date)
     [:div.salutation (if today? [:h2 (str "🎂 Happy Birthday " person "! 🎂")] (str possisive-name " Birthday"))]]))

(defn display-anniversary [{:keys [people date]} all-people today?]
  (let [[p1 p2] people
        p1 (p1 all-people)
        p2 (p2 all-people)]
    [:div.anniversary
     (display-date date)
     [:div.salutation (if today? [:h2 (str "Happy Anniversary ") (:preferred-name p1) " and " (:preferred-name p2) "!"] (str (:preferred-name p1) " and " (:preferred-name p2) "'s Anniversary."))]]))

(defn get-event-message [{:keys [person people] :as event} all-people]
  (condp = (:type event)
    :birthday (str "🎂 Happy Birthday " person "! 🎂")
    :anniversary (let [[p1 p2] people
                       p1 (p1 all-people)
                       p2 (p2 all-people)]
                   (str "Happy Anniversary " (:preferred-name p1) " and " (:preferred-name p2) "!"))))

(defn copy-to-clipboard [text]
  (-> (.writeText (.-clipboard js/navigator) text)
      (.then #(js/console.log "Copied to clipboard"))
      (.catch #(js/console.error "Failed to copy to clipboard" %))))

(defn handle-event-click [event all-people family today?]
  (when today?
    (let [message (get-event-message event all-people)
          route (rfe/href ::family-events {:family family})
          url (str (.-origin js/location)
                   (when-not (string/starts-with? route "/") "/")
                   route)
          text-to-copy (str message " " url)]
      (copy-to-clipboard text-to-copy))))

(defn display-event [{:keys [person people date] :as event} all-people family]
  (let [month-class (string/lower-case (t/format (t/formatter "MMM") date))
        today? (t/= date now)
        class-list (if today? [month-class "today"] month-class)]
    (when today? (shower-confetti))
    [:div.event {:key (str date "-" person people)
                 :class class-list
                 :on-click #(handle-event-click event all-people family today?)}
     (condp = (:type event)
       :birthday (display-birthday event today?)
       :anniversary (display-anniversary event all-people today?))]))

(defn events-page [match]
  (let [{:keys [path]} (:parameters match)
        {:keys [family]} path
        day-of-week (t/day-of-week now)
        family-name (clojure.string/capitalize family)]
    [:div.content [:h1 (str family-name " Family Events")]
     [:div.events
      (doall (for [e (:events @event-data)]
               (display-event e (:people @event-data) family)))]]))

(defn links [match]
  [:div.content
   [:div.events
    [:h1 "Events Lists"]
    [:ul
     (for [[k {:keys [display]}] families]
       [:li {:key k} [:a {:href (rfe/href ::family-events {:family (name k)})} (or display (str (clojure.string/capitalize (name k))))]])]]])

(def routes
  [["events"
    {:name ::family-list
     :view links}]
   ["events/:family"
    {:name ::family-events
     :view events-page
     :parameters {:path {:family string?}}}]])
