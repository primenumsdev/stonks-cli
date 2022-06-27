(ns stonks.core
  (:require [clojure.pprint :refer [print-table]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [stonks.db :as db]
            [stonks.api :as api])
  (:import (java.util Date))
  (:gen-class))

(defn utc-now! [] (Date.))

(defn cls
  "Clear screen"
  []
  (print (str (char 27) "[2J")))

(defn reset-cur
  "Move cursor to the top left corner of the screen"
  []
  (print (str (char 27) "[;H")))

(defn user-home-dir []
  (System/getProperty "user.home"))

(defn userdata-path []
  (str (user-home-dir) "/.stonks/userdata.db"))

(defn userdata-exists? []
  (.exists (io/file (userdata-path))))

(defn initial-setup! []
  (println "Hello, what is your name?")
  (let [username (read-line)]
    (printf "Hi %s!\n" username)
    (println "We are going to setup a new user data file for you.")
    (println "Please specify a password:")
    (let [pass (read-line)]
      (db/create!
        {:file-path (userdata-path)
         :name      "userdata"
         :password  pass}))
    (println "Enter finnhub.io API token:")
    (let [token (read-line)]
      (db/set :finnhub-token token)
      (api/set-token! token))
    (db/set :username username)
    (db/set :registered (utc-now!))
    (db/set :last-login (utc-now!))
    (db/set :transactions [])
    (db/save!)))

(defn load-userdata! []
  (println "Hello, please enter your password:")
  (let [pass (read-line)]
    (db/connect!
      {:file-path (userdata-path)
       :password  pass}))
  (printf "Welcome back, %s!\n" (db/get :username))
  (printf "Last login: %s\n" (db/get :last-login))
  (db/set :last-login (utc-now!))
  (api/set-token! (db/get :finnhub-token))
  (db/save!))

(defn menu [{:keys [prompt options]}]
  (let [options       (map (fn [o idx]
                             (if (string? o)
                               {:id (str (inc idx)) :text o}
                               o)) options (range))
        valid-options (set (map :id options))]
    (loop []
      (when prompt
        (println)
        (println prompt)
        (println))
      (doseq [{:keys [id text]} options]
        (println (str " [" id "]") text))
      (println)
      (println "or press <enter> to cancel")
      (let [in (str/trim (read-line))]
        (cond (= in "")
              :cancelled
              (not (valid-options in))
              (do
                (println (format "\n-- Invalid option '%s'!" in))
                (recur))
              :else
              (first (filter #(= in (:id %)) options)))))))

(defn dashboard []
  (println "---")
  (let [trans (db/get :transactions)]
    (prn trans))
  (println "---")
  (println "Menu:")
  (println "1 - Add buy transaction")
  (println "2 - Add sell transaction")
  (println "q - Exit")
  ;; wait user input
  (case (read-line)
    "1" (do
          (println "Add new buy transaction")
          (println "Ticker:")
          (let [ticker (read-line)]
            (println "Price:")
            (let [price (read-line)]
              (prn "price is " price)
              (db/update :transactions #(conj % [:buy ticker price "USD" (utc-now!)]))
              (db/save!)))
          (recur))
    "2" (do
          (println "Add new sell transaction")
          (println "Ticker:")
          (let [ticker (read-line)]
            (println "Price:")
            (let [price (read-line)]
              (prn "price is " price)
              (db/update :transactions #(conj % [:sell ticker price "USD" (utc-now!)]))
              (db/save!)))
          (recur))
    "q" (do
          (println "Bye bye!"))))

(defn -main
  "Entry."
  [& args]
  ;; check userdata
  (if-not (userdata-exists?)
    (initial-setup!)
    (load-userdata!))
  (dashboard))

(comment
  (-main)



  (menu {:prompt  "Which database"
         :options ["Localhost" "Remote" {:id "o" :text "Other"}]})
  )
