(ns stonks.core
  (:require [clojure.pprint :refer [print-table]]
            [clojure.java.io :as io]
            [stonks.db :as db])
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
  (db/save!))

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
  )
