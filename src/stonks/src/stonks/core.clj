(ns stonks.core
  (:require [clojure.pprint :refer [print-table]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [stonks.db :as db]
            [stonks.api :as api])
  (:import (java.util Date))
  (:gen-class))

(defn utc-now! [] (Date.))

(defn user-home-dir []
  (System/getProperty "user.home"))

(defn userdata-path []
  (str (user-home-dir) "/.stonks/userdata.db"))

(defn userdata-exists? []
  (.exists (io/file (userdata-path))))

(defn read-big-dec []
  (BigDecimal. (str (read-line))))

(defn read-int []
  (Integer/parseInt (str (read-line))))

(defn prompt-str [msg]
  (println msg)
  (read-line))

(defn prompt-int [msg]
  (println msg)
  (try
    (read-int)
    (catch NumberFormatException _
      (println "Invalid number format, try again.")
      (prompt-int msg))))

(defn prompt-big-dec [msg]
  (println msg)
  (try
    (read-big-dec)
    (catch NumberFormatException _
      (println "Invalid number format, try again.")
      (prompt-int msg))))

(defn initial-setup! []
  (let [username (prompt-str "Hello, what is your name?")]
    (printf "Hi %s!\n" username)
    (println "We are going to setup a new user data file for you.")
    (let [pass (prompt-str "Please specify a password:")]
      (db/create!
        {:file-path (userdata-path)
         :name      "userdata"
         :password  pass}
        {:override? true
         :discard?  true}))
    (let [token (prompt-str "Enter finnhub.io API token:")]
      (db/set :finnhub-token token)
      (api/set-token! token))
    (db/with-save!
      (db/set :username username)
      (db/set :registered (utc-now!))
      (db/set :last-login (utc-now!))
      (db/set :transactions []))))

(defn load-userdata! []
  (let [pass (prompt-str "Hello, please enter your password:")]
    (db/connect!
      {:file-path (userdata-path)
       :password  pass}))
  (printf "Welcome back, %s!\n" (db/get :username))
  (printf "Last login: %s\n" (db/get :last-login))
  (db/with-save!
    (db/set :last-login (utc-now!)))
  (api/set-token! (db/get :finnhub-token)))

(defn stats []
  (let [trans       (db/get :transactions)
        total-spent (->> trans
                         (filter #(= :buy (first %)))
                         (map #(* (nth % 2) (nth % 3)))
                         (reduce +))
        ticker-amt  (->> trans
                         (group-by second)
                         (reduce-kv (fn [m k v]
                                      (assoc m k (reduce
                                                   (fn [val tr]
                                                     (case (first tr)
                                                       :buy (+ val (nth tr 2))
                                                       :sell (- val (nth tr 2))))
                                                   0 v)))
                                    {}))
        cur-eval    (->> ticker-amt
                         (reduce-kv (fn [t k v]
                                      (+ t (* (get (api/get-quote k) "c") v)))
                                    0))]
    (printf "Total spent: %s USD\n" total-spent)
    (printf "Current evaluation: %s USD\n" cur-eval)
    (println "Holdings:" ticker-amt)))

(defn add-new-transaction [trans-type]
  (let [ticker (prompt-str "Ticker:")
        amt    (prompt-int "Amount:")
        price  (prompt-big-dec "Price:")]
    (db/with-save!
      (db/update :transactions #(conj % [trans-type ticker amt price "USD" (utc-now!)])))))

(defn dashboard []
  (println "---")
  (stats)
  (println "---")
  (println "Menu:")
  (println "1 - Add buy transaction")
  (println "2 - Add sell transaction")
  (println "c - Clear all transactions")
  (println "q - Exit")
  ;; wait user input
  (case (read-line)
    "1" (do
          (println "Add new buy transaction")
          (add-new-transaction :buy)
          (recur))
    "2" (do
          (println "Add new sell transaction")
          (add-new-transaction :sell)
          (recur))

    "c" (do
          (println "Clearing all transactions...")
          (db/with-save!
            (db/set :transactions []))
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
