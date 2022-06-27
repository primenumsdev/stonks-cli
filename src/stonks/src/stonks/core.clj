(ns stonks.core
  (:require [clojure.pprint :refer [print-table]]
            [clojure.java.io :as io]
            [stonks.db :as db])
  (:gen-class))

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

(defn app-settings-path []
  (str (user-home-dir) "/.stonks/settings.db"))

(defn settings-exists? []
  (.exists (io/file (app-settings-path))))

(defn create-settings-dir! []
  (-> (app-settings-path)
      io/file
      .getParentFile
      .mkdirs))

(defn initial-setup! []
  (println "Hello, what is your name?")
  (let [username (read-line)]
    (printf "Hi %s!\n" username)
    (println "We are going to setup a new local settings for you.")
    (create-settings-dir!)
    (println "Please specify a password:")
    (let [pass (read-line)]
      (db/create!
        {:file-path (app-settings-path)
         :name      username
         :password  pass}))))

(defn load-settings! []
  (println "Hello, please enter password:")
  (let [pass (read-line)]
    (db/connect!
      {:file-path (app-settings-path)
       :password  pass})))

(defn -main
  "Entry."
  [& args]
  (if-not (settings-exists?)
    (initial-setup!)
    (load-settings!))
  ;(print-table @db/state)
  (read-line))

(comment
  (-main)


  @db/state
  (.exists (io/file (app-settings-path)))
  (create-settings-dir)
  )
