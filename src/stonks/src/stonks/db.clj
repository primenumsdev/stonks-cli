(ns stonks.db
  (:refer-clojure :exclude [get set update])
  (:require [clojure.java.io :as io]
            [taoensso.nippy :as nippy])
  (:import (java.util Date UUID)))

(def ^:dynamic *DEBUG* false)

(defn debug [& msg]
  (when *DEBUG*
    (apply prn "[DEBUG]" msg)))

;; This is my own DB :)

;; Features:
;; 1. Keep all db state in memory, max size is limited by RAM
;; 2. Persist state to encrypted local file
;; 3. Load state from encrypted local file
;; 4. Query as usual clojure structures with filter, map, reduce

;; TODOs:
;; 1. Backup - create file with date in the db folder
;; 2. Use optimized local state structure - transient, type hints, arrays, .length, aget - clojure perf check
;; 3. Separate file for each collection, master file for DB meta
;; 4. query-memo - to cache queries - user choose when to use memo, no indexes


;; Datalog looks complicated, I just want to save data to disk and query it
;; perhaps I will get back to it
;; https://github.com/juji-io/datalevin

;; Using nippy for serialization, encryption
;; https://github.com/ptaoussanis/nippy

(def ^:private file-db "")
(def ^:private file-pass "")
(def ^:private state (atom {}))

(defn utc-now! [] (Date.))

(defn uuid! [] (str (UUID/randomUUID)))

(defn- create-file-path! [file-path]
  (-> file-path
      io/file
      .getParentFile
      .mkdirs))

(defn load!
  "Load state from file db."
  []
  (debug "loading db...")
  (-> file-db
      (nippy/thaw-from-file {:password [:salted file-pass]})
      (assoc-in [:meta :loaded] (utc-now!))
      (->> (reset! state))))

(defn save!
  "Save current state to file."
  []
  (debug "saving db...")
  (let [s (-> @state
              (assoc-in [:meta :saved] (utc-now!))
              (clojure.core/update :meta #(dissoc % :has-changes?)))]
    (nippy/freeze-to-file file-db s {:password [:salted file-pass]})
    (reset! state s)))

(defn- create-state
  "Create default state for new db."
  [file-path name]
  {:meta {:id           (uuid!)
          :name         name
          :location     file-path
          :created      (utc-now!)
          :saved        nil
          :loaded       nil
          :has-changes? true}})

(defn create!
  [{:keys [file-path name password]} & {:keys [override? discard?]}]
  (if (and (.exists (io/file file-path))
           (not override?))
    (throw (Exception. "db already exists, use override? option, aborting..."))
    (if (and (seq @state)
             (not discard?))
      (throw (Exception. "current db state is not empty, use discard? option, aborting..."))
      (do
        (debug (str "creating db at " file-path "..."))
        (create-file-path! file-path)
        (def file-db file-path)
        (def file-pass password)
        (reset! state (create-state file-path name))
        (save!)
        (load!)))))

(defn connect!
  [{:keys [file-path password]} & {:keys [discard?]}]
  (if (= file-path file-db)
    (debug "already connected")
    (if (and (seq @state)
             (not discard?))
      (throw (Exception. "current db state is not empty, use discard? option, aborting..."))
      (if-not (.exists (io/file file-path))
        (throw (Exception. (str "db not found at " file-path)))
        (do
          (debug (str "connecting to db at " file-path "..."))
          (try
            ;; try load data with given creds
            (with-redefs [file-db   file-path
                          file-pass password]
              (load!))
            ;; save to local state if success
            ;; otherwise it will not be reached due to exception above
            (def file-pass password)
            (def file-db file-path)
            (debug "db loaded successfully")
            (catch Exception ex
              (debug (ex-message ex))
              (debug "db load error, aborting...")
              (throw ex))))))))

(defn set [path val]
  (if (vector? path)
    (swap! state assoc-in path val)
    (swap! state assoc path val))
  (swap! state assoc-in [:meta :has-changes?] true))

(defn update [path cb]
  (if (vector? path)
    (swap! state clojure.core/update-in path cb)
    (swap! state clojure.core/update path cb))
  (swap! state assoc-in [:meta :has-changes?] true))

(defn get [path]
  (if (vector? path)
    (get-in @state path)
    (path @state)))

(defmacro with-save! [& body]
  `(do
     ~@body
     (save!)))

(comment
  @state
  )
